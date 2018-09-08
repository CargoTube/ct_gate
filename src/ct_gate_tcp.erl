%%
%% Copyright (c) 2017-2018 Bas Wegh

-module(ct_gate_tcp).
-include_lib("ct_msg/include/ct_msg.hrl").

-behaviour(gen_server).
-behaviour(ranch_protocol).


%% for ranch_protocol
-export([start_link/4]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).


-record(state, {gate_in = undefined,
                socket = undefined,
                transport = undefined,
		ip = undefined,
		port = undefined,
		serializer = undefined,
		max_length = undefined
               }).

start_link(Ref, Socket, Transport, Opts) ->
     spawn_link_tcp_connection_server(Ref, Socket, Transport, Opts).

init({Ref, Socket, Transport, _Opts = []}) ->
    ok = ranch:accept_ack(Ref),
    {ok, {IP, Port}} = Transport:peername(Socket),
    State = #state{
               socket = Socket,
               transport = Transport,
	       ip = IP,
	       port = Port
              },
    connection_active_once(State),
    gen_server:enter_loop(?MODULE, [], State).

handle_info({tcp, Socket, << 127, MaxLengthExp:4, SerializerNumber:4, 0, 0 >>},
            State=#state{ socket=Socket, gate_in=undefined }) ->
	% handshake
	handle_handshake_message(MaxLengthExp, SerializerNumber, State);	
handle_info({tcp, _Socket, _Data}, State=#state{ gate_in=undefined }) ->
	{stop, normal, State};

handle_info({tcp, Socket, Data},
            State=#state{socket=Socket, gate_in=Pid})
  when byte_size(Data) > 1 ->
    ct_gate_in:handle_raw_data(Data, Pid),
    {noreply, State};
handle_info(connection_once, State) ->
    connection_active_once(State),
    {noreply, State};
handle_info({connection_send, Data}, State) ->
    connection_send(Data, State),
    {noreply, State};
handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};
handle_info({tcp_error, _, Reason}, State) ->
    {stop, Reason, State};
handle_info(_Info, State) ->
    {stop, normal, State}.


handle_call(_, _, State) ->
    {reply, ignored, State}.

handle_cast(_, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    connection_close(State),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

connection_active_once(#state{transport = Transport, socket = Socket}) ->
    Transport:setopts(Socket, [{active, once}]).

connection_send(Data, #state{transport = Transport, socket = Socket}) ->
    Transport:send(Socket, Data).

connection_close(#state{transport=Transport, socket=Socket, gate_in=Pid}) ->
    ct_gate_in:stop(Pid),
    Transport:close(Socket).

spawn_link_tcp_connection_server(Ref, Socket, Transport, Opts) ->
    {ok, proc_lib:spawn_link(?MODULE, init, [{Ref, Socket, Transport, Opts}])}.


handle_handshake_message(MaxLengthExp, SerializerNumber, State) ->
    lager:debug("[~p] handle handshake", [self()]),
    Serializer = translate_serializer_number_to_name(SerializerNumber),
    MaxLength = calculateMaxLength(MaxLengthExp),
    send_handshake_reply(Serializer, State),
    NewState = State#state{ serializer = Serializer, max_length = MaxLength},
    activate_or_close_connection(Serializer, NewState).

activate_or_close_connection(unsupported, State) ->
	{stop, normal, State};
activate_or_close_connection(Serializer, #state{ip=IP, port=Port } = State) ->
    connection_active_once(State),
    {ok, Pid} = ct_gate_in:start_link({tcp, Serializer, IP, Port}),
    {noreply, State#state{gate_in=Pid}}.

translate_serializer_number_to_name(1) -> raw_json;
translate_serializer_number_to_name(2) -> raw_msgpack;
translate_serializer_number_to_name(_) -> unsupported.

calculateMaxLength(MaxLengthExp) ->
    round(math:pow(2,9+MaxLengthExp)).

send_handshake_reply(raw_json, State) ->
    connection_send(use_raw_json_message(), State);
send_handshake_reply(raw_msgpack, State) ->
    connection_send(use_raw_msgpack_message(), State);
send_handshake_reply(_, State) ->
    connection_send(unsupported_serializer_message(), State).


unsupported_serializer_message() ->
    <<127,1:4,0:4,0,0>>.

%% max_length_unacceptable_message() ->
%%     <<127,2:4,0:4,0,0>>.

%% use_reserved_bits_message() ->
%%     <<127,3:4,0:4,0,0>>.

%% max_connections_message() ->
%%     <<127,4:4,0:4,0,0>>.

use_raw_msgpack_message() ->
    Length = max_length(),
    <<127,Length:4,2:4,0,0>>.

use_raw_json_message() ->
    Length = max_length(),
    <<127,Length:4,1:4,0,0>>.


% (x+9) ** 2 is the lengh
% so
%   0 -> 2 ** 9
%   1 -> 2 ** 10 = 1024 etc.
% the number gets shifted 4 bits to the left.
% 15 is the max receive length possible ~ 16M (2 ** 24).
max_length() ->
    application:get_env(ct_gate, tcp_max_length, 15).


