%%
%% Copyright (c) 2017 Bas Wegh

-module(ct_gate_tcp).
-include_lib("ct_msg/include/ct_msg.hrl").

-behaviour(gen_server).
-behaviour(ranch_protocol).


%% for tcp
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
                transport = undefined
               }).

start_link(Ref, Socket, Transport, Opts) ->
     start_link_tcp_connection_server(Ref, Socket, Transport, Opts).

init({Ref, Socket, Transport, _Opts = []}) ->
    lager:debug("init/1"),
    {ok, Pid} = ct_gate_in:start_link(tcp),
    lager:debug("got gate_in"),
    State = #state{
               gate_in = Pid,
               socket = Socket,
               transport = Transport
              },
    ok = ranch:accept_ack(Ref),
    connection_active_once(State),
    lager:debug("enter loop"),
    gen_server:enter_loop(?MODULE, [], State).

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

connection_close(#state{transport = Transport, socket = Socket}) ->
    Transport:close(Socket).

start_link_tcp_connection_server(Ref, Socket, Transport, Opts) ->
    {ok, proc_lib:start_link(?MODULE, init, [{Ref, Socket, Transport, Opts}])}.
