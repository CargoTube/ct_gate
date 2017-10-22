%%
%% Copyright (c) 2017 Bas Wegh

-module(ct_gate_tcp).

-behaviour(ranch_protocol).
-behaviour(gen_statem).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% for tcp
-export([start_link/4]).
-export([init/4]).

%% gen_server.
-export([init/1]).
-export([callback_mode/0]).
-export([terminate/3]).
-export([code_change/4]).



-record(data,{
          socket,
          transport,
          ok,
          closed,
          error,
          enc = undefined,
          length = infitity,
          buffer = <<"">>,
          %% routing = undefined
          session = undefined
         }).


callback_mode() -> handle_event_function.

% (x+9) ** 2 is the lengh
% so
%   0 -> 2 ** 9
%   1 -> 2 ** 10 = 1024 etc.
% the number gets shifted 4 bits to the left.
% 15 is the max receive length possible ~ 16M (2 ** 24).
% @TODO: make it configurable
-define(MAXLENGTH, 15).

%%% for TCP

start_link(Ref, Socket, Transport, Opts) ->
	start_link_tcp_connection_server(Ref, Socket, Transport, Opts).

init(Ref, Socket, Transport, _Opts = []) ->
	ack_otp_starting(Ref),
	enable_socket_once(Transport, Socket),
	Data = create_initial_data(Transport,Socket),
	gen_statem:enter_loop(?MODULE, [], handshake, Data).


init(_Opts) ->
  erlang:error("don't call").


%% handle_info({Ok, Socket, Data}, #state{ok=Ok, socket=Socket}=State) ->
%% 	handle_incomming_tcp(Data,State);
%% handle_info({Closed, Socket}, #state{closed=Closed, socket=Socket}=State) ->
%% 	handle_socket_closed(State);
%% handle_info({Err,Socket,Error}, #state{error=Err,socket=Socket}=State) ->
%% 	handle_socket_error(Error,State);
%% handle_info({erwa,Msg},State) ->
%% 	handle_message_from_routing(Msg,State);
%% handle_info(Msg,State) ->
%% 	handle_unsupported_info(Msg,State).




ack_otp_starting(Ref) ->
	ok = proc_lib:init_ack({ok, self()}),
	ok = ranch:accept_ack(Ref).

create_initial_data(Transport,Socket) ->
    {Ok,Closed, Error} = Transport:messages(),
    #data{
       socket=Socket,
       transport=Transport,
       ok=Ok,
       closed=Closed,
       error=Error,
       %% routing=erwa_routing:init()
       session = undefined
      }.


%% handle_incomming_tcp(Data,#state{enc=undefined} = State ) ->
%% 	handle_handshake(Data,State);
%% handle_incomming_tcp(Data, State) ->
%% 	handle_incomming_wamp(Data,State).


%% handle_incomming_wamp(Data, #state{transport=Transport, socket=Socket}=State) ->
%% 	enable_socket_once(Transport,Socket),
%% 	deserialize_and_handle_incomming_messages(Data,State).


%% deserialize_and_handle_incomming_messages(Data,State) ->
%% 	{Messages, State} = deserialize_messages_update_state(Data,State),
%% 	handle_incomming_wamp_messages( Messages, State ).

%% deserialize_messages_update_state(Data,#state{buffer=Buffer, enc=Enc} = State) ->
%% 	{Messages, NewBuffer} = wamper_protocol:deserialize(<<Buffer/binary, Data/binary>>,Enc),
%% 	{Messages, State#state{buffer=NewBuffer}}.


%% handle_incomming_wamp_messages([],#state{} = State) ->
%% 	{noreply, State};
%% handle_incomming_wamp_messages([Msg | Tail], State) ->
%% 	Result = handle_message_by_routing(Msg,State),
%% 	handle_result_of_routing(Result,Tail,State).


%% handle_result_of_routing({ok,NewRouting},Tail,State) ->
%% 	handle_incomming_wamp_messages(Tail,State#state{routing=NewRouting});
%% handle_result_of_routing({reply, OutMsg, NewRouting},Tail,State) ->
%% 	serialize_and_send_to_peer(OutMsg,State),
%% 	handle_incomming_wamp_messages(Tail,State#state{routing=NewRouting});
%% handle_result_of_routing({reply_stop,OutMsg, NewRouting}, _Tail, State) ->
%% 	serialize_and_send_to_peer(OutMsg, State),
%% 	close_connection(State#state{routing=NewRouting});
%% handle_result_of_routing({stop,NewRouting},_Tail,State) ->
%% 	close_connection(State#state{routing=NewRouting}).




%% handle_message_by_routing(Msg,#state{routing=Routing}) ->
%% 	erwa_routing:handle_message(Msg,Routing).


%% handle_message_from_routing(Msg,#state{routing=Routing}=State) ->
%% 	Result = handle_routing_message_by_routing(Msg,Routing),
%% 	handle_result_of_routing_for_routing(Result,State).


%% handle_routing_message_by_routing(Msg,Routing) ->
%% 	erwa_routing:handle_info(Msg,Routing).


%% handle_result_of_routing_for_routing({ok, NewRouting},State) ->
%% 	{noreply,State#state{routing=NewRouting}};
%% handle_result_of_routing_for_routing({send, OutMsg, NewRouting},State) ->
%% 	serialize_and_send_to_peer(OutMsg,State),
%% 	{noreply,State#state{routing=NewRouting}};
%% handle_result_of_routing_for_routing({send_stop, OutMsg, NewRouting},State) ->
%% 	serialize_and_send_to_peer(OutMsg,State),
%% 	close_connection( State#state{routing=NewRouting});
%% handle_result_of_routing_for_routing({stop, NewRouting},State) ->
%% 	close_connection(State#state{routing=NewRouting}).


%% handle_handshake(<<127,MaxLengthExp:4,ProtocolNumber:4,0,0>>, State) ->
%% 	handle_handshake_message(MaxLengthExp,ProtocolNumber,State);
%% handle_handshake(<<127,_:4,_:4,_,_>>, State) ->
%% 	handle_bad_handshake(State);
%% handle_handshake(_,State) ->
%% 	handle_misbehaving_client(State).


%% handle_handshake_message(MaxLengthExp, ProtocolNumber, State ) ->
%% 	ProtocolName = translate_protocol_number_to_name(ProtocolNumber),
%% 	MaxLength = calculateMaxLength(MaxLengthExp),
%% 	send_handshake_reply(ProtocolName, State),
%% 	NewState = update_erwa_routing_and_state(ProtocolName,MaxLength,State),
%% 	closeOrActivateConnection(ProtocolName,NewState).


%% handle_bad_handshake(State) ->
%% 	send_to_peer(unsupported_message(),State),
%% 	close_connection(State).

%% handle_misbehaving_client(State) ->
%% 	close_connection(State).

%% translate_protocol_number_to_name(0) -> unsupported;
%% translate_protocol_number_to_name(1) -> raw_json;
%% translate_protocol_number_to_name(2) -> raw_msgpack;
%% translate_protocol_number_to_name(N) ->
%% 	ErlBin = erlbin_protocol_number(),
%% 	if
%% 		N == ErlBin -> raw_erlbin;
%% 		true -> unsupported
%% 	end.


%% calculateMaxLength(MaxLengthExp) ->
%% 	round(math:pow(2,9+MaxLengthExp)).

%% send_handshake_reply(raw_json,State) ->
%% 	send_to_peer(use_raw_json_message(), State);
%% send_handshake_reply(raw_msgpack, State) ->
%% 	send_to_peer(use_raw_msgpack_message(), State);
%% send_handshake_reply(raw_erlbin, State) ->
%% 	send_to_peer(use_raw_erlbin_message(), State);
%% send_handshake_reply(_, State) ->
%% 	send_to_peer(unsupported_message(), State).

%% update_erwa_routing_and_state(unsupported,_,State) ->
%% 	State;
%% update_erwa_routing_and_state(ProtocolName, MaxLength, #state{routing=Routing,
%% 														  transport=Transport
%% 														 } = State) ->
%% 	updateState(ProtocolName,
%% 				MaxLength,
%% 				updateErwaRouting(Transport,
%% 								  get_peername(State),
%% 								  Routing),
%% 				State).


%% updateState(ProtocolName, MaxLength, Routing, State) ->
%% 	State#state{
%% 	  enc = ProtocolName,
%% 	  length = MaxLength,
%% 	  routing = Routing
%% 	 }.

%% updateErwaRouting(_Transport, _Peer, Routing) ->
%% 	%% Routing1 = erwa_routing:set_peer(Peer,Routing),
%% 	%% Routing2 = erwa_routing:set_source(Transport,Routing1),
%% 	Routing.

%% closeOrActivateConnection(unsupported,State) ->
%% 	close_connection(State);
%% closeOrActivateConnection(_, State) ->
%%  	activateConnectionOnce(State).

%% close_connection( State ) ->
%%     erwa_routing:close(),
%% 	{stop, normal, State}.

%% activateConnectionOnce(#state{transport=Transport, socket=Socket} = State) ->
%% 	enable_socket_once(Transport,Socket),
%% 	{noreply,State}.

%% handle_unsupported_info(_Msg,#state{} = State) ->
%% 	% @TODO: should be logged
%% 	{noreply, State}.

enable_socket_once(Transport, Socket) ->
  ok = Transport:setopts(Socket, [{active, once}]).

%% handle_socket_closed(State) ->
%% 	% need to close the routing - maybe
%% 	{stop, normal, State}.

%% handle_socket_error(Error, State) ->
%% 	% need to close the routing - maybe
%% 	{stop, {error, Error}, State }.


%% serialize_and_send_to_peer(Msg,#state{enc=Enc}=State) ->
%% 	OutMsg = wamper_protocol:serialize(Msg,Enc),
%% 	send_to_peer(OutMsg,State).


%% send_to_peer(Msg, #state{transport=Transport, socket=Socket}) ->
%% 	Transport:send(Socket,Msg).


%% get_peername(#state{socket=Socket}) ->
%% 	{ok, Peer} = inet:peername(Socket),
%% 	Peer.


%% unsupported_message() ->
%% 	<<127,0,0,0>>.

%% use_raw_msgpack_message() ->
%% 	<<127,?MAXLENGTH:4,2:4,0,0>>.

%% use_raw_json_message() ->
%% 	<<127,?MAXLENGTH:4,1:4,0,0>>.

%% use_raw_erlbin_message() ->
%% 	S = erlbin_protocol_number(),
%% 	<<127,?MAXLENGTH:4,S:4,0,0>>.


terminate(_Reason, _State, _Data) ->
  ok.

code_change(_OldVsn, State, Data, _Extra) ->
  {ok, State, Data}.


%% erlbin_protocol_number() ->
%%   application:get_env(erwa,erlbin_number,undefined).


start_link_tcp_connection_server(Ref, Socket, Transport, Opts) ->
    proc_lib:start_link(?MODULE, init, [Ref, Socket, Transport, Opts]).
