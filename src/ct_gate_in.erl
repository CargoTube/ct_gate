%%
%% Copyright (c) 2018 Bas Wegh
%%

-module(ct_gate_in).
-include_lib("ct_msg/include/ct_msg.hrl").
-behaviour(gen_statem).


%%
-export([start_link/1]).
-export([handle_raw_data/2]).
-export([close_session/1]).
-export([stop/1]).


%% gen_statem.
-export([init/1]).
-export([callback_mode/0]).
-export([handle_event/4]).
-export([terminate/3]).
-export([code_change/4]).


-record(data, {
          def_state = expect_hello,
          peer_pid = undefined,
          serializer = undefined,
          max_length = 0,
          buffer = <<"">>,
          message_queue = [],
          session_id = undefined,
          router_if = undefined,
          ping_time = undefined,
          ping_payload = undefined,
          transport_type = undefined,
          peer_ip = undefined,
          peer_port = undefined
         }).


start_link(Type) ->
    Data = create_initial_data(Type),
    gen_statem:start_link(?MODULE, Data, []).

create_initial_data({tcp, IP, Port}) ->
    create_initial_data(undefined, handshake, rawsocket, IP, Port);
create_initial_data({ws, Serializer, IP, Port}) ->
    create_initial_data(Serializer, expect_hello, websocket, IP, Port).

create_initial_data(Serializer, DefState, TransportType, IP, Port) ->
    #data{
       def_state = DefState,
       peer_pid = self(),
       serializer = Serializer,
       transport_type = TransportType,
       session_id = undefined,
       peer_ip = IP,
       peer_port = Port,
       router_if = application:get_env(ct_gate, router_if, ct_router_if_off)
      }.

handle_raw_data(Data, Pid) ->
    gen_statem:cast(Pid, {raw_data, Data}).

close_session(Pid) ->
    gen_statem:call(Pid, session_close).

stop(Pid) ->
    gen_statem:stop(Pid).

%% use the handle event function
callback_mode() -> handle_event_function.

init(#data{def_state = State} = Data) ->
    {ok, State, Data}.


handle_event(cast, {raw_data, InData}, State, Data) ->
    lager:debug("[~p] >>> ~p",[self(), InData]),
    handle_raw_data(InData, State, Data);
handle_event({call, {Peer, _} = From},
             session_close, State, #data{peer_pid = Peer} = Data) ->
    {next_state, State, reset_data_close_session(Data), [{reply, From, ok}]};

handle_event(info, next_message, State,
             #data{message_queue = [ Message | Tail ]} = Data) ->
    trigger_next_message(),
    Type = ct_msg:get_type(Message),
    NewData = Data#data{message_queue = Tail},
    lager:debug("[~p] --> ~p", [self(), Message]),
    {Time, Result} = timer:tc(fun handle_incoming_wamp_message/4, [State, Type,
                                                                   Message,
                                                                   NewData]),
    ctg_stats:add(Type, Time),
    Result;
handle_event(info, next_message, State, #data{message_queue = []} = Data) ->
    activate_connection_once(Data),
    {next_state, State, Data};
handle_event(info,
             {to_peer, Message},
             State,
             Data) ->
    Type = ct_msg:get_type(Message),
    handle_outgoing_wamp_message(State, Type, Message, Data);
handle_event(Event, Content, State, Data) ->
    lager:debug("[~p] ignore event [~p] ~p ~p",[self(), State, Event, Content]),
    {next_state, State, Data}.




handle_raw_data(<< 127, MaxLengthExp:4, SerializerNumber:4, 0, 0>>,
                handshake, Data) ->
    handle_tcp_handshake_message(MaxLengthExp, SerializerNumber, Data);
handle_raw_data(RawData, State, Data) ->
    {Messages, NewData} = deserialize_messages_update_data(RawData, Data),
    set_wamp_message_queue(Messages, State, NewData).

handle_tcp_handshake_message(MaxLengthExp, SerializerNumber, Data) ->
    lager:debug("[~p] handle handshake", [self()]),
    Serializer = translate_serializer_number_to_name(SerializerNumber),
    MaxLength = calculateMaxLength(MaxLengthExp),
    send_handshake_reply(Serializer, Data),
    NewData = Data#data{ serializer = Serializer, max_length = MaxLength},
    activate_or_close_connection(Serializer, NewData).

set_wamp_message_queue([], State, Data) ->
    activate_connection_once(Data),
    {next_state, State, Data};
set_wamp_message_queue(Messages, State, Data) ->
    trigger_next_message(),
    {next_state, State, Data#data{message_queue = Messages}}.


handle_incoming_wamp_message(expect_hello, hello, Hello, Data) ->
    router_handle_hello(Hello, Data),
    {next_state, welcome_or_challenge, Data};
handle_incoming_wamp_message(expect_hello, _Type, _Message,
                             #data{ def_state = DefState} =  Data) ->
    serialize_and_send_to_peer(?ABORT(#{}, canceled), Data),
    {next_state, DefState, reset_data_close_session(Data)};
handle_incoming_wamp_message(established, goodbye, _Message,
                             #data{ def_state = DefState} =  Data) ->
    serialize_and_send_to_peer(?GOODBYE(#{}, goodbye_and_out), Data),
    {next_state, DefState, reset_data_close_session(Data)};
handle_incoming_wamp_message(expect_goodbye, goodbye, _Message,
                             #data{ def_state = DefState} =  Data) ->
    {next_state, DefState, reset_data_close_session(Data)};
handle_incoming_wamp_message(established, Type, Message, Data)
  when Type == error; Type == publish; Type == subscribe;
       Type == unsubscribe; Type == call; Type == register; Type == unregister;
       Type == yield->
    %% TODO: check what type the peer is and allow only messages for that
    %% type e.g. caller
    router_handle_established_message(Message, Data),
    {next_state, established, Data};
handle_incoming_wamp_message(State, ping, {ping, Payload}, Data) ->
    send_to_peer(ct_msg:pong(Payload), Data),
    {next_state, State, Data};
handle_incoming_wamp_message(State, pong, {pong, Payload},
                             #data{ ping_payload = Payload } = Data) ->
    {next_state, State, Data};
handle_incoming_wamp_message(State, pong, _, Data) ->
    %% bad/old pong reply - just ignore it
    {next_state, State, Data};
handle_incoming_wamp_message(expect_goodbye, Message, _, Data) ->
    lager:debug("[~p] bad message ~p; kill connection", [self(), Message]),
    close_connection(Data);
handle_incoming_wamp_message(_, Message, _, Data) ->
    lager:debug("[~p] bad message ~p; goodbye and kill connection",
                [Message, self()]),
    serialize_and_send_to_peer(?GOODBYE(#{}, canceled), Data),
    close_connection(Data).



handle_outgoing_wamp_message(State, welcome, Welcome, Data)
  when State == welcome_or_challenge; State == welcome ->
    UpdatedData = set_session_id(Welcome, Data),
    serialize_and_send_to_peer(Welcome, UpdatedData),
    {next_state, established, UpdatedData};
handle_outgoing_wamp_message(State, abort, Abort,
                             #data{ def_state = DefState} =  Data)
  when State == welcome_or_challenge; State == welcome ->
    serialize_and_send_to_peer(Abort, Data),
    {next_state, DefState, reset_data_close_session(Data)};
handle_outgoing_wamp_message(welcome_or_challenge, challenge, Clg, Data) ->
    serialize_and_send_to_peer(Clg, Data),
    {next_state, expect_authenticate, Data};
handle_outgoing_wamp_message(established, Type, Message, Data)
  when Type == error; Type == published; Type == subscribed;
       Type == unsubscribed; Type == event; Type == result; Type == registered;
       Type == unregistered; Type == invocation ;Type == interrupt ->
    serialize_and_send_to_peer(Message, Data),
    {next_state, established, Data};
handle_outgoing_wamp_message(established, goodbye, Message, Data) ->
    serialize_and_send_to_peer(Message, Data),
    {next_state, expect_goodbye, Data};
handle_outgoing_wamp_message(State, _Type, Message, Data) ->
    lager:warning("[~p] throw away bad out message [~p] ~p",[self(), State,
                                                             Message]),
    {next_state, established, Data}.



set_session_id(Welcome, #data{peer_ip = IP, peer_port = Port } = Data) ->
    {ok, SessionId} = ct_msg:extract_session(Welcome),
    lager:debug("[~p] SessionId: ~p", [self(), SessionId]),
    lager:debug("[~p] Peer: ~p ~p", [self(), IP, Port]),
    Data#data{session_id = SessionId}.



translate_serializer_number_to_name(1) -> raw_json;
translate_serializer_number_to_name(2) -> raw_msgpack;
translate_serializer_number_to_name(_) -> unsupported.

calculateMaxLength(MaxLengthExp) ->
    round(math:pow(2,9+MaxLengthExp)).

send_handshake_reply(raw_json, Data) ->
    send_to_peer(use_raw_json_message(), Data);
send_handshake_reply(raw_msgpack, Data) ->
    send_to_peer(use_raw_msgpack_message(), Data);
send_handshake_reply(_, Data) ->
    send_to_peer(unsupported_serializer_message(), Data).


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


trigger_next_message() ->
    self() ! next_message.

% (x+9) ** 2 is the lengh
% so
%   0 -> 2 ** 9
%   1 -> 2 ** 10 = 1024 etc.
% the number gets shifted 4 bits to the left.
% 15 is the max receive length possible ~ 16M (2 ** 24).
max_length() ->
    application:get_env(ct_gate, tcp_max_length, 15).



deserialize_messages_update_data(TcpData, #data{buffer=OldBuffer,
                                                serializer=Ser} = Data) ->
    Buffer = <<OldBuffer/binary, TcpData/binary>>,
    {Messages, NewBuffer} = ct_msg:deserialize(Buffer, Ser),
    {Messages, Data#data{buffer=NewBuffer}}.


reset_data_close_session(Data) ->
    ok = router_handle_session_closed(Data),
    Data#data{
      max_length = 0,
      buffer = <<"">>,
      session_id = undefined
     }.

close_connection( Data ) ->
    ok = router_handle_session_closed(Data),
    {stop, normal, Data}.

activate_or_close_connection(unsupported, Data) ->
    close_connection(Data);
activate_or_close_connection(_, Data) ->
    activate_connection_once(Data),
    {next_state, expect_hello, Data}.


activate_connection_once(#data{ peer_pid = Peer }) ->
    Peer ! connection_once.

serialize_and_send_to_peer(Msg,#data{serializer=Serializer}=Data) ->
    lager:debug("[~p] <-- ~p",[self(), Msg]),
    OutMsg = ct_msg:serialize(Msg, Serializer),
    send_to_peer(OutMsg, Data).


send_to_peer(Msg, #data{ peer_pid = Peer }) ->
    lager:debug("[~p] <<< ~p",[self(), Msg]),
    Peer ! {connection_send, Msg}.


terminate(_Reason, _State, Data) ->
    lager:debug("[~p] terminate",[self()]),
    ok = router_handle_session_closed(Data),
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.



%% generic router handling

router_handle_hello(Hello, #data{router_if = RouterIf, peer_ip = PeerIp,
                                 transport_type = TransType,
                                 peer_port = PeerPort,
                                 serializer = Serializer}) ->
    Transport = #{peer_ip => convert_ip(PeerIp), peer_port => PeerPort,
                  type => TransType, serializer => Serializer },
    ct_router_if:handle_hello(Hello, RouterIf, Transport).

router_handle_established_message(Message, #data{router_if = RouterIf,
                                                 session_id = SessionId}) ->
    ct_router_if:handle_established(Message, SessionId, RouterIf).

router_handle_session_closed(#data{session_id = undefined}) ->
    ok;
router_handle_session_closed(#data{router_if = RouterIf,
                                   session_id = SessionId}) ->
    ct_router_if:handle_session_closed(SessionId, RouterIf).

convert_ip({A,B,C,D}) ->
    list_to_binary(io_lib:format("~p.~p.~p.~p",[A, B, C, D])).
