%%
%% Copyright (c) 2018 Bas Wegh
%%

-module(ct_gate_in).
-include_lib("ct_msg/include/ct_msg.hrl").

-behaviour(gen_statem).

%%
-export([create_initial_tcp_data/2]).
-export([create_initial_ws_data/2]).
-export([close_connection/1]).

%% gen_statem.
-export([init/1]).
-export([callback_mode/0]).
-export([handle_event/4]).
-export([terminate/3]).
-export([code_change/4]).


-record(tcp_data, {
          socket,
          transport,
          ok,
          closed,
          error,
          peer = undefined
         }).

-record(ws_data, {
          frametag = undefined,
          state = undefined
         }).

-record(data, {
          con = undefined,
          serializer = undefined,

          max_length = 0,
          buffer = <<"">>,
          message_queue = [],
          session_id = undefined,
          router_if = undefined,
          ping_time = undefined,
          ping_payload = undefined,
          peer = undefined
         }).

%% use the handle even function
callback_mode() -> handle_event_function.


%%% for TCP

init(_Opts) ->
    erlang:error("don't call").


create_initial_tcp_data(Transport,Socket) ->
    {Ok, Closed, Error} = Transport:messages(),
    TcpData = #tcp_data{
                 socket = Socket,
                 transport = Transport,
                 peer = get_peername(Socket),
                 ok = Ok,
                 closed = Closed,
                 error = Error
                },
    create_initial_data(TcpData, undefined).

create_initial_ws_data(FrameTag, Serializer) ->
    WsData = #ws_data{
       frametag = FrameTag
      },
    create_initial_data(WsData, Serializer).


create_initial_data(ConData, Serializer) ->
    #data{
       con = ConData,
       serializer = Serializer,
       session_id = undefined,
       router_if = application:get_env(ct_gate, router_if, ct_router_if_off)
      }.


handle_event(info, next_message, State,
             #data{message_queue = [ Message | Tail ]} = Data) ->
    trigger_next_message(),
    Type = ct_msg:get_type(Message),
    NewData = Data#data{message_queue = Tail},
    lager:debug("[~p] --> ~p", [self(), Message]),
    handle_incoming_wamp_message(State, Type, Message, NewData);
handle_event(info, next_message, State, #data{message_queue = []} = Data) ->
    activate_connection_once(Data),
    {next_state, State, Data};
handle_event(info,
             {tcp, Socket,
              <<127, MaxLengthExp:4, SerializerNumber:4, 0, 0>> = TcpData},
             handshake,
             #data{con = #tcp_data{socket = Socket}} = Data) ->
    lager:debug("[~p] >>> ~p", [self(), TcpData]),
    handle_tcp_handshake_message(MaxLengthExp, SerializerNumber, Data );
handle_event(info,
             {tcp, S, TcpData},
             State,
             #data{con = #tcp_data{socket = S}} = Data) ->
    lager:debug("[~p] >>> ~p",[self(), TcpData]),
    handle_tcp_data(TcpData, State, Data);
handle_event(info,
             {tcp_closed, Socket},
             _,
             #data{con = #tcp_data{ socket = Socket }} = Data) ->
    close_connection(Data);
handle_event(info,
             {to_peer, Message},
             State,
             Data) ->
    Type = ct_msg:get_type(Message),
    handle_outgoing_wamp_message(State, Type, Message, Data);
handle_event(Event, Content, State, Data) ->
    lager:debug("[~p] ignore event [~p] ~p ~p",[self(), State, Event, Content]),
    {next_state, State, Data}.


handle_tcp_handshake_message(MaxLengthExp, SerializerNumber, Data) ->
    lager:debug("[~p] handle handshake", [self()]),
    Serializer = translate_serializer_number_to_name(SerializerNumber),
    MaxLength = calculateMaxLength(MaxLengthExp),
    send_handshake_reply(Serializer, Data),
    NewData = Data#data{ serializer = Serializer, max_length = MaxLength},
    activate_or_close_connection(Serializer, NewData).


handle_tcp_data(TcpData, State, Data) ->
    {Messages, NewData} = deserialize_messages_update_data(TcpData, Data),
    set_wamp_message_queue(Messages, State, NewData).


set_wamp_message_queue([], State, Data) ->
    activate_connection_once(Data),
    {next_state, State, Data};
set_wamp_message_queue(Messages, State, Data) ->
    trigger_next_message(),
    {next_state, State, Data#data{message_queue = Messages}}.


handle_incoming_wamp_message(expect_hello, hello, Hello, Data) ->
    router_handle_hello(Hello, Data),
    {next_state, welcome_or_challenge, Data};
handle_incoming_wamp_message(expect_hello, _Type, _Message, Data) ->
    serialize_and_send_to_peer(?ABORT(#{}, canceled), Data),
    {next_state, handshake, reset_data_close_session(Data)};
handle_incoming_wamp_message(established, goodbye, _Message, Data) ->
    serialize_and_send_to_peer(?GOODBYE(#{}, goodbye_and_out), Data),
    {next_state, handshake, reset_data_close_session(Data)};
handle_incoming_wamp_message(expect_goodbye, goodbye, _Message, Data) ->
    {next_state, handshake, reset_data_close_session(Data)};
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
    %% TODO: calculate ping time
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
handle_outgoing_wamp_message(State, abort, Abort, Data)
  when State == welcome_or_challenge; State == welcome ->
    serialize_and_send_to_peer(Abort, Data),
    {next_state, handshake, reset_data_close_session(Data)};
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



set_session_id(Welcome, Data) ->
    {ok, SessionId} = ct_msg:extract_session(Welcome),
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
      con = undefined,
      session_id = undefined
     }.

activate_or_close_connection(unsupported, Data) ->
    close_connection(Data);
activate_or_close_connection(_, Data) ->
    ok = activate_connection_once(Data),
    {next_state, expect_hello, Data}.

close_connection( #data{ con = #tcp_data{ socket = Socket} } = Data) ->
    lager:debug("[~p] connection closing", [self()]),
    ok = router_handle_session_closed(Data),
    ok = gen_tcp:close(Socket),
    {stop, normal, Data}.

activate_connection_once(#data{ con = #tcp_data{transport=Transport,
                                                socket=Socket} }) ->
    ok = Transport:setopts(Socket, [{active, once}]).


serialize_and_send_to_peer(Msg,#data{serializer=Serializer}=Data) ->
    lager:debug("[~p] <-- ~p",[self(), Msg]),
    OutMsg = ct_msg:serialize(Msg, Serializer),
    send_to_peer(OutMsg, Data).


send_to_peer(Msg, #data{ con = #tcp_data{transport=Transport,
                                         socket=Socket} }) ->
    lager:debug("[~p] <<< ~p",[self(), Msg]),
    Transport:send(Socket,Msg).


get_peername(Socket) ->
    {ok, Peer} = inet:peername(Socket),
    Peer.


terminate(_Reason, _State, Data) ->
    close_connection(Data),
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.



%% generic router handling

router_handle_hello(Hello, #data{router_if = RouterIf}) ->
    ct_router_if:handle_hello(Hello, RouterIf).

router_handle_established_message(Message, #data{router_if = RouterIf,
                                                 session_id = SessionId}) ->
    ct_router_if:handle_established(Message, SessionId, RouterIf).

router_handle_session_closed(#data{session_id = undefined}) ->
    ok;
router_handle_session_closed(#data{router_if = RouterIf,
                                   session_id = SessionId}) ->
    ct_router_if:handle_session_closed(SessionId, RouterIf).