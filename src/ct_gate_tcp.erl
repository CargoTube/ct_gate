%%
%% Copyright (c) 2017 Bas Wegh

-module(ct_gate_tcp).

-behaviour(ranch_protocol).
-behaviour(gen_statem).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include_lib("ct_msg/include/ct_msg.hrl").

%% for tcp
-export([start_link/4]).
-export([init/4]).

%% gen_statem.
-export([init/1]).
-export([callback_mode/0]).
-export([handle_event/4]).
-export([terminate/3]).
-export([code_change/4]).



-record(data, {
          socket,
          transport,
          ok,
          closed,
          error,
          peer = undefined,

          serializer = undefined,
          max_length = 0,
          buffer = <<"">>,
          message_queue = [],
          ping_payload = undefined,
          ping_time = undefined,
          session = undefined,
          router_if = undefined
         }).

%% use the handle even function
callback_mode() -> handle_event_function.


%%% for TCP

start_link(Ref, Socket, Transport, Opts) ->
    start_link_tcp_connection_server(Ref, Socket, Transport, Opts).

init(Ref, Socket, Transport, _Opts = []) ->
    lager:debug("init tcp"),
    ack_otp_starting(Ref),
    Data = create_initial_data(Transport, Socket),
    activate_connection_once(Data),
    gen_statem:enter_loop(?MODULE, [], handshake, Data).


init(_Opts) ->
    erlang:error("don't call").


ack_otp_starting(Ref) ->
    ok = proc_lib:init_ack({ok, self()}),
    ok = ranch:accept_ack(Ref).

create_initial_data(Transport,Socket) ->
    {Ok, Closed, Error} = Transport:messages(),
    #data{
       socket=Socket,
       transport=Transport,
       ok=Ok,
       closed=Closed,
       error=Error,
       session = undefined,
       router_if = application:get_env(ct_gate, router_if, ct_router_if_off),
       peer = get_peername(Socket)
      }.


handle_event(info, next_message, State, #data{
                                           message_queue = [ Message | Tail ]
                                             } = Data) ->
    trigger_next_message(),
    Type = ct_msg:get_type(Message),
    NewData = Data#data{message_queue = Tail},
    handle_incoming_wamp_message(State, Type, Message, NewData);
handle_event(info, next_message, State, #data{message_queue = []} = Data) ->
    activate_connection_once(Data),
    {next_state, State, Data};
handle_event(info, {tcp, Socket,
                    <<127, MaxLengthExp:4, SerializerNumber:4, 0, 0>>
                        = TcpData}, handshake, #data{socket = Socket} = Data) ->
    lager:debug(">>> ~p", [TcpData]),
    handle_handshake_message(MaxLengthExp, SerializerNumber, Data );

handle_event(info, {tcp, S, TcpData}, State, #data{socket = S} = Data) ->
    lager:debug(">>> ~p",[TcpData]),
    handle_tcp_data(TcpData, State, Data);
handle_event(info, {tcp_closed, Socket}, _, #data{socket = Socket} = Data) ->
    close_connection(Data);
handle_event(info, {to_peer, Message}, State, Data) ->
    Type = ct_msg:get_type(Message),
    handle_outgoing_wamp_message(State, Type, Message, Data);
handle_event(Event, Content, State, Data) ->
    lager:debug("ignoring event [~p] ~p ~p",[State, Event, Content]),
    {next_state, State, Data}.


handle_handshake_message(MaxLengthExp, SerializerNumber, Data) ->
    lager:debug("handle handshake"),
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
handle_incoming_wamp_message(established, Type, Message, Data)
  when Type == error; Type == publish; Type == subscribe;
       Type == unsubscribe; Type == call; Type == register; Type == unregister;
       Type == yield->
    %% TODO: check what type the peer is and allow only messages for that type e.g. caller
    router_handle_established_message(Message, Data),
    {next_state, established, Data};
handle_incoming_wamp_message(State, ping, {ping, Payload}, Data) ->
    send_to_peer(ct_msg:pong(Payload), Data),
    {next_state, State, Data};
handle_incoming_wamp_message(State, pong, {pong, Payload},
                             #data{ping_payload = Payload} = Data) ->
    %% TODO: calculate ping time
    {next_state, State, Data};
handle_incoming_wamp_message(State, pong, _, Data) ->
    %% bad/old pong reply - just ignore it
    {next_state, State, Data};
handle_incoming_wamp_message(_, _, _, Data) ->
    lager:debug("bad message, goodbye and kill connection"),
    serialize_and_send_to_peer(?GOODBYE(#{}, canceled), Data),
    close_connection(Data).



handle_outgoing_wamp_message(State, welcome, Welcome, Data)
  when State == welcome_or_challenge; State == welcome ->
    serialize_and_send_to_peer(Welcome, Data),
    {next_state, established, Data};
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
handle_outgoing_wamp_message(State, _Type, Message, Data) ->
    lager:warning("throwing away unsupported out message [~p] ~p",[State, Message]),
    {next_state, established, Data}.



%% handle_router_messages(State, {}V


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
    Data#data{serializer = undefined,
              max_length = 0,
              buffer = <<"">>,
              ping_payload = undefined,
              ping_time = undefined,
              session = undefined
             }.

activate_or_close_connection(unsupported, Data) ->
    close_connection(Data);
activate_or_close_connection(_, Data) ->
    ok = activate_connection_once(Data),
    {next_state, expect_hello, Data}.

close_connection( #data{socket = Socket} = Data) ->
    ok = router_handle_session_closed(Data),
    ok = gen_tcp:close(Socket),
    {stop, normal, Data}.

activate_connection_once(#data{transport=Transport, socket=Socket}) ->
    ok = Transport:setopts(Socket, [{active, once}]).


%% handle_socket_error(Error, State) ->
%%     % need to close the routing - maybe
%%     {stop, {error, Error}, State }.


serialize_and_send_to_peer(Msg,#data{serializer=Serializer}=Data) ->
    OutMsg = ct_msg:serialize(Msg, Serializer),
    send_to_peer(OutMsg, Data).


send_to_peer(Msg, #data{transport=Transport, socket=Socket}) ->
    lager:debug("<<< ~p",[Msg]),
    Transport:send(Socket,Msg).


get_peername(Socket) ->
    {ok, Peer} = inet:peername(Socket),
    Peer.


terminate(_Reason, _State, _Data) ->
    lager:debug("connection closing"),
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.


start_link_tcp_connection_server(Ref, Socket, Transport, Opts) ->
    proc_lib:start_link(?MODULE, init, [Ref, Socket, Transport, Opts]).


router_handle_hello(Hello, #data{router_if = RouterIf}) ->
    ct_router_if:handle_hello(Hello, RouterIf).

router_handle_established_message(Message, #data{router_if = RouterIf,
                                                 session = Session}) ->
    ct_router_if:handle_established(Message, Session, RouterIf).

router_handle_session_closed(#data{session = undefined}) ->
    ok;
router_handle_session_closed(#data{router_if = RouterIf,
                                   session = Session}) ->
    ct_router_if:handle_session_closed(Session, RouterIf).
