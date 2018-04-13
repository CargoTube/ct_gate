%%
%% Copyright (c) 2017 Bas Wegh

-module(ct_gate_tcp).
-include_lib("ct_msg/include/ct_msg.hrl").

-behaviour(ranch_protocol).
-behaviour(gen_statem).


%% for tcp
-export([start_link/4]).
-export([init/4]).


%% gen_statem.
-export([init/1]).
-export([callback_mode/0]).
-export([handle_event/4]).
-export([terminate/3]).
-export([code_change/4]).



%% use the handle event function
callback_mode() -> handle_event_function.


%%% for TCP
start_link(Ref, Socket, Transport, Opts) ->
    start_link_tcp_connection_server(Ref, Socket, Transport, Opts).

init(Ref, Socket, Transport, _Opts = []) ->
    lager:debug("[~p] init tcp", [self()]),
    ack_otp_starting(Ref),
    Data = ct_gate_in:create_initial_tcp_data(Transport, Socket),
    gen_statem:enter_loop(?MODULE, [], handshake, Data).


init(_Opts) ->
    erlang:error("don't call").


ack_otp_starting(Ref) ->
    ok = proc_lib:init_ack({ok, self()}),
    ok = ranch:accept_ack(Ref).

handle_event(Type, Message, State, Data) ->
    ct_gate_in:handle_event(Type, Message, State, Data).


terminate(_Reason, _State, Data) ->
    ct_gate_in:close_connection(Data),
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.


start_link_tcp_connection_server(Ref, Socket, Transport, Opts) ->
    proc_lib:start_link(?MODULE, init, [Ref, Socket, Transport, Opts]).
