-module(ct_gate_init).

-behaviour(gen_server).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {}).

init(noparams) ->
    {ok, #state{}}.


handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call(_Msg, _From, State) ->
    {reply, ignored, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(normal, _State) ->
    ok;
terminate(Reason, _State) ->
    lager:error("Gate init: terminated with reason ~p", [Reason]).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


maybe_start_tcp_listener(true) ->
    Port = application:get_env(ct_gate, tcp_port, 5555),
    {ok,_} = ranch:start_listener(ct_gate_tcp, 5,
                                  ranch_tcp, [{port, Port}],
                                  ct_gate_tcp, []),
    ok;
maybe_start_tcp_listener(false) ->
    ok.
