%%%-------------------------------------------------------------------
%% @doc ct_gate public API
%% @end
%%%-------------------------------------------------------------------
-module(ct_gate_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    ok = ctg_init_web:start(),
    ok = ctg_init_tcp:start(),

    ct_gate_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.
