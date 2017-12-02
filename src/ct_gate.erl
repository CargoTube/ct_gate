-module(ct_gate).
-export([
         start_debug/1,
         start_debug/2,
         stop_debug/0
        ]).

start_debug(ListOfModules) ->
    %debug for an hour or 10000 messages
    Options = [{time, 3600000}, {msgs, 10000}],
    start_debug(ListOfModules, Options).

-spec start_debug([string()], [any()]) -> {ok, any()}.
start_debug(ListOfModules, Options) ->
    redbug:start(ListOfModules, Options).


-spec stop_debug() -> stopped | not_started.
stop_debug() ->
redbug:stop().
