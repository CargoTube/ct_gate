-module(ctg_stats).

-export([add/2,
         update/0,
         enabled/0]).


add(Type, Duration) ->
    maybe_add(enabled(), Type, Duration).

maybe_add(true, Type, Duration) ->
    cts_messages:add(Type, Duration/1000.00);
maybe_add(_, _Trype, _Duration) ->
    ok.

enabled() ->
    application:get_env(ct_gate, enable_stats, false).

update() ->
    maybe_update(enabled()).

maybe_update(true) ->
    cts_messages:update();
maybe_update(_) ->
    ok.
