-module(ctg_stats).

-export([add/2,
         update/0,
         enabled/0]).


add(Type, Duration) ->
    maybe_add(enabled(), Type, Duration).

maybe_add(true, Type, Duration) ->
    ct_stats:add_message(Type, Duration/1000.00);
maybe_add(_, _Trype, _Duration) ->
    ok.

enabled() ->
    application:get_env(ct_gate, enable_stats, false).

update() ->
    maybe_update(enabled()).

maybe_update(true) ->
    ct_stats:update();
maybe_update(_) ->
    ok.
