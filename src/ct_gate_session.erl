-module(ct_gate_session).

-export([new/0]).

-export_type([session/0]).

-record(session, {}).
-type session() :: #session{}.


-spec new() -> session().
new() ->
    #session{}.
