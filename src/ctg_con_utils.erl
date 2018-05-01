-module(ctg_con_utils).

-export([
         start_tls_or_clear/4,
         options/3,
         use_ssl/2
        ]).

start_tls_or_clear(true, Name, Options, Dispatch) ->
    {ok, _} = cowboy:start_tls(Name, Options,
                                 #{ env => #{dispatch => Dispatch}}),
    ok;
start_tls_or_clear(false, Name, Options, Dispatch) ->
    {ok, _} = cowboy:start_clear(Name, Options,
                                 #{ env => #{dispatch => Dispatch}}),
    ok.




options([], Options, _SSL) ->
    Options;
options([{port, _} = Port | T], Options, SSL) ->
    options(T, [ Port | Options ], SSL);
options([{inet6, true} | T], Options, SSL) ->
    options(T, [ inet6 | Options ], SSL);
options([{inet6_only, true} | T], Options, SSL) ->
    options(T, [ {ipv6_v6only, true} | Options ], SSL);
options([{inet6_only, false} | T], Options, SSL) ->
    options(T, [ {ipv6_v6only, false} | Options ], SSL);
options([{num_acceptors, _} = Opt | T], Options, SSL) ->
    options(T, [ Opt | Options ], SSL);

%% SSL options
options([ {certfile, undefined} | T ], Options, true) ->
    options(T,  Options, true);
options([ {certfile, _} = Cert | T ], Options, true) ->
    options(T, [Cert |  Options], true);
options([ {keyfile, undefined} | T ], Options, true) ->
    options(T,  Options, true);
options([ {keyfile, _} = Key | T ], Options, true) ->
    options(T, [Key |  Options], true);

options([ _ | T ], Options, SSL) ->
    options(T, Options, SSL).


use_ssl(Cert, Key) ->
    filelib:is_regular(Cert) and filelib:is_regular(Key).
