-module(ctg_con_utils).

-export([
         options/3,
         use_ssl/2
        ]).


options([], Options, _SSL) ->
    clean_options(Options);
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

clean_options(Options) ->
    IPv6 = lists:member(inet6, Options),
    maybe_remove_ipv6(IPv6, Options).

maybe_remove_ipv6(false, Options) ->
    Keys = [inet6, inet6_only],
    Delete = fun(Key, Opts) ->
                     lists:keydelete(Key, 1, Opts)
             end,
    lists:foldl(Delete, Options, Keys);
maybe_remove_ipv6(_, Options) ->
    Options.



use_ssl(Cert, Key) ->
    filelib:is_regular(Cert) and filelib:is_regular(Key).
