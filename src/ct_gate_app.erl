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

    StartTCP = application:get_env(ct_gate, enable_tcp, false),
    ok = maybe_start_tcp_listener(StartTCP),

    StartWeb = application:get_env(ct_gate, enable_web, false),
    ok = maybe_start_web_listener(StartWeb),

    ct_gate_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

maybe_start_tcp_listener(true) ->
    SSL = tcp_use_ssl(),
    Protocol = tcp_procotol(SSL),
    NumAcceptors = application:get_env(ct_gate, tcp_num_acceptors, 5),
    Options = tcp_options(SSL),
    {ok,_} = ranch:start_listener(ct_gate_tcp, NumAcceptors,
                                  Protocol, Options,
                                  ct_gate_tcp, []),
    ok;
maybe_start_tcp_listener(false) ->
    ok.

tcp_use_ssl() ->
    Cert = application:get_env(ct_gate, tcp_cert_file, undefined),
    Key = application:get_env(ct_gate, tcp_key_file, undefined),
    use_ssl(Cert, Key).

tcp_procotol(true) ->
    ranch_ssl;
tcp_procotol(_) ->
    ranch_tcp.

tcp_options(SSL) ->
    Port = application:get_env(ct_gate, tcp_port, 8080),
    Inet6 = application:get_env(ct_gate, tcp_inet6, true),
    Inet6only = application:get_env(ct_gate, tcp_inet6only, false),

    Cert = application:get_env(ct_gate, tcp_cert_file, undefined),
    Key = application:get_env(ct_gate, tcp_key_file, undefined),
    options([{inet6, Inet6},
             {inet6_only, Inet6only},
             {port, Port},
             {certfile, Cert},
             {keyfile, Key}], [], SSL).




maybe_start_web_listener(true) ->
    SSL = web_use_ssl(),
    Name = ct_gate_web,
    ok = start_tls_or_clear(SSL, Name, web_options(SSL), web_dispatch()),
    NumAcceptors = application:get_env(ct_gate, web_num_acceptors, 5),
    ok = ranch:set_max_connections(Name, NumAcceptors),
    ok;
maybe_start_web_listener(_) ->
    ok.

web_use_ssl() ->
    Cert = application:get_env(ct_gate, web_cert_file, undefined),
    Key = application:get_env(ct_gate, web_key_file, undefined),
    use_ssl(Cert, Key).

start_tls_or_clear(true, Name, Options, Dispatch) ->
    {ok, _} = cowboy:start_tls(Name, Options,
                                 #{ env => #{dispatch => Dispatch}}),
    ok;
start_tls_or_clear(false, Name, Options, Dispatch) ->
    {ok, _} = cowboy:start_clear(Name, Options,
                                 #{ env => #{dispatch => Dispatch}}),
    ok.

web_dispatch() ->
    Path = application:get_env(ct_gate, web_ws_path, "/ct"),
    cowboy_router:compile(
      [
       {'_', [
              {Path, ct_gate_ws, []}
             ]}
      ]).



web_options(SSL) ->
    Port = application:get_env(ct_gate, web_port, 8080),
    Inet6 = application:get_env(ct_gate, web_inet6, true),
    Inet6only = application:get_env(ct_gate, web_inet6only, false),

    Cert = application:get_env(ct_gate, web_cert_file, undefined),
    Key = application:get_env(ct_gate, web_key_file, undefined),
    options([{inet6, Inet6},
             {inet6_only, Inet6only},
             {port, Port},
             {certfile, Cert},
             {keyfile, Key}], [], SSL).


options([{port, _} = Port | T], Options, SSL) ->
    options(T, [ Port | Options ], SSL);
options([{inet6, true} | T], Options, SSL) ->
    options(T, [ inet6 | Options ], SSL);
options([{inet6_only, true} | T], Options, SSL) ->
    options(T, [ {ipv6_v6only, true} | Options ], SSL);
options([{inet6_only, false} | T], Options, SSL) ->
    options(T, [ {ipv6_v6only, false} | Options ], SSL);

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
