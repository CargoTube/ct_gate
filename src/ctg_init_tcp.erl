-module(ctg_init_tcp).

-export([start/0]).

start() ->
    StartTCP = application:get_env(ct_gate, tcp_enable, false),
    ok = maybe_start_tcp_listener(StartTCP),
    ok.

maybe_start_tcp_listener(true) ->
    UseSSL = tcp_use_ssl(),
    Protocol = tcp_procotol(UseSSL),
    NumAcceptors = application:get_env(ct_gate, tcp_num_acceptors, 5),
    Options = tcp_options(UseSSL),
    {ok, _} = ranch:start_listener(ct_gate_tcp, NumAcceptors,
                                   Protocol, Options,
                                   ct_gate_tcp, []),
    ok;
maybe_start_tcp_listener(false) ->
    ok.

tcp_use_ssl() ->
    Cert = application:get_env(ct_gate, tcp_cert_file, undefined),
    Key = application:get_env(ct_gate, tcp_key_file, undefined),
    ctg_con_utils:use_ssl(Cert, Key).

tcp_procotol(true) ->
    ranch_ssl;
tcp_procotol(_) ->
    ranch_tcp.

tcp_options(UseSSL) ->
    Port = application:get_env(ct_gate, tcp_port, 5555),
    Inet6 = application:get_env(ct_gate, tcp_inet6, false),
    Inet6only = application:get_env(ct_gate, tcp_inet6only, false),

    Cert = application:get_env(ct_gate, tcp_cert_file, undefined),
    Key = application:get_env(ct_gate, tcp_key_file, undefined),
    ctg_con_utils:options([{inet6, Inet6},
                           {inet6_only, Inet6only},
                           {port, Port},
                           {certfile, Cert},
                           {keyfile, Key}], [], UseSSL).
