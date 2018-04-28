%%%-------------------------------------------------------------------
%% @doc ct_gate public API
%% @end
%%%-------------------------------------------------------------------
-module(ct_gate_app).
-include_lib("kernel/include/file.hrl").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    StartTCP = application:get_env(ct_gate, tcp_enable, false),
    ok = maybe_start_tcp_listener(StartTCP),

    StartWeb = application:get_env(ct_gate, web_enable, false),
    ok = maybe_start_web_listener(StartWeb),
    ct_gate_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

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
    use_ssl(Cert, Key).

tcp_procotol(true) ->
    ranch_ssl;
tcp_procotol(_) ->
    ranch_tcp.

tcp_options(UseSSL) ->
    Port = application:get_env(ct_gate, tcp_port, 5555),
    Inet6 = application:get_env(ct_gate, tcp_inet6, true),
    Inet6only = application:get_env(ct_gate, tcp_inet6only, false),

    Cert = application:get_env(ct_gate, tcp_cert_file, undefined),
    Key = application:get_env(ct_gate, tcp_key_file, undefined),
    options([{inet6, Inet6},
             {inet6_only, Inet6only},
             {port, Port},
             {certfile, Cert},
             {keyfile, Key}], [], UseSSL).




maybe_start_web_listener(true) ->
    SSL = web_use_ssl(),
    Name = ct_gate_web,
    ok = start_tls_or_clear(SSL, Name, web_options(SSL), web_dispatch()),
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
    cowboy_router:compile(web_routes()).


web_routes() ->
    WsPath = application:get_env(ct_gate, web_ws_path, "/"),
    StaticDir = application:get_env(ct_gate, web_static_dir, undefined),
    StaticPath = application:get_env(ct_gate, web_static_path, undefined),

    PathList = web_path_list([
                              {ws_path, WsPath},
                              {static, StaticPath, StaticDir}
                             ],
                             []),
    lager:debug("web pathlist: ~p", [PathList]),
    [{'_', PathList}].


web_path_list([], List) ->
    List;
web_path_list([{ws_path, Path}|Tail], List) ->
    web_path_list(Tail, [{Path, ct_gate_ws, []} | List]);
web_path_list([{static, Path, Dir}|Tail], List)
  when is_list(Path), is_list(Dir) ->
    BinDir = list_to_binary(Dir),
    BinPath = list_to_binary(Path),
    PatternList = path_to_pattern_list(BinPath, BinDir),
    web_path_list(Tail,  PatternList ++ List);
web_path_list([{static, _Path, _Dir}|Tail], List) ->
    web_path_list(Tail, List).

path_to_pattern_list(Path, Dir) ->
    file:make_dir(Dir),
    {ok, FileList} = file:list_dir(Dir),
    path_to_pattern_list(Path, FileList, Dir, []).

path_to_pattern_list(_Path, [],  _Dir, PatternList) ->
    PatternList;
path_to_pattern_list(Path, [File | Tail],  Dir, PatternList) ->
    FileDir = filename:join(Dir, File),
    {ok, #file_info{type = Type}} = file:read_file_info(FileDir),
    NewPatternList = add_file_to_pattern_list(File, Path, Type, FileDir,
                                              PatternList),
    path_to_pattern_list(Path, Tail, Dir, NewPatternList).

add_file_to_pattern_list(File, Path, regular, FileDir, PatternList) ->
    FilePath = filename:join(Path, File),
    NewPatternList = [{FilePath, cowboy_static, {file, FileDir}} | PatternList],
    maybe_add_path(File, Path, FileDir, NewPatternList) ;
add_file_to_pattern_list(Dir, Path, directory, FileDir, PatternList) ->
    DirPath = filename:join(Path, Dir),
    Pattern = dir_to_pattern(DirPath, binary:last(DirPath)),
    [{Pattern, cowboy_static, {dir, FileDir}} | PatternList];
add_file_to_pattern_list(_File, _Path, _Type, _Dir, PatternList) ->
    PatternList.


maybe_add_path(<<"index.html">>, Path, FileDir, List) ->
    [{Path, cowboy_static, {file, FileDir}} | List];
maybe_add_path(_, _, _, List) ->
    List.


dir_to_pattern(BinDir, $/) ->
    Pattern = <<"[...]">>,
    << BinDir/binary, Pattern/binary>>;
dir_to_pattern(BinDir, _) ->
    Slash = <<"/">>,
    WithSlash = << BinDir/binary, Slash/binary >>,
    dir_to_pattern(WithSlash, $/).


web_options(UseSSL) ->
    Port = application:get_env(ct_gate, web_port, 8080),
    Inet6 = application:get_env(ct_gate, web_inet6, true),
    Inet6only = application:get_env(ct_gate, web_inet6only, false),

    Cert = application:get_env(ct_gate, web_cert_file, undefined),
    Key = application:get_env(ct_gate, web_key_file, undefined),

    NumAcceptors = application:get_env(ct_gate, web_num_acceptors, 5),
    options([{inet6, Inet6},
             {inet6_only, Inet6only},
             {port, Port},
             {certfile, Cert},
             {keyfile, Key},
             {num_acceptors, NumAcceptors}
            ], [], UseSSL).


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
