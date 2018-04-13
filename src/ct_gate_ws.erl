%%
%% Copyright (c) 2015-2018 Bas Wegh
%%

%% @private
-module(ct_gate_ws).
-behaviour(cowboy_websocket).


-export([init/2]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

-define(TIMEOUT,60000).

-define(SUBPROTHEADER,<<"sec-websocket-protocol">>).
-define(WSMSGPACK,<<"wamp.2.msgpack">>).
-define(WSJSON,<<"wamp.2.json">>).
-define(WSMSGPACK_BATCHED,<<"wamp.2.msgpack.batched">>).
-define(WSJSON_BATCHED,<<"wamp.2.json.batched">>).


init( Req, _State) ->
    % need to check for the wamp.2.json or wamp.2.msgpack
    Protocols = cowboy_req:parse_header(?SUBPROTHEADER, Req),
    {FrameTag, Serializer, Header} = find_supported_protocol(Protocols),
    handle_supported_protocol(FrameTag, Serializer, Header, Req).


websocket_handle(InFrame, Data) ->
    ok.


websocket_info(Info, Data) ->
    ok.


terminate(_Reason, _PartialReq, Data) ->
    ct_gate_in:close_connection(Data),
    ok.


handle_supported_protocol(none, _, _, Req) ->
    {shutdown, Req};
handle_supported_protocol(FrameTag, Serializer, Header, Req) ->
    State = ct_gate_in:create_initial_ws_data(FrameTag, Serializer),
    Req1  = cowboy_req:set_resp_header(?SUBPROTHEADER, Header, Req),
    {cowboy_websocket, Req1, State}.


-spec find_supported_protocol([binary()]) -> ProtocolOrError
                                    when ProtocolOrError
                                         :: none |
                                            {json | json_batched | msgpack |
                                             msgpack_batched,
                                             text|binary,
                                             binary()}.
find_supported_protocol([]) ->
    { none, undefined, undefined };
find_supported_protocol([?WSJSON|_T]) ->
    { text, json,?WSJSON };
find_supported_protocol([?WSJSON_BATCHED|_T]) ->
    { text, json_batched, ?WSJSON_BATCHED };
find_supported_protocol([?WSMSGPACK|_T]) ->
    { binary, msgpack, ?WSMSGPACK };
find_supported_protocol([?WSMSGPACK_BATCHED|_T]) ->
    { binary, msgpack_batched, ?WSMSGPACK_BATCHED };
find_supported_protocol([_|T]) ->
    find_supported_protocol(T).