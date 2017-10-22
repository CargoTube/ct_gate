%%
%% Copyright (c) 2015-2017 Bas Wegh
%%

%% @private
-module(ct_gate_ws).
-behaviour(cowboy_websocket).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


-export([init/2]).
-export([websocket_handle/2]).
-export([websocket_info/3]).
-export([terminate/3]).

-define(TIMEOUT,60000).

-define(SUBPROTHEADER,<<"sec-websocket-protocol">>).
-define(WSMSGPACK,<<"wamp.2.msgpack">>).
-define(WSJSON,<<"wamp.2.json">>).
-define(WSMSGPACK_BATCHED,<<"wamp.2.msgpack.batched">>).
-define(WSJSON_BATCHED,<<"wamp.2.json.batched">>).


-record(state,{
               enc = undefined,
               ws_enc = undefined,
               length = infitity,
               buffer = <<"">>,
               session = undefined
              }).


init( Req, _State) ->
  % need to check for the wamp.2.json or wamp.2.msgpack
  Protocols = cowboy_req:parse_header(?SUBPROTHEADER, Req),
  case find_supported_protocol(Protocols) of
    {Enc, WsEncoding, Header} ->
      Req1  = cowboy_req:set_resp_header(?SUBPROTHEADER,Header,Req),
      %% Peer = cowboy_req:peer(Req1),
      {cowboy_websocket,Req1,#state{enc=Enc,ws_enc=WsEncoding}};
    _ ->
      % unsupported
      {shutdown,Req}
  end.



websocket_handle({WsEnc, Data},
                 #state{ws_enc=WsEnc, enc=Enc, buffer=OldBuffer} = State) ->
    Buffer = <<OldBuffer/binary, Data/binary>>,
    {_MList, NewBuffer} = ct_msg:deserialize(Buffer, Enc),
  %% {ok,OutFrames,NewRouting} = handle_messages(MList,[],Routing,State),
  %% {reply,OutFrames,Req,State#state{buffer=NewBuffer}};
    {ok, State#state{buffer=NewBuffer}};
websocket_handle(Data, State) ->
    lager:error("unsupported data at websocket: ~p", [Data]),
  {ok, State}.

websocket_info(erwa_stop, Req, State) ->
  {stop,Req,State};
%% websocket_info({erwa,Msg}, Req, #state{ws_enc=WsEnc,enc=Enc}=State) when is_tuple(Msg)->
%%   Encode = fun(M) ->
%%              {WsEnc,wamper_protocol:serialize(M,Enc)}
%%            end,
%%   case erwa_routing:handle_info(Msg, Routing) of
%%     {ok, NewRouting} ->
%%       {ok,Req,State#state{routing=NewRouting}};
%%     {send, OutMsg, NewRouting} ->
%%       {reply,Encode(OutMsg),Req,State#state{routing=NewRouting}};
%%     {send_stop, OutMsg, NewRouting} ->
%%       self() ! erwa_stop,
%%       {reply,Encode(OutMsg),Req,State#state{routing=NewRouting}};
%%     {stop, NewRouting} ->
%%       {stop,Req,State#state{routing=NewRouting}}
%%   end;
websocket_info(_Data, Req, State) ->
  {ok,Req,State}.

terminate(_Reason, _Req, _State) ->
  ok.


%% handle_messages([],ToSend,Routing,_State) ->
%%   {ok,lists:reverse(ToSend),Routing};
%% handle_messages([Msg|Tail],ToSend,Routing,#state{ws_enc=WsEnc,enc=Enc}=State) ->
%%   Encode = fun(M) ->
%%              {WsEnc,wamper_protocol:serialize(M,Enc)}
%%            end,
%%   case erwa_routing:handle_message(Msg, Routing) of
%%     {ok, NewRouting} ->
%%       handle_messages(Tail,ToSend,NewRouting,State);
%%     {reply, OutMsg, NewRouting} ->
%%       handle_messages(Tail,[Encode(OutMsg)|ToSend],NewRouting,State);
%%     {reply_stop, OutMsg, NewRouting} ->
%%       self() ! erwa_stop,
%%       {ok,lists:reverse([Encode(OutMsg)|ToSend]),NewRouting};
%%     {stop, NewRouting} ->
%%       self() ! erwa_stop,
%%       {ok, lists:reverse(ToSend), NewRouting}
%%   end.



-spec find_supported_protocol([binary()])
                             -> ProtocolOrError
                                    when ProtocolOrError
                                         :: none |
                                            {json | json_batched | msgpack |
                                             msgpack_batched,
                                             text|binary,
                                             binary()}.
find_supported_protocol([]) ->
  none;
find_supported_protocol([?WSJSON|_T]) ->
  {json,text,?WSJSON};
find_supported_protocol([?WSJSON_BATCHED|_T]) ->
  {json_batched,text,?WSJSON_BATCHED};
find_supported_protocol([?WSMSGPACK|_T]) ->
  {msgpack,binary,?WSMSGPACK};
find_supported_protocol([?WSMSGPACK_BATCHED|_T]) ->
  {msgpack_batched,binary,?WSMSGPACK_BATCHED};
find_supported_protocol([_|T]) ->
  find_supported_protocol(T).
