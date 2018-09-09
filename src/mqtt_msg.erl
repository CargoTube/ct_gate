%%
%% Copyright (c) 2018 Bas Wegh

-module(mqtt_msg).



%% for ranch_protocol
-export([parse/1]).

-export([connect/1, connack/2, 
	 publish/4, publish/2, puback/1, 
	 pubrec/1, pubrel/1, 
	 pubcomp/1, subscribe/2, 
	 suback/2, unsubscribe/2, 
	 unsuback/1, pingreq/0, 
	 pingresp/0, disconnect/0 
	]).

-export([test/0]).

connect(Id) ->
	ProtName = string(<<"MQTT">>),
	ProtLevel = <<4:8>>,
	ConFlags = <<0:8>>,
	KeepAlive = <<1:16>>,
	ClientId = string(Id),
	VarHeader = <<ProtName/binary, ProtLevel/binary, ConFlags/binary, KeepAlive/binary>>,
	Payload = ClientId,
	packet(<<1:4, 0:4>>, VarHeader, Payload).


connack(SessPres, RetCode) ->
	SessPresBit = to_bit(SessPres),
	VarHeader = << 0:7, SessPresBit:1, RetCode:8 >>, 
	packet(<<2:4, 0:4>>, VarHeader, <<>>).

publish(TopicName, Data) ->
  publish(TopicName, Data, 0, 0).

publish(TopicName, Data, PacketId, Qos) ->
	Dup = 0, 
	Retain =0,
	VarHeader = publish_var_header(string(TopicName), PacketId, Qos),
	packet(<<3:4, Dup:1, Qos:2, Retain:1>>, VarHeader, Data).

publish_var_header(TopicName, PacketId, Qos) when Qos == 1; Qos == 2 ->
	<< TopicName/binary, PacketId:16/unsigned-integer-big >>;
publish_var_header(TopicName, _PacketId, _) ->
	<< TopicName/binary>>.

puback(PacketId) ->
	VarHeader = << PacketId:16/unsigned-integer-big >>,
	packet(<<4:4, 0:4>>, VarHeader, <<>>).

pubrec(PacketId) ->
	VarHeader = << PacketId:16/unsigned-integer-big >>,
	packet(<<5:4, 0:4>>, VarHeader, <<>>).

pubrel(PacketId) ->
	VarHeader = << PacketId:16/unsigned-integer-big >>,
	packet(<<6:4, 2:4>>, VarHeader, <<>>).

pubcomp(PacketId) ->
	VarHeader = << PacketId:16/unsigned-integer-big >>,
	packet(<<7:4, 0:4>>, VarHeader, <<>>).

subscribe(PacketId, TopicFilter) ->
	VarHeader = << PacketId:16/unsigned-integer-big >>,
	GenPayload =
	    fun({Topic, Qos}, PL) ->
	        TopicString = string(Topic),
	        << TopicString/binary, 0:6, Qos:2, PL/binary>>
	    end,
	Payload = lists:foldl(GenPayload, <<>>, TopicFilter),
	packet(<<8:4, 2:4>>, VarHeader, Payload).

suback(PacketId, ReturnCodes) ->
	VarHeader = << PacketId:16/unsigned-integer-big >>,
	GenPayload =
	    fun(ReturnCode, PL) ->
	        << ReturnCode:8, PL/binary>>
	    end,
	Payload = lists:foldl(GenPayload, <<>>, ReturnCodes),
	packet(<<9:4, 0:4>>, VarHeader, Payload).

unsubscribe(PacketId, TopicList) ->
	VarHeader = << PacketId:16/unsigned-integer-big >>,
	GenPayload =
	    fun(TopicFilter, PL) ->
	        TopicString = string(TopicFilter),
	        << TopicString/binary, PL/binary>>
	    end,
	Payload = lists:foldl(GenPayload, <<>>, TopicList),
	packet(<<10:4, 2:4>>, VarHeader, Payload).

unsuback(PacketId) ->
	VarHeader = << PacketId:16/unsigned-integer-big >>,
	packet(<<11:4, 0:4>>, VarHeader, <<>>).

pingreq() ->
	packet(<<12:4, 0:4>>, <<>>, <<>>).

pingresp() ->
	packet(<<13:4, 0:4>>, <<>>, <<>>).

disconnect() ->
	packet(<<14:4, 0:4>>, <<>>, <<>>).


packet(FixedHeader, VarHeader, Payload) ->
	VarContent = << VarHeader/binary, Payload/binary>>,
	RemLength = remaining_length(VarContent),
	<<FixedHeader/binary, RemLength/binary, VarContent/binary>>.


parse(<< Type:4/unsigned, Flags:4, 0:1, Len:7/unsigned, Rest/binary >> = Buffer) ->
	RemainingLength = calc_remaining_length([Len]),
	maybe_parse_packet(Type, Flags, RemainingLength, Rest, Buffer);
parse(<< Type:4/unsigned, Flags:4, 1:1, Len1:7/unsigned, 0:1, Len2:7/unsigned, Rest/binary >> = Buffer) ->
	RemainingLength = calc_remaining_length([Len1, Len2]),
	maybe_parse_packet(Type, Flags, RemainingLength, Rest, Buffer);
parse(<< Type:4/unsigned, Flags:4, 1:1, Len1:7/unsigned, 1:1, Len2:7/unsigned, 
	 0:1, Len3:7/unsigned, Rest/binary >> = Buffer) ->
	RemainingLength = calc_remaining_length([Len1, Len2, Len3]),
	maybe_parse_packet(Type, Flags, RemainingLength, Rest, Buffer);
parse(<< Type:4/unsigned, Flags:4, 1:1, Len1:7/unsigned, 1:1, Len2:7/unsigned, 
	 1:1, Len3:7/unsigned, 0:1, Len4:7/unsigned, Rest/binary >> = Buffer) ->
	RemainingLength = calc_remaining_length([Len1, Len2, Len3, Len4]),
	maybe_parse_packet(Type, Flags, RemainingLength, Rest, Buffer);
parse(<< _Type:4/unsigned, _Flags:4, 1:1, _Len1:7/unsigned, 1:1, _Len2:7/unsigned, 
	 1:1, _Len3:7/unsigned, 1:1, _Len4:7/unsigned, _Rest/binary >>) ->
	throw(bad_remaining_length);
parse(Buffer) ->
	{{none, #{}}, Buffer}.

maybe_parse_packet(Type, Flags, RemainingLength, Rest, _Buffer) when byte_size(Rest) >= RemainingLength  ->
	<< PacketData:RemainingLength/binary, NewBuffer/binary >> = Rest, 
	parse_packet(Type, Flags, PacketData, NewBuffer);
maybe_parse_packet(_Type, _Flags, _RemainingLength, _Rest, Buffer)  ->
	{{none, #{}}, Buffer}.


parse_packet(1, Flags, Data, NewBuffer) ->
	parse_connect(Flags, Data, NewBuffer);
parse_packet(2, Flags, Data, NewBuffer) ->
	parse_connack(Flags, Data, NewBuffer);
parse_packet(3, Flags, Data, NewBuffer) ->
	parse_publish(Flags, Data, NewBuffer);
parse_packet(4, Flags, Data, NewBuffer) ->
	parse_puback(Flags, Data, NewBuffer);
parse_packet(5, Flags, Data, NewBuffer) ->
	parse_pubrec(Flags, Data, NewBuffer);
parse_packet(6, Flags, Data, NewBuffer) ->
	parse_pubrel(Flags, Data, NewBuffer);
parse_packet(7, Flags, Data, NewBuffer) ->
	parse_pubcomp(Flags, Data, NewBuffer);
parse_packet(8, Flags, Data, NewBuffer) ->
	parse_subscribe(Flags, Data, NewBuffer);
parse_packet(9, Flags, Data, NewBuffer) ->
	parse_suback(Flags, Data, NewBuffer);
parse_packet(10, Flags, Data, NewBuffer) ->
	parse_unsubscribe(Flags, Data, NewBuffer);
parse_packet(11, Flags, Data, NewBuffer) ->
	parse_unsuback(Flags, Data, NewBuffer);
parse_packet(12, Flags, Data, NewBuffer) ->
	parse_pingreq(Flags, Data, NewBuffer);
parse_packet(13, Flags, Data, NewBuffer) ->
	parse_pingresp(Flags, Data, NewBuffer);
parse_packet(14, Flags, Data, NewBuffer) ->
	parse_disconnect(Flags, Data, NewBuffer);
parse_packet(_Type, _Flags, _Data, _NewBuffer) ->
	throw(unknown_package).


parse_connect(_Flags, Data, NewBuffer) ->
	{ProtName, Data1} = parse_string(Data),
	<< ProtLevel:8/unsigned, ConFlags:8, Data2/binary>> = Data1,
	{KeepAlive, Data3} = parse_int(Data2),
	Strings = parse_list_of_strings(Data3),
	<< UserName:1, Pwd:1, WillRetain:1, WillQos:2/unsigned, Will:1, CleanSess:1, 0:1 >> = << ConFlags >>,
	{{connect, #{prot_level => ProtLevel, keep_alive => KeepAlive, 
		     prot_name => ProtName,
		    flags => #{ user_name => to_bool(UserName),
				password => to_bool(Pwd),
				will_retain => to_bool(WillRetain),
				will_qos => WillQos,
				will => to_bool(Will),
				clean_session => to_bool(CleanSess)
			       } ,
		    strings => Strings
		   }}, NewBuffer}.


parse_connack(_Flags, Data, NewBuffer) ->
	<< 0:7, SessPresent:1, ReturnCode:8/unsigned >> = Data,
	{{connack, #{ session_present => to_bool(SessPresent),
		      return_code => ReturnCode }}, NewBuffer}. 

parse_publish(Flags, Data, NewBuffer) ->
	<< Dup:1, Qos:2/unsigned, Retain:1 >> = << Flags:4 >>,
	{TopicName, Data1} = parse_string(Data),
	{PacketId, Payload} = parse_pub_packet_id(Qos, Data1),
	{{publish, #{duplicate => to_bool(Dup), qos => Qos, 
		    packet_id => PacketId, retain => Retain,
		    topic => TopicName, payload => Payload}}, NewBuffer}.

parse_pub_packet_id(1, Data) ->
    parse_int(Data);
parse_pub_packet_id(2, Data) ->
    parse_int(Data);
parse_pub_packet_id(_, Data) ->
    {-1, Data}.


parse_puback(_Flags, Data, NewBuffer) ->
	{PacketId, <<>>} = parse_int(Data),
	{{puback, #{packet_id => PacketId}}, NewBuffer}.

parse_pubrec(_Flags, Data, NewBuffer) ->
	{PacketId, <<>>} = parse_int(Data),
	{{pubrec, #{packet_id => PacketId}}, NewBuffer}.

parse_pubrel(_Flags, Data, NewBuffer) ->
	{PacketId, <<>>} = parse_int(Data),
	{{pubrel, #{packet_id => PacketId}}, NewBuffer}.

parse_pubcomp(_Flags, Data, NewBuffer) ->
	{PacketId, <<>>} = parse_int(Data),
	{{pubcomp, #{packet_id => PacketId}}, NewBuffer}.

parse_subscribe(_Flags, Data, NewBuffer) ->
	{PacketId, Data1} = parse_int(Data),
	TopicList = parse_list_of_topic_filter(Data1),
	{{subscribe, #{packet_id => PacketId, 
		       topic_list => TopicList}}, NewBuffer}.

parse_suback(_Flags, Data, NewBuffer) ->
	{PacketId, Data1} = parse_int(Data),
	RetCodes = parse_list_of_ret_codes(Data1),
	{{suback, #{packet_id => PacketId,
		    return_codes => RetCodes
		   }}, NewBuffer}.

parse_unsubscribe(_Flags, Data, NewBuffer) ->
    {PacketId, Data1} = parse_int(Data),
    TopicList = parse_list_of_strings(Data1),
    {{unsubscribe, #{packet_id => PacketId, 
		     topic_list => TopicList}}, NewBuffer}. 
    

parse_unsuback(_Flags, Data, NewBuffer) ->
    {PacketId, <<>>} = parse_int(Data),
    {{unsuback, #{packet_id => PacketId}}, NewBuffer}. 

parse_pingreq(_Flags, <<>>, NewBuffer) ->
    {{pingreq, #{}}, NewBuffer}.

parse_pingresp(_Flags, <<>>, NewBuffer) ->
    {{pingresp, #{}}, NewBuffer}.

parse_disconnect(_Flags, <<>>, NewBuffer) ->
    {{disconnect, #{}}, NewBuffer}.


parse_int(Data) ->
    <<Int:16/unsigned-integer-big, Rest/binary>> = Data,
    {Int, Rest}.

parse_string(<<0:16/unsigned-integer-big, Rest/binary >>)  ->
    {<<>>, Rest};
parse_string(<<Length:16/unsigned-integer-big, Rest/binary >>) when byte_size(Rest) >= Length ->
    << String:Length/binary, NewBuffer/binary >> = Rest, 
    {String, NewBuffer};
parse_string(_) ->
    throw(bad_string_length).

parse_list_of_strings(Data) ->
    parse_list_of_strings(Data, []).

parse_list_of_strings(<<>>, Strings) ->
    lists:reverse(Strings);
parse_list_of_strings(Data, Strings) ->
    {String, NewData} = parse_string(Data),
    parse_list_of_strings(NewData, [String | Strings]).

parse_list_of_topic_filter(Data) ->
    parse_list_of_topic_filter(Data, []).

parse_list_of_topic_filter(<<>>, Filter) ->
    lists:reverse(Filter);
parse_list_of_topic_filter(Data0, Filter) ->
    {Topic, Data} = parse_string(Data0),
    << 0:6, Qos:2/unsigned, NewData/binary>> = Data,
    parse_list_of_topic_filter(NewData, [{Topic, Qos} | Filter]).

parse_list_of_ret_codes(Data) -> 
    parse_list_of_ret_codes(Data, []).

parse_list_of_ret_codes(<<>>, List) -> 
    lists:reverse(List);
parse_list_of_ret_codes(Data, List) -> 
    << RetCode:8/unsigned, NewData/binary >> = Data,
    parse_list_of_ret_codes(NewData, [ RetCode | List ]).

calc_remaining_length(List) ->
    CalcLength = fun(Value, Current) ->
			(Current * 128) + Value
		 end,
    lists:foldl(CalcLength, 0, List).


remaining_length(Data) ->
  Len = byte_size(Data),
  remaining_length(Len, <<>>).

remaining_length(0, <<>>) ->
    << 0 >>;
remaining_length(0, Enc) ->
    Enc;
remaining_length(Len, Enc) ->
    PartLen = Len rem 128,
    NewLen = Len - PartLen,
    NewEnc = encode_length(PartLen, NewLen, Enc),
    remaining_length(round(NewLen/128), NewEnc).
    
encode_length(PartLen, 0, Enc) ->
    << Enc/binary, 0:1, PartLen:7/unsigned >>;
encode_length(PartLen, _, Enc) ->
    << Enc/binary, 1:1, PartLen:7/unsigned >>.


string(String) ->
	Len = byte_size(String),
	<<Len:16/unsigned-integer-big, String/binary>>.

to_bool(0) ->
	false;
to_bool(_) ->
	true.

to_bit(true) ->
	1;
to_bit(false) ->
	0.

test() ->
    redbug:start(["mqtt_msg -> return"], [{msgs, 100000}, {time, 3600000}]),

    io:format("connect"),
    Con = connect(<<"test">>),
    {{connect, _}, <<>>} = parse(Con),
    sleep(500),

    io:format("concack"),
    ConnAck = connack(false, 0),
    {{connack, _}, <<>>} = parse(ConnAck),
    sleep(500),

    io:format("pub1"),
    Publish1 = publish(<<"test">>,<<"one">>,4,0),
    {{publish, Map1}, <<>>} = parse(Publish1),
    -1 = maps:get(packet_id, Map1, 0),
    sleep(500),

    io:format("pub2"),
    Publish2 = publish(<<"test">>,<<"one">>,4,1),
    {{publish, Map2}, <<>>} = parse(Publish2),
    4 = maps:get(packet_id, Map2, 0),
    sleep(500),

    io:format("puback"),
    PubAck = puback(4),
    {{puback, _}, <<>>} = parse(PubAck),
    sleep(500),

    io:format("pubrec"),
    PubRec = pubrec(4),
    {{pubrec, _}, <<>>} = parse(PubRec),
    sleep(500),

    io:format("pubrel"),
    PubRel = pubrel(4),
    {{pubrel, _}, <<>>} = parse(PubRel),
    sleep(500),

    io:format("pubcomp"),
    PubComp = pubcomp(4),
    {{pubcomp, _}, <<>>} = parse(PubComp),
    sleep(500),

    io:format("sub"),
    Subscribe = subscribe(23,[{<<"topic1">>,2},{<<"topic2">>,0}]),
    {{subscribe, _}, <<>>} = parse(Subscribe),
    sleep(500),

    io:format("suback"),
    Suback = suback(23, [1,0]),
    {{suback, _}, <<>>} = parse(Suback),
    sleep(500),

    io:format("unsub"),
    Unsubscribe = unsubscribe(13, [<<"topic1">>, <<"topic2">>]),
    {{unsubscribe, _}, <<>>} = parse(Unsubscribe),
    sleep(500),

    io:format("unsuback"),
    Unsuback = unsuback(13),
    {{unsuback, _}, <<>>} = parse(Unsuback),
    sleep(500),

    io:format("ping req"),
    Pingreq = pingreq(),
    {{pingreq, _}, <<>>} = parse(Pingreq),
    sleep(500),
    
    io:format("ping resp"),
    Pingresp = pingresp(),
    {{pingresp, _}, <<>>} = parse(Pingresp),
    sleep(500),

    io:format("disconnect"),
    Disconnect = disconnect(),
    {{disconnect, _}, <<>>} = parse(Disconnect),

    io:format("done"),
    ok.

sleep(Time) ->
	timer:sleep(Time).
