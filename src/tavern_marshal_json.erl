-module(tavern_marshal_json).

-export([decode/1, encode/1]).

-include_lib("eunit/include/eunit.hrl").

-spec decode(binary()) -> {ok, Data :: tavern_http:tree()} | {error, Error :: atom()}.
decode(Payload) ->
	case (catch mochijson2:decode(Payload)) of
		{struct, [_]} = Res         -> {ok, [decode_mochistruct(Res)]};
		{struct, _} = Res           -> {ok, decode_mochistruct(Res)};
		Struct when is_list(Struct) -> {ok, decode_mochistruct(Struct)};
		_                           -> {error, 'invalid json payload'}
	end.

-spec encode(tavern_http:tree()) -> {ok, Data :: iolist()} | {error, Error :: atom()}.
encode(Payload) -> 
	case (catch mochijson2:encode(Payload)) of
		{'EXIT', _} -> {error, 'json serialization failed'};
		Data -> {ok, Data}
	end.
%% Optimistic, we don't have a "objects" in the JSON sense, instead we have a notion
%% similar to DOM node lists, meaning there can be no single object that's not inside
%% a list.
-spec decode_mochistruct({struct | binary(), [any()] | any()} | [any()]) -> [any()].
decode_mochistruct({K, {struct, _} = V})              -> {K, [decode_mochistruct(V)]};
decode_mochistruct({struct, [V]})                     -> decode_mochistruct(V); 
decode_mochistruct({struct, V})                       -> decode_mochistruct(V); 
decode_mochistruct({<<K/binary>>, V})                 -> {K, decode_mochistruct(V)};
decode_mochistruct(List) when is_list(List)           -> [decode_mochistruct(S) || S <- List];
decode_mochistruct(V)                                 -> V.

-ifdef(TEST).
	encode_single_level_test() ->
		{ok, _} = encode([{level1, [{level2, "value"}]}]),
		{ok, _} = encode([{level1, [{level2, "value"}, {level2, "value2"}]}]).

	encode_datatypes_test() ->
		{ok, _} = encode([{level1, [{level2, atomvalue}]}]),
		{ok, _} = encode([{level1, [{level2, <<"binaryval">>}]}]),
		{ok, _} = encode([{level1, [{level2, 1.23}]}]),
		{ok, _} = encode([{level1, [{level2, 1000000}]}]),
		{ok, _} = encode([{level1, [{level2, "stringvalue"}]}]).

	encode_multilevel_test() ->
		{ok, _} = encode([{level1, [{level2, [{level3, [{level4, <<"value">>}]}]}]}]).

	encode_common_elem_test() ->
		{ok, _} = encode([{level1, [
			  {level2a, <<"value A">>}
			, {level2b, <<"value B">>}
			, {level2c, <<"value C">>}
			, {level2d, <<"value D">>}
		]}]).

	decode_test() ->
		?assertEqual({ok, [{<<"t0a">>,  <<"v0">>}]},
			decode(<<"{\"t0a\" : \"v0\"}">>)),
		?assertEqual({ok, [{<<"t0b">>,  <<"v0">>}]},
			decode(<<"[{\"t0b\" : \"v0\"}]">>)),
		?assertEqual({ok, [{<<"t1">>,[{<<"k2">>,  <<"v2">>}]}]},
			decode(<<"{\"t1\" :  {\"k2\" : \"v2\"}}">>)),
		?assertEqual({ok, [{<<"t2">>,[{<<"k2">>, <<"v2">>}, {<<"k3">>, <<"v3">>}]}]},
			decode(<<"{\"t2\":[{\"k2\":\"v2\"}, {\"k3\" : \"v3\"}]}">>)),
		?assertEqual({ok, [{<<"t3">>,[{<<"k2">>, <<"v2">>}]}]},
			decode(<<"{\"t3\":[{\"k2\":\"v2\"}]}">>)),
		?assertEqual({ok, [{<<"t4">>,[{<<"k2">>, <<"v2">>}, {<<"k3">>, <<"v3">>}]}]},
			decode(<<"[{\"t4\":[{\"k2\":\"v2\"},{\"k3\":\"v3\"}]}]">>)),
		?assertEqual({ok, [{<<"t5">>,[{<<"k1">>, <<"v1">>}]}, {<<"t5a">>, <<"v2">>}]},
			decode(<<"[{\"t5\":{\"k1\":\"v1\"}}, {\"t5a\":\"v2\"}]">>)).

	decode_multiprop_object_test() ->
		?assertEqual({ok, [{<<"m0a">>, <<"v0a">>}, {<<"m0b">>, <<"v0b">>}]},
			decode(<<"{\"m0a\":\"v0a\", \"m0b\" : \"v0b\"}">>)).

	decode_datatypes_test() ->
		?assertEqual({ok, [{<<"p0">>, 1.23}]},      decode(<<"{\"p0\":1.23}">>)),
		?assertEqual({ok, [{<<"p1">>, 4}]},         decode(<<"{\"p1\":4}">>)),
		?assertEqual({ok, [{<<"p2">>, null}]},      decode(<<"{\"p2\":null}">>)),
		?assertEqual({ok, [{<<"p3">>, true}]},      decode(<<"{\"p3\":true}">>)),
		?assertEqual({ok, [{<<"p4">>, false}]},     decode(<<"{\"p4\":false}">>)),
		{error, _} = decode(<<"{\"p5\":undefined}">>),
		{error, _} = decode(<<"{\"p6\":ok}">>).

	encode_decode_test() ->
		ErlTerm = [{<<"level1">>, [{<<"level2">>, [{<<"level3">>, [{<<"level4">>, <<"value">>}]}]}]}],
		{ok, JSON} = encode(ErlTerm),
		?assertEqual({ok, ErlTerm}, decode(JSON)).

	encode_atom_key_test() ->
		{ok, _} = encode([{level1, [{level2, "value"}]}]).
-endif.
