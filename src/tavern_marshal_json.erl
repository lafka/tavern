-module(tavern_marshal_json).

-export([decode/1, encode/1]).

-include_lib("eunit/include/eunit.hrl").

-spec decode(binary()) -> {ok, Data :: tavern_http:tree()} | {error, Error :: atom()}.
decode(Payload) ->
	case (catch mochijson2:decode(Payload)) of
		{struct, _}   = Res         -> {ok, decode_mochistruct(Res)};
		Struct when is_list(Struct) -> {ok, decode_mochistruct(Struct)};
		_                           -> {error, {'invalid json payload'}}
	end.

-spec encode(tavern_http:tree()) -> {ok, Data :: iolist()} | {error, Error :: atom()}.
encode(Payload) ->
	case (catch mochijson2:encode(Payload)) of
		{'EXIT', _} -> {error, 'json serialization failed'};
		Data -> {ok, Data}
	end.

%% Optimistic, we don't have a "list" in the JSON sense so we can assume
%% a list containing a single object is actually a multi level object.
%% This is to allow XML like <root><child>val</child></root> to be the same
%% as both {root : {child : "val"}} AND {root : [{child : "val"}]}.
-spec decode_mochistruct({struct, [any()] | [any()]}) -> [any()].
decode_mochistruct({struct, Struct})        -> decode_mochistruct(Struct);
decode_mochistruct([{Key, Struct}])         -> decode_mochistruct({Key, Struct});
decode_mochistruct({Key, Struct})           -> {Key, decode_mochistruct(Struct)};
decode_mochistruct(List) when is_list(List) -> [decode_mochistruct(A) || A <- List];
decode_mochistruct(Val)                     -> Val.


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
		{ok, {<<"t0">>,  <<"v0">>}}  = decode(
			<<"[{\"t0\" : \"v0\"}]">>),
		{ok, {<<"t1">>,{<<"k2">>,  <<"v2">>}}}  = decode(
			<<"{\"t1\" :  {\"k2\" : \"v2\"}}">>),
		{ok, {<<"t2">>,[{<<"k2">>, <<"v2">>}, {<<"k3">>, <<"v3">>}]}} = decode(
			<<"{\"t2\":[{\"k2\":\"v2\"}, {\"k3\" : \"v3\"}]}">>),
		{ok, {<<"t3">>,{<<"k2">>, <<"v2">>}}} = decode(
			<<"{\"t3\":[{\"k2\":\"v2\"}]}">>),
		{ok, {<<"t4">>,[{<<"k2">>, <<"v2">>}, {<<"k3">>, <<"v3">>}]}} = decode(
			<<"[{\"t4\":[{\"k2\":\"v2\"},{\"k3\":\"v3\"}]}]">>),
		{ok, [{<<"t5">>,{<<"k1">>, <<"v1">>}}, {<<"t5a">>, <<"v2">>}]} = decode(
			<<"[{\"t5\":{\"k1\":\"v1\"}}, {\"t5a\":\"v2\"}]">>).

%	encode_decode_test() ->
%		{ok, JSON} = encode([{level1, [{level2, [{level3, [{level4, <<"value">>}]}]}]}]),
%		{ok, _}    = decode(JSON).
%
%	encode_binary_key_test() ->
%		{ok, _} = encode([{<<"level1">>, [{<<"level2">>, "value"}]}]).
-endif.
