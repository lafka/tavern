-module(tavern_marshal_xml).

-export([decode/1, encode/1]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-spec decode(iolist()) -> {ok, Data :: tavern_http:tree()} | {error, Error :: atom()}.
decode(Payload) when is_binary(Payload) ->
	decode(binary_to_list(Payload));
decode(Payload) ->
	case (catch xmerl_scan:string(Payload)) of
		{#xmlElement{name = K, content = V}, _} -> {ok, [{atom_to_binary(K, utf8), decode_xmerl(V)}]};
		 _ -> {error, 'invalid xml payload'}
	end.

-spec encode(tavern_http:tree()) -> {ok, Data :: binary()} | {error, Error :: atom()}.
encode(Payload) ->
		case (catch xmerl:export_simple(encode_list(Payload), xmerl_xml)) of
		{'EXIT', A} -> {error, 'xml serialization failed'};
		Data -> {ok, Data}
	end.

-spec encode_list(Struct :: [{atom(), any()}]) -> list().
encode_list(List) ->
	encode_list(List, []).

-spec encode_list(Struct :: [{atom(), any()}], Acc :: list()) -> list().
encode_list([], Acc) ->
	lists:reverse(Acc);

encode_list([{Key, Payload} | Tail], Acc) ->
	encode_list(Tail, [{encode_key(Key), [{encode_key(A), encode_value(B)} || {A, B} <- Payload]} | Acc]);
encode_list([Value | Tail], Acc) ->
	encode_list(Tail, [encode_value(Value) | Acc]).

-spec encode_value(binary() | atom() | integer() | any()) -> list().
encode_value(V)   when is_binary(V)               -> [binary_to_list(V)];
encode_value([V]) when is_binary(V)               -> [binary_to_list(V)];
encode_value(V)   when is_atom(V)                 -> [atom_to_list(V)];
encode_value(V)   when is_float(V); is_integer(V) -> [mochinum:digits(V)];
encode_value([{_, _} | _ ] = V)                 -> encode_list(V);
encode_value(V)                                 -> [V].

-spec encode_key(Key :: binary() | any()) -> atom().
encode_key(Key) when is_binary(Key) ->
	binary_to_atom(Key, utf8);
encode_key(Key) when is_atom(Key) ->
	Key;
encode_key(_) -> exit(invalid_key_type).

decode_xmerl([#xmlText{value = V, type = text}]) ->
    list_to_binary(V);
decode_xmerl(X) when is_list(X) ->
    [{atom_to_binary(K, utf8), decode_xmerl(V)} || #xmlElement{name = K, content = V} <- X].

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

	encode_binary_key_test() ->
		{ok, _} = encode([{<<"level1">>, [{<<"level2">>, "value"}]}]).

	decode_test() ->
		{ok, _} = decode("<?xml version=\"1.0\"?><root><abc>def</abc></root>").
-endif.
