-module(tavern_marshal_xml).

-export([decode/1, encode/1]).

-include("types.hrl").
-include_lib("eunit/include/eunit.hrl").

-spec decode(binary()) -> {ok, Data :: tree()} | {error, Error :: atom()}.
decode(Payload) ->
	case erlsom:simple_form(Payload) of
		{ok, {_, _, NodeList}, _} ->
			F = fun(H, [I | _] = S) ->
				case I of
					{_,_,_} -> [{list_to_atom(A), H(H, C)} || {A, _, C} <- S];
					_       -> list_to_binary(S)
				end
			end,
			F(F, NodeList);
		_ -> {error, 'xml unserialization failed'}
	end.

-spec encode(tree()) -> {ok, Data :: binary()} | {error, Error :: atom()}.
encode(Payload) ->
		case (catch xmerl:export_simple(encode_list(Payload), xmerl_xml)) of
		{'EXIT', _} -> {error, 'xml serialization failed'};
		Data -> {ok, Data}
	end.

-spec encode_list(Struct :: [{atom(), any()}]) -> list().
encode_list(List) ->
	encode_list(List, []).

-spec encode_list(Struct :: [{atom(), any()}], Acc :: list()) -> list().
encode_list([], Acc) ->
	lists:reverse(Acc);

encode_list([{Key, Payload} | Tail], Acc) ->
	encode_list(Tail, [{Key, [{A, encode_value(B)} || {A, B} <- Payload]} | Acc]).

-spec encode_value(binary() | atom() | integer() | any()) -> list().
encode_value(V) when is_binary(V)               -> [binary_to_list(V)];
encode_value(V) when is_atom(V)                 -> [atom_to_list(V)];
encode_value(V) when is_float(V); is_integer(V) -> [mochinum:digits(V)];
encode_value([{_, _} | _ ] = V)                 -> encode_list(V);
encode_value(V)                                 -> [V].


-ifdef(TEST).
	-record(testrec, {abc, def}).

	encode_single_level_test() ->
		{ok, _} = encode([{level1, [{level2, "value"}]}]),
		{ok, _} = encode([{level1, [{level2, "value"}, {level2, "value2"}]}]).

	encode_datatypes_test() ->
		{ok, _} = encode([{level1, [{level2, atomvalue}]}]),
		{ok, _} = encode([{level1, [{level2, <<"binaryval">>}]}]),
		{ok, _} = encode([{level1, [{level2, 1.23}]}]),
		{ok, _} = encode([{level1, [{level2, 1000000}]}]),
		{ok, _} = encode([{level1, [{level2, "stringvalue"}]}]).

	encode_record_test() ->
		{error, _} = encode([{record, #testrec{abc=123, def=abc}}]).

	encode_multilevel_test() ->
		{ok, _} = encode([{level1, [{level2, [{level3, [{level4, <<"value">>}]}]}]}]).

	encode_common_elem_test() ->
		{ok, _} = encode([{level1, [
			  {level2a, <<"value A">>}
			, {level2b, <<"value B">>}
			, {level2c, <<"value C">>}
			, {level2d, <<"value D">>}
		]}]).
-endif.
