-module(tavern_marshal_xml).

-export([decode/1, encode/1]).

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

encode(Payload) ->
	Enc = fun(_, {K, V}) when is_atom(V)    -> {K, [atom_to_list(V)]};
	         (_, {K, V}) when is_binary(V)  -> {K, [binary_to_list(V)]};
	         (_, {K, V}) when is_integer(V) -> {K, [integer_to_list(V)]};
	         (F, [{K, V}]) -> [F(F, {K, V})];
	         (F, {K, V}) -> {K, F(F, V)} end,

	case (catch xmerl:export_simple(Enc(Enc, Payload), xmerl_xml)) of
		{'EXIT', _} -> {error, 'xml serialization failed'};
		Data -> {ok, Data}
	end.
