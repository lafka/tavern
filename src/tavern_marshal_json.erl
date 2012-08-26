-module(tavern_marshal_json).

-export([decode/1, encode/1]).

-spec decode(binary()) -> {ok, Data :: tavern_http:tree()} | {error, Error :: atom()}.
decode(Payload) ->
	case (catch mochijson2:decode(Payload)) of
		{struct, _} -> {ok, encode_mochistruct(Payload)};
		_           -> {error, 'invalid json payload'}
	end.

-spec encode(tavern_http:tree()) -> {ok, Data :: iolist()} | {error, Error :: atom()}.
encode(Payload) ->
	case (catch mochijson2:encode(Payload)) of
		{'EXIT', _} -> {error, 'json serialization failed'};
		Data -> {ok, Data}
	end.

-spec encode_mochistruct({struct, [any()]}) -> [any()].
encode_mochistruct({struct, Struct}) ->
	[{binary_to_existing_atom(A, utf8), encode_mochistruct(B)} || {A, B} <- Struct];

encode_mochistruct(Struct) ->
	Struct.
