-module(tavern_marshal_plain).

-export([decode/1, encode/1]).

decode(Payload) ->
	[Payload].

encode(Payload) ->
	{ok, io_lib:format("~p~n", [Payload])}.
