-module(tavern_marshal_plain).

-export([decode/1, encode/1]).

-spec decode(binary()) -> {ok, Data :: tavern_http:tree()}.
decode(Payload) ->
	{ok, Payload}.

-spec encode(tavern_http:tree()) -> {ok, Data :: iolist()}.
encode(Payload) ->
	{ok, io_lib:format("~p~n", [Payload])}.
