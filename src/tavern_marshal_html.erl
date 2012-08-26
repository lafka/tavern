-module(tavern_marshal_html).

-export([decode/1, encode/1]).

-spec decode(binary()) -> {error, not_implemented}.
decode(_Payload) ->
	{error, not_implemented}.

-spec encode(tavern_http:tree()) -> {error, not_implemented}.
encode(_Payload) ->
	{error, not_implemented}.
