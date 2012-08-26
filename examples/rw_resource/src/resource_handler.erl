-module(resource_handler).

-behaviour(cowboy_http_handler).

-export([methods/1, consumes/1, provides/1, handle_put/2, handle_get/2]).

-include_lib("tavern/include/rest_module.hrl").

methods(_Req) ->
	['GET', 'PUT'].

consumes(_Req) ->
	[ {<<"application">>, <<"xml">>, []}
	, {<<"octet">>, <<"stream">>, []}].

provides(_Req) ->
	[ {<<"application">>, <<"xml">>, []}
	, {<<"octet">>, <<"stream">>, []}].

handle_get(Req, State) ->
	{ID, Req} = cowboy_http_req:binding(id, Req),
	{'OK', Req, State, [{result, [{message, <<"fetch: ", ID/binary>>}]}]}.

handle_put(Req, #tavern{body = _Body} = State) ->
	{ID, Req} = cowboy_http_req:binding(id, Req),
	{'Created', Req, State, [{result, [{message, <<"created: ", ID/binary>>}]}]}.
