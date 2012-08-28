-module(resource_handler).

-behaviour(cowboy_http_handler).

-export([allowed_methods/2, content_types_accepted/2, content_types_provided/2,
	handle_put/2, handle_get/2]).

-include_lib("tavern/include/rest_module.hrl").

allowed_methods(Req, State) ->
	{['GET', 'PUT'], Req, State}.

content_types_accepted(Req, State) ->
	{ [ {<<"application">>, <<"xml">>, []}
	  , {<<"octet">>, <<"stream">>, []}]
	, Req
	, State}.

content_types_provided(Req, State) ->
	{ [ {<<"application">>, <<"xml">>, []}
	  , {<<"octet">>, <<"stream">>, []}]
	, Req
	, State}.

handle_get(Req, State) ->
	{ID, Req} = cowboy_http_req:binding(id, Req),
	{'OK', Req, State, [{result, [{message, <<"fetch: ", ID/binary>>}]}]}.

handle_put(Req, #tavern{body = _Body} = State) ->
	{ID, Req} = cowboy_http_req:binding(id, Req),
	{'Created', Req, State, [{result, [{message, <<"created: ", ID/binary>>}]}]}.
