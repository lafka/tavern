-module(fallback_handler).

-behaviour(cowboy_http_handler).

-export([method_handlers/2, allowed_methods/2, handle_404/2]).

-include_lib("tavern/include/rest_module.hrl").

method_handlers(Req, State) ->
	Handler = handle_404,
	{ [{'HEAD',   Handler}, {'GET', Handler},
	   {'POST',   Handler}, {'PUT', Handler},
	   {'PATCH', Handler},  {'OPTIONS', handle_default_options}]
	, Req
	, State}.

allowed_methods (Req, State) ->
	{['HEAD', 'GET', 'POST', 'PUT', 'PATCH', 'OPTIONS'], Req, State}.

handle_404(Req, State) ->
	{'Not Found', Req, State, [{error, [{message, <<"resource not found">>}, {code, 404}]}]}.

