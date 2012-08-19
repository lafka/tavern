-module(fallback_handler).

-behaviour(cowboy_http_handler).

-export([methods/1, handle_put/2, handle_req/2, handlers/1 ,handle_patch/2]).

-include_lib("tavern/include/rest_module.hrl").

handlers(_Req) ->
	Handler = handle_404,
	[{'HEAD',   Handler}, {'GET',   Handler},
	 {'POST',   Handler}, {'PATCH', Handler}].

methods(_Req) ->
	['HEAD', 'GET', 'POST', 'PUT', 'PATCH'].

handle_get(Req, State) ->
	{'Ok', Req, State, [{hello, <<"world">>}], Req, State}.

handle_req(Req, State) ->
	{'Ok', Req, State, [{hello, <<"world">>}], Req, State}.

handle_put(Req, #tavern{body = _Body} = State) ->
	{'Created', Req, State, [{message, <<"world replaces">>}]}.

handle_patch(Req, State) ->
	{'Ok', Req, State, [{message, <<"new world order">>}]}.
