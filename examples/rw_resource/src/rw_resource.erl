-module(rw_resource).
-behaviour(application).
-export([start/0, start/2, stop/1]).

-include_lib("tavern/include/rest_module.hrl").

start() ->
	application:start(crypto),
	application:start(public_key),
	application:start(ssl),
	application:start(cowboy),
	application:start(rw_resource).

start(_Type, _Args) ->
	Dispatch = [
		{'_', [
			{[<<"id">>, id], resource_handler, []},
			{'_', fallback_handler, []}
		]}
	],
	cowboy:start_listener(rw_resource_http_listener, 100,
		cowboy_tcp_transport, [{port, 8081}],
		cowboy_http_protocol, [{dispatch, Dispatch}]
	),
	error_logger:info_msg("Listening on 0.0.0.0:~B~n", [80801]),
	rw_resource_sup:start_link().

stop(_State) ->
	ok.
