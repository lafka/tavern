-module(tavern_http).

%% HTTP parse callbacks
-export([init/3, handle/3, terminate/3, status/1]).

-include("rest.hrl").
-include_lib("cowboy/include/http.hrl").

init(_Transport, Req, [Handler]) ->
	Defaults = [
		  {methods,  ['HEAD', 'GET', 'OPTIONS']}
		, {handlers, [
			  {'HEAD',    handle_head}
			, {'GET',     handle_get}
			, {'POST',    handle_post}
			, {'PUT',     handle_put}
			, {'DELETE',  handle_delete}
			, {'PATCH',   handle_patch}
			, {'OPTIONS', handle_default_options}
			, {unauthorized, fun handle_unauthorized/2}
			, {forbidden,    fun handle_forbidden/2}
			, {error,        fun handle_error/2}
		]}
		, {provides, [
			  {<<"text">>,       <<"html">>, []}
			, {<<"application">>,<<"xml">>,  []}
			, {<<"application">>,<<"json">>, []}
		]}
		, {consumes, [
			  {<<"text">>,        <<"html">>, []}
			, {<<"application">>, <<"xml">>,  []}
			, {<<"application">>, <<"json">>, []}
		]} ],
	SetRecord = fun(methods,  V, R) -> R#tavern{methods  = V};
	               (handlers, V, R) -> R#tavern{handlers = V};
	               (provides, V, R) -> R#tavern{provides = V};
	               (consumes, V, R) -> R#tavern{consumes = V} end,
	Fun = fun({X, Y}, Acc) ->
		case erlang:function_exported(Handler, X, 1) of
			true  ->
				A = Handler:X(Req),
				{{X, A}, SetRecord(X, A, Acc)};
			false ->
				{{X, Y}, SetRecord(X, Y, Acc)}
		end
	end,
	{_, State} = lists:mapfoldl( Fun, #tavern{}, Defaults),
	{ok, Req, State#tavern{module = Handler}}.

handle(Module, Req, #tavern{} = State) ->
	try
		case tavern_req:call(Req, State, fun tavern_req:client_acceptable/2) of
			%% Request is valid, pass it back to module for completion
			{true, Req2, State2} ->
				{Status, Payload, Req3, State3} = handle_call(Module, Req2, State2),
				error_logger:info_msg("resp: call: ~p ~p~n", [Status, Payload]),
				handle_resp(Req3, State3, Status, Payload);
			%% tavern_req specifies return status
			{{Status, Payload}, Req2, State2} ->
				error_logger:info_msg("resp: direct: ~p ~p~n", [Status, Payload]),
				handle_resp(Req2, State2, Status, Payload)
		end
	catch
		Class:Reason ->
			#tavern{} = State,
			error_logger:error_msg(
				"** Handler ~p terminating in tavern_http:handle/3~n"
				"   for the reason ~p:~p~n** "
				"** Request was ~p~n"
				"** State: ~p~n"
				"** Stacktrace: ~p~n~n",
				[Module, Class, Reason,  Req, State, erlang:get_stacktrace()]),
				 handle_error(Req, State),
			handle_error(Req, State)
	end.

terminate(_Handler, _Req, _State) ->
	ok.

handle_call(Module, Req, #tavern{handlers = Handlers} = State) ->
	{Method, Req}     = cowboy_http_req:method(Req),
	{Method, Handler} = lists:keyfind(Method, 1, Handlers),
	error_logger:info_msg("handler_list: ~p~nmethod: ~p~nhandler: ~p~n",
		[Handlers, Method, Handler]),
	case erlang:function_exported(Module, Handler, 2) of
		true  -> Module:Handler(Req, State);
		false -> erlang:error({no_export, Module, Handler})
	end.

handle_resp(Req, State, _Status, undefined) ->
	handle_error(Req, State, <<"empty response">>);

handle_resp(Req, State, Status, Payload) when is_atom(Status) ->
	handle_resp(Req, State, status(Status), Payload);

handle_resp(Req, #tavern{accept = Accept} = State, Status, Payload) ->
	try
		{ok, EncodedPayload} = tavern_marshal:encode(Accept, Payload),
		error_logger:info_msg("abc: ~p~n", [EncodedPayload]),
		A = cowboy_http_req:reply(Status, [], EncodedPayload, Req),
		{ok, Resp} = A,
		{ok, Resp, State}
	catch
		Class:Reason ->
			#tavern{module = Module} = State,
			error_logger:error_msg(
				"** Handler ~p terminating in tavern_http:handle_resp/4~n"
				"   for the reason: ~p:~p~n"
				"** stacktrace:~n~p~n",
				[Module, Class, Reason, erlang:get_stacktrace()]),
			{ok, Resp2} = cowboy_http_req:reply(status('Internal Server Error'),
				[], <<"(error #1001:) could write response">>,
				Req),
			{ok, Resp2, State}
	end.

handle_unauthorized(Req, State) ->
	handle_error(Req, State, status('Unauthorized'), <<"authentication required">>).

handle_forbidden(Req, State) ->
	handle_error(Req, State, status('Forbidden'), <<"unauthorized">>).

handle_error(Req, State) ->
	handle_error(Req, State, <<"(error #1000) an unexpected error occured">>).

handle_error(Req, State, Payload) ->
	handle_error(Req, State, status('Internal Server Error'), Payload).

handle_error(Req, State, Status, Payload) ->
	handle_resp(Req, State, Status, [{error, [{message, Payload}]}]).

-spec status(atom()) -> non_neg_integer().
status('Continue') ->                        100;
status('Switching Protocols') ->             101;
status('Processing') ->                      102;
status('OK') ->                              200;
status('Created') ->                         201;
status('Accepted') ->                        202;
status('Non-Authoritative Information') ->   203;
status('No Content') ->                      204;
status('Reset Content') ->                   205;
status('Partial Content') ->                 206;
status('Multi-Status') ->                    207;
status('IM Used') ->                         226;
status('Multiple Choices') ->                300;
status('Moved Permanently') ->               301;
status('Found') ->                           302;
status('See Other') ->                       303;
status('Not Modified') ->                    304;
status('Use Proxy') ->                       305;
status('Switch Proxy') ->                    306;
status('Temporary Redirect') ->              307;
status('Bad Request') ->                     400;
status('Unauthorized') ->                    401;
status('Payment Required') ->                402;
status('Forbidden') ->                       403;
status('Not Found') ->                       404;
status('Method Not Allowed') ->              405;
status('Not Acceptable') ->                  406;
status('Proxy Authentication Required') ->   407;
status('Request Timeout') ->                 408;
status('Conflict') ->                        409;
status('Gone') ->                            410;
status('Length Required') ->                 411;
status('Precondition Failed') ->             412;
status('Request Entity Too Large') ->        413;
status('Request-URI Too Long') ->            414;
status('Unsupported Media Type') ->          415;
status('Requested Range Not Satisfiable') -> 416;
status('Expectation Failed') ->              417;
status('I\'m a teapot') ->                   418;
status('Unprocessable Entity') ->            422;
status('Locked') ->                          423;
status('Failed Dependency') ->               424;
status('Unordered Collection') ->            425;
status('Upgrade Required') ->                426;
status('Precondition Required') ->           428;
status('Too Many Requests') ->               429;
status('Request Header Fields Too Large') -> 431;
status('Internal Server Error') ->           500;
status('Not Implemented') ->                 501;
status('Bad Gateway') ->                     502;
status('Service Unavailable') ->             503;
status('Gateway Timeout') ->                 504;
status('HTTP Version Not Supported') ->      505;
status('Variant Also Negotiates') ->         506;
status('Insufficient Storage') ->            507;
status('Not Extended') ->                    510;
status('Network Authentication Required') -> 511.


-ifdef(TEST).
	-include_lib("eunit/include/eunit.hrl").

	accept_test() ->
		ok.

	accept_invalid_test() ->
		ok.

	accept_weight_test() ->
		ok.

	accept_catch_all_test() ->
		ok.

-endif().
