-module(tavern_http).

-include("rest.hrl").

%% HTTP parse callbacks
-export([init/3, handle/3, terminate/3, status/1]).

-type key()             :: atom() | binary().
-type value()           :: binary() | number() | string() | tree().
-type tree()            :: [{key(), value()}].
-type request_method()  :: atom().
-type mime()            :: {binary(), binary()}.
-type mime_options()    :: {binary(), binary(), [{binary(), binary()}]}.
-type mime_charset()    :: {binary(), binary(), binary()}.
-type returnstatus()    :: http_int_status() | atom().
-type http_int_status() :: 100..599.
-type return()          :: {returnstatus(), cowboy_http:req(), #tavern{}, tree()}.

-define(DEFAULT_HANDLER,
	  {allowed_methods,  [<<"HEAD">>, <<"GET">>, <<"OPTIONS">>]}
	, {method_handlers, [
		  {<<"HEAD">>,    handle_head}
		, {<<"GET">>,     handle_get}
		, {<<"POST">>,    handle_post}
		, {<<"PUT">>,     handle_put}
		, {<<"DELETE">>,  handle_delete}
		, {<<"PATCH">>,   handle_patch}
		, {<<"OPTIONS">>, handle_default_options}
	]}
	, {content_types_provided, [
		  {<<"text">>,       <<"html">>, []}
		, {<<"application">>,<<"xml">>,  []}
		, {<<"application">>,<<"json">>, []}
	]}
	, {content_types_accepted, [
		  {<<"text">>,        <<"html">>, []}
		, {<<"application">>, <<"xml">>,  []}
		, {<<"application">>, <<"json">>, []}
	]}
).

-export_type([mime/0, tree/0, mime_charset/0, request_method/0, returnstatus/0,
	mime_options/0, return/0]).

-spec init(Transport :: module(), Req :: cowboy_http:req(), [module()]) -> {ok, cowboy_http:req(), #tavern{}}.
init(_Transport, Req, [Handler]) ->
	Defaults = [?DEFAULT_HANDLER],
	Fun = fun({Key, Val}, {InnerReq, State}) ->
		case erlang:function_exported(Handler, Key, 2) of
			true  ->
				{NewVal, InnerReq2, State2} = Handler:Key(InnerReq, State),
				{InnerReq2, tavern_lens(Key, NewVal, State2)};
			false ->
				{InnerReq, tavern_lens(Key, Val, State)}
		end
	end,
	{Req2, State} = lists:foldl( Fun, {Req, #tavern{}}, Defaults),
	Req3 = cowboy_req:set_resp_header(<<"Allow">>
		, lists:foldl(fun(A,<<>>) -> A; (A,B) -> <<A/binary, $,, B/binary>> end
			, <<>>
			, State#tavern.allowed_methods)
		, Req2),
	case erlang:function_exported(Handler, init, 2) of
		true  -> Handler:init(Req3, State#tavern{module = Handler});
		false -> {ok, Req3, State#tavern{module = Handler}}
	end.

tavern_lens(allowed_methods, V, R)        -> R#tavern{allowed_methods  = V};
tavern_lens(method_handlers, V, R)        -> R#tavern{method_handlers  = V};
tavern_lens(content_types_provided, V, R) -> R#tavern{content_types_provided = V};
tavern_lens(content_types_accepted, V, R) -> R#tavern{content_types_accepted = V}.

-spec handle(Handler :: module(), cowboy_http:req(), #tavern{})-> {ok, cowboy_http:req(), #tavern{}}.
handle(Module, Req, #tavern{} = State) ->
	case tavern_req:validate_req(Req, State) of
		{true, Req2, State2} ->
			case handle_call(Module, Req2, State2) of
				{Status, NewReq, NewState, Payload} ->
					handle_resp(NewReq, NewState, Status, Payload);
				ok ->
					{ok, Req2, State2}
			end;
		{{Status, Payload}, NewReq, NewState} ->
			handle_resp(NewReq, NewState, Status, Payload)
	end.

-spec terminate(Handler :: module(), cowboy_http:req(), #tavern{})-> ok.
terminate(_Handler, _Req, _State) ->
	ok.

-spec handle_call(Handler :: module(), cowboy_http:req(), #tavern{})-> {returnstatus(), cowboy_http:req(), #tavern{}, tree()}.
handle_call(_, Req, #tavern{method_handlers = []} = State) ->
	{'Internal Server Error', Req, State, [{error, [
		  {code,    1002}
		, {message, <<"(error #1002) no request handler found">>}
	]}]};

handle_call(Module, Req, #tavern{method_handlers = Handlers} = State) ->
	try
		{Method, Req}     = cowboy_req:method(Req),
		{Method, Handler} = lists:keyfind(Method, 1, Handlers),
		case erlang:function_exported(Module, Handler, 2) of
			true  -> Module:Handler(Req, State);
			false -> erlang:error({no_export, Module, Handler})
		end
	catch Class:Reason ->
		error_logger:error_msg(eunit_lib:format_exception({Class,Reason,erlang:get_stacktrace()})),
		{'Internal Server Error', Req, State, [{error, [
			  {code,    1000}
			, {message, <<"(error #1000) an unexpected error occured">>}
		]}]}
	end.

-spec handle_resp(cowboy_http:req(), #tavern{}, Status :: returnstatus(), Result :: tree())-> {ok, cowboy_http:req(), #tavern{}}.
handle_resp(Req, State, _Status, []) ->
	handle_resp(Req, State, 'Internal Server Error', [{error, [
		  {code,    1001}
		, {message, <<"(error #1001) empty response">>}
	]}]);

handle_resp(Req, State, Status, Payload) when is_atom(Status) ->
	handle_resp(Req, State, status(Status), Payload);

handle_resp(Req, #tavern{accept = {A, B, EncFun}} = State, Status, Payload) ->
	try
		{ok, EncodedPayload} = EncFun(Req, State, Payload),
		{ok, Resp} = cowboy_req:reply(Status, [], EncodedPayload, Req),
		{ok, Resp, State}
	catch _Class:_Reason ->
		ErrBody = case EncFun(Req, State, [{error, [
			  {code, 1003}
			, {message, <<"(error #1003) could not serialize content">>}
		]}]) of
			{ok, Body} ->
				Body;
			{error, Err} ->
				error_logger:warning_msg("could not serialize '~s/~s': ~p, ~p"
					, [A, B, Err, Payload]),
				<<>>
			end,
		{ok, ErrResp} = cowboy_req:reply(400, [], ErrBody, Req),
		{ok, ErrResp, State}
	end.

-spec status(atom()) -> http_int_status().
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

-endif.
