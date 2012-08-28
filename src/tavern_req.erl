-module(tavern_req).

%% HTTP Request data retrieval API
-export([content_type/1, content_type_charset/1,
	accepts/1, accepts_charset/1, accepts_language/1]).

%% HTTP Request validation cycle
-export([validate_req/2]).

-include("rest.hrl").
-include_lib("cowboy/include/http.hrl").

-type resp() :: { { Status  :: tavern_http:returnstatus()
                  , Payload :: tavern_http:tree()}
                , Req   :: #http_req{}
                , State :: #tavern{}}.
-type continue() :: { true
                    , Req        :: #http_req{}
                    , State      :: #tavern{}
                    , NextAction :: function() | success}.

%% Public request data API
-spec accepts(#http_req{}) -> [{binary(), binary(), float()}].
accepts(Req) ->
	{Val, Req} = cowboy_http_req:header('Accept', Req, <<"text/plain">>),
	AcceptList = lists:map(fun(Seg) ->
		{T1, T2, P} = cowboy_http:content_type(Seg),
		case lists:takewhile(fun({<<"q">>, _}) -> true; (_) -> false end, P) of
			[{_, Q} | _] -> {T1, T2, list_to_float(binary_to_list(Q))};
			_            -> {T1, T2, 1.0}
		end
	end, binary:split(sanitize_media_header(Val), [<<$,>>],[global])),
	lists:sort(fun({_, _, P1}, {_, _, P2}) -> P1 =< P2 end, AcceptList).

-spec accepts_charset(#http_req{}) -> binary().
accepts_charset(Req) ->
	{Val, Req} = cowboy_http_req:header(<<"Accept-Charset">>, <<"utf-8">>),
	Val.

-spec accepts_language(#http_req{}) -> binary().
accepts_language(Req) ->
	{Val, Req} = cowboy_http_req:header(<<"Content-Language">>, <<"en-US">>),
	Val.

-spec content_type(#http_req{}) -> tavern_http:mime_options().
content_type(Req) ->
	{'Content-Type', V} = cowboy_http_req:header('Content-Type', Req, <<"text/plain; charset=utf8">>),
	cowboy_http:content_type(V).

-spec content_type_charset(#http_req{}) -> binary().
content_type_charset(Req) ->
	{_ContentType, _ContentType2, CParams} = content_type(Req),
	case [ B || {<<"charset">>, B} <- CParams] of
		[Charset] -> Charset;
		[]        -> ?DEFAULT_CHARSET
	end.

%% Private request validation
-spec authorized(Req :: #http_req{}, State :: #tavern{}) -> continue().
authorized(Req, State) ->
	{true, Req, State, fun client_acceptable/2}.

-spec client_acceptable(Req :: #http_req{}, State:: #tavern{}) -> continue() | resp().
client_acceptable(Req, #tavern{content_types_provided = AcceptTypes} = State) ->
	AcceptFilter = fun(A) -> not ([] == match_media_type(A, AcceptTypes)) end,
	case lists:filter(AcceptFilter, [{A, B} || {A, B, _} <- accepts(Req)]) of
		[Type | _] ->
			{true, Req, State#tavern{accept = Type}, fun exposed_method/2};
		[] ->
			{Val, Req} = cowboy_http_req:header(<<"Accept">>, Req, <<"missing valid \"Accept\" header">>),
			{ {'Not Acceptable'
			, [{error, [ {code, 405}
			           , {message, <<"no supported accept types given">>}
			           , {param,   Val}]}]}
			, Req
			, State#tavern{accept = {<<"text">>, <<"plain">>}}}
	end.


-spec exposed_method(Req :: #http_req{}, State :: #tavern{}) -> continue() | resp().
exposed_method(Req, #tavern{allowed_methods= []} = State) ->
	{true, Req, State, fun consumed_type/2};

exposed_method(Req, #tavern{allowed_methods = Methods} = State) ->
	{HTTPMethod, Req} = cowboy_http_req:method(Req),
	case lists:member(HTTPMethod, Methods) of
		true ->
			{true, Req, State, fun consumed_type/2};
		_    ->
			{ { 'Method Not Allowed'
			, [{error, [ {code, 405}
			           , {message, <<"unsupported HTTP method provided">>}
			           , {param,   HTTPMethod}]}]}
			, Req
			, State}
	end.

-spec consumed_type(Req :: #http_req{}, State :: #tavern{}) -> continue() | resp().
consumed_type(Req, State) ->
	case cowboy_http_req:has_body(Req) of
		{true, Req}  ->
			#tavern{content_types_accepted = TypeList} = State,
			{Mime1, Mime2, _} = content_type(Req),
			Charset = content_type_charset(Req),
			case match_media_type({Mime1, Mime2}, TypeList) of
				[] ->
					{ {'Not Acceptable'
					, [ {error, [ {code, 406}
					            , {'message',      <<"content type not understood">>}
					            , {'param',        <<Mime1/binary, $/, Mime2/binary>>}]}]}
					, Req
					, State};
				_ ->
					call( Req
						, State#tavern{ content_type = {Mime1, Mime2}
						              , charset      = Charset}
						, fun decode_body/2)
			end;
		{false, Req} -> {true, Req, State, success}
	end.

-spec decode_body(Req :: #http_req{}, State :: #tavern{}) -> continue() | resp().
decode_body(Req, #tavern{content_type = ContentType} = State) ->
	{ok, Binary}  = cowboy_http_req:body(1500, Req),
	{ok, Payload} = tavern_marshal:decode(ContentType, Binary),
	{true, Req, State#tavern{body = Payload}, success}.

-spec validate_req(Req :: #http_req{}, State :: #tavern{}) ->
	{true, #http_req{}, #tavern{}} |
	{{tavern_http:return_status(), tavern_http:tree()}, #http_req{}, #tavern{}}.
validate_req(Req, State) ->
	call(Req, State, fun authorized/2).

-spec call(Req :: #http_req{}, State :: #tavern{}, Callback :: function()) ->
	{true, #http_req{}, #tavern{}} |
	{{tavern_http:return_status(), tavern_http:tree()}, #http_req{}, #tavern{}}.
call(Req, State, Fun) when is_function(Fun) ->
	case Fun(Req, State) of
		{true, Req2, State2, success} ->
			{true, Req2, State2};
		{true, Req2, State2, Next}    ->
			call(Req2, State2, Next);
		{{Status, Payload}, Req2, State2} ->
			{{Status, Payload}, Req2, State2}
	end.

%% Internal helper functions

-spec sanitize_media_header(Header :: binary()) -> binary().
sanitize_media_header(Header) ->
	<< <<C>> || <<C>> <= Header, (C > 32), (C < 127)>>.

%% Validate mime predicate for a prioritized mime list.
-spec match_media_type(tavern_http:mime(), [tavern_http:mime_charset()]) -> [] | [tavern_http:mime_charset()].
match_media_type(Mime, []) ->
	[Mime];

match_media_type({<<$*>>, <<$*>>}, [Item | _]) ->
	[Item];

match_media_type({T1, <<$*>>}, List) ->
	case lists:keyfind(T1, 1, List) of
		false -> [];
		A     -> [A]
	end;

match_media_type({<<$*>>, T2}, List) ->
	case lists:keyfind(T2, 2, List) of
		false -> [];
		A     -> [A]
	end;

match_media_type({T1, T2}, List) ->
	Fun = fun({A, B, _}) when A == T1 andalso B == T2 -> true; (_) -> false end,
	case lists:filter(Fun, List) of
		[]      -> [];
		[A | _] -> [A]
	end.


-ifdef(TEST).
	match_wildcard_media_type_test() ->
		AcceptList = [ {<<"text">>,       <<"html">>,[]}
		             , {<<"application">>,<<"xml">>, []}
		             , {<<"application">>,<<"json">>,[]}],
		[{<<"application">>,<<"xml">>}] = match_media_type(
			{<<$*>>, <<"xml">>, []}, AcceptList),
		[{<<"application">>,<<"xml">>, _}] = match_media_type(
			{<<"application">>, <<$*>>}, AcceptList),
		[{<<"application">>,<<"xml">>, _}] = match_media_type(
			{<<"application">>, <<"xml">>}, AcceptList),
		[{<<"application">>,<<"xml">>, _}] = match_media_type(
			{<<$*>>,<<$*>>}, AcceptList),
		[] = match_media_type({<<"abc">>, <<"123">, []}, AcceptList).
-endif().
