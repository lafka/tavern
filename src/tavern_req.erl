-module(tavern_req).

%% HTTP Request data retrieval API
-export([content_type/1, content_type_charset/1, content_language/1,
	accepts/1, accept_charset/1, accept_language/1]).

%% HTTP Request validation cycle
-export([validate_req/2]).

-include("rest.hrl").
-include_lib("eunit/include/eunit.hrl").

-type resp() :: { { Status  :: tavern_http:returnstatus()
                  , Payload :: tavern_http:tree()}
                , Req   :: cowboy_http:req()
                , State :: #tavern{}}.
-type continue() :: { true
                    , Req        :: cowboy_http:req()
                    , State      :: #tavern{}
                    , NextAction :: function() | success}.
-define(defaultmime, <<"application/json">>).
-define(defaultcharset, <<"utf-8">>).
-define(defaultlang, <<"en-GB">>).

%% Public request data API
-spec accepts(cowboy_http:req()) -> [{binary(), binary(), float()}].
accepts(Req) ->
	{Val, Req} = cowboy_req:header(<<"Accept">>, Req, ?defaultmime),
	AcceptList = lists:map(fun(Seg) ->
		{T1, T2, P} = cowboy_http:content_type(Seg),
		case lists:takewhile(fun({<<"q">>, _}) -> true; (_) -> false end, P) of
			[{_, Q} | _] -> {T1, T2, list_to_float(binary_to_list(Q))};
			_            -> {T1, T2, 1.0}
		end
	end, binary:split(sanitize_media_header(Val), [<<$,>>],[global])),
	lists:sort(fun({_, _, P1}, {_, _, P2}) -> P1 >= P2 end, AcceptList).

-spec accept_charset(cowboy_http:req()) -> binary().
accept_charset(Req) ->
	{Val, Req} = cowboy_req:header(<<"accept-charset">>, Req, ?defaultcharset),
	Val.

-spec accept_language(cowboy_http:req()) -> binary().
accept_language(Req) ->
	{Val, Req} = cowboy_req:header(<<"accept-language">>, Req, ?defaultlang),
	Val.
-spec content_language(cowboy_http:req()) -> binary().
content_language(Req) ->
	{Val, Req} = cowboy_req:header(<<"content-language">>, Req, ?defaultlang),
	Val.

-spec content_type(cowboy_http:req()) -> tavern_http:mime_options().
content_type(Req) ->
	{Val, Req} = cowboy_req:header(<<"content-type">>, Req, ?defaultmime),
	cowboy_http:content_type(Val).

-spec content_type_charset(cowboy_http:req()) -> binary().
content_type_charset(Req) ->
	{_ContentType, _ContentType2, CParams} = content_type(Req),
	case [ B || {<<"charset">>, B} <- CParams] of
		[Charset] -> Charset;
		[]        -> ?DEFAULT_CHARSET
	end.

%% Private request validation
-spec authorized(Req :: cowboy_http:req(), State :: #tavern{}) -> continue().
authorized(Req, State) ->
	{true, Req, State, fun client_acceptable/2}.

-spec client_acceptable(Req :: cowboy_http:req(), State:: #tavern{}) -> continue() | resp().
client_acceptable(Req, #tavern{content_types_provided = AcceptTypes} = State) ->
	AcceptFilter = fun(A) -> not ([] == match_media_type(A, AcceptTypes)) end,
	case lists:filter(AcceptFilter, [{A, B} || {A, B, _} <- accepts(Req)]) of
		[Type | _] ->
			{true, Req, State#tavern{accept = Type}, fun exposed_method/2};
		[] ->
			{Val, Req} = cowboy_req:header(<<"accept">>, Req, <<"missing valid \"Accept\" header">>),
			{ {'Not Acceptable'
			, [{error, [ {code, 405}
			           , {message, <<"no supported accept types given">>}
			           , {param,   Val}]}]}
			, Req
			, State#tavern{accept = {<<"text">>, <<"plain">>}}}
	end.


-spec exposed_method(Req :: cowboy_http:req(), State :: #tavern{}) -> continue() | resp().
exposed_method(Req, #tavern{allowed_methods= []} = State) ->
	{true, Req, State, fun consumed_type/2};

exposed_method(Req, #tavern{allowed_methods = Methods} = State) ->
	{HTTPMethod, Req} = cowboy_req:method(Req),
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

-spec consumed_type(Req :: cowboy_http:req(), State :: #tavern{}) -> continue() | resp().
consumed_type(Req, State) ->
	case cowboy_req:has_body(Req) of
		true  ->
			#tavern{content_types_accepted = TypeList} = State,
			{Mime1, Mime2, _} = content_type(Req),
			Charset = content_type_charset(Req),
			case match_media_type({Mime1, Mime2}, TypeList) of
				[] ->
					{ {'Unsupported Media Type'
					, [ {error, [ {code, 415}
					            , {'message',      <<"content type not allowed">>}
					            , {'param',        <<Mime1/binary, $/, Mime2/binary>>}]}]}
					, Req
					, State};
				_ ->
					{true, Req, State#tavern{ content_type = {Mime1, Mime2}
						              , charset      = Charset}, fun decode_body/2}
			end;
		false -> {true, Req, State, success}
	end.

-spec decode_body(Req :: cowboy_http:req(), State :: #tavern{}) -> continue() | resp().
decode_body(Req, #tavern{content_type = ContentType} = State) ->
	{ok, Binary, Req2} = cowboy_req:body(Req),
	case tavern_marshal:decode(ContentType, Binary) of
		{ok, Payload} ->
			{true, Req2, State#tavern{body = Payload}, success};
		{error, Err} ->
			ErrBin = atom_to_binary(Err, utf8),
			{ {'Unsupported Media Type'
			, [ {error, [ {code, 1003}
			            , {'message',      <<"(error #1003) ", ErrBin/binary>>}]}]}
			, Req2
			, State}
	end.

-spec validate_req(Req :: cowboy_http:req(), State :: #tavern{}) ->
	{true, cowboy_http:req(), #tavern{}} |
	{{tavern_http:return_status(), tavern_http:tree()}, cowboy_http:req(), #tavern{}}.
validate_req(Req, State) ->
	call(Req, State, fun authorized/2).

-spec call(Req :: cowboy_http:req(), State :: #tavern{}, Callback :: function()) ->
	{true, cowboy_http:req(), #tavern{}} |
	{{tavern_http:return_status(), tavern_http:tree()}, cowboy_http:req(), #tavern{}}.
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

match_media_type({A, B, _}, L) ->
	match_media_type({A, B}, L);
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
	-define(HTTP_REQ(Headers), cowboy_req:new(undefined, undefined, <<"GET">>, <<$/>>
		, <<"">>, <<"">>, {1, 1}, Headers, <<"localhost">>, 8080, <<"">>
		, true, undefined)).

	accepts_test() ->
		Req = ?HTTP_REQ([{<<"accept">>, <<"application/xml, application/json">>}]),
		?assertEqual(
			[{<<"application">>,<<"xml">>,1.0},{<<"application">>,<<"json">>,1.0}],
			accepts(Req)).

	accept_charset_test() ->
		Req = ?HTTP_REQ([{<<"accept">>, <<"application/xml, application/json">>}]),
		?assertEqual(
			[{<<"application">>,<<"xml">>,1.0},{<<"application">>,<<"json">>,1.0}],
			accepts(Req)).

	accept_language_test() ->
		?assertEqual(ok, ok).
	content_language_test() ->
		?assertEqual(ok, ok).
	content_type_charset_test() ->
		?assertEqual(ok, ok).
	content_type_test() ->
		?assertEqual(ok, ok).
	authorized_test() ->
		?assertEqual(ok, ok).
	client_acceptable_test() ->
		?assertEqual(ok, ok).
	exposed_method_test() ->
		?assertEqual(ok, ok).
	consumed_type_test() ->
		?assertEqual(ok, ok).
	decode_body_test() ->
		?assertEqual(ok, ok).

	match_wildcard_media_type_test() ->
		AcceptList = [ {<<"text">>,       <<"html">>,[]}
		             , {<<"application">>,<<"xml">>, []}
		             , {<<"application">>,<<"json">>,[]}],
		?assertEqual([{<<"application">>,<<"xml">>,  []}], match_media_type({<<$*>>, <<"xml">>}, AcceptList)),
		?assertEqual([{<<"application">>,<<"xml">>,  []}], match_media_type({<<"application">>, <<$*>>}, AcceptList)),
		?assertEqual([{<<"application">>,<<"xml">>,  []}], match_media_type({<<"application">>, <<"xml">>}, AcceptList)),
		?assertEqual([{<<"text">>,<<"html">>,        []}], match_media_type({<<$*>>,<<$*>>}, AcceptList)),
		?assertEqual([], match_media_type({<<"abc">>, <<"123">>, []}, AcceptList)).
-endif.
