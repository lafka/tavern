-module(tavern_req).

%% HTTP Request data retrieval API
-export([content_type/1, content_type_charset/1,
	accepts/1, accepts_charset/1, accepts_language/1]).

%% HTTP Request validation API
-export([exposed_method/2, client_acceptable/2, authorized/2, consumed_type/2]).

%% HTTP Request validation cycle
-export([call/3, call/4]).

-compile([export_all]).

-include("rest.hrl").
-include_lib("cowboy/include/http.hrl").

-spec accepts(#http_req{}) -> {binary(), binary(), [{binary(), binary()}]}.
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

accepts_charset(Req) ->
	{Val, Req} = cowboy_http_req:header(<<"Accept-Charset">>, <<"utf-8">>),
	Val.

accepts_language(Req) ->
	{Val, Req} = cowboy_http_req:header(<<"Content-Language">>, <<"en-US">>),
	Val.

-spec content_type(#http_req{}) -> mime().
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

authorized(Req, State) ->
	{true, Req, State, fun exposed_method/2}.

%% Validation of request
client_acceptable(Req, #tavern{provides = AcceptTypes} = State) ->
	AcceptFilter = fun(A) -> not ([] == match_media_type(A, AcceptTypes)) end,
	case lists:filter(AcceptFilter, [{A, B} || {A, B, _} <- accepts(Req)]) of
		[Type | _] ->
			{true, Req, State#tavern{accept = Type}, fun exposed_method/2};
		[] ->
			{Val, Req} = cowboy_http_req:header(<<"Accept">>, Req, <<"missing valid \"Accept\" header">>),
			{ {'Not Acceptable',
				[{error, [{message, <<"no supported accept types given">>},
				          {param,   Val}]}]}
			, Req
			, State#tavern{accept = {<<"text">>, <<"plain">>}}}
	end.

exposed_method(Req, #tavern{methods = []} = State) ->
	{true, Req, State, fun consumed_type/2};

exposed_method(Req, #tavern{methods = Methods} = State) ->
	{HTTPMethod, Req} = cowboy_http_req:method(Req),
	case lists:member(HTTPMethod, Methods) of
		true ->
			{true, Req, State, fun consumed_type/2};
		_    ->
			{ { 'Method Not Allowed',
				[{error, [{message, <<"unsupported HTTP method provided">>},
				          {param,   HTTPMethod}]}]}
			, Req
			, State}
	end.

consumed_type(Req, State) ->
	case cowboy_http_req:has_body(Req) of
		{true, Req}  ->
			#tavern{consumes = TypeList} = State,
			{Mime1, Mime2, Charset} = content_type(Req),
			case match_media_type({Mime1, Mime2, Charset}, TypeList) of
				[] ->
					{ {'Not Acceptable', [
						{error, [{'message',      <<"content type not understood">>},
						         {'param',        <<Mime1/binary, $/, Mime2>>}]}]}
					, Req
					, State};
				_ ->
					call(true, Req,
						  State#tavern{
							  content_type = <<Mime1, $/, Mime2>>
							, charset      = Charset}
						, fun decode_body/2)
			end;
		{false, Req} -> {true, Req, State, success}
	end.

%%encode_body_to_charset(Req, #tavern{charset = Charset} = State) ->
%%	{ok, Body, Req} = cowboy_http_req:body(2000, Req),
%%	encode_body_to_charset(Req, State, iso1_bin_to_lower(Charset), <<"utf8">>);
%%
%%encode_body_to_charset(Req, State, From, To) when From == To ->
%%	{true, Req, State, success};
%%
%%encode_body_to_charset(Req, State, From, To) ->
%%encode_body_to_charset(Req, State, From, <<"iso-8859-1">>) ->
%%encode_body_to_charset(Req, State, From, <<"utf8">>) ->
%%	{true, Req, State#tavern{body = }
%%encode_body_to_charset(Req, State) ->
%%	{true, Req, State}.

decode_body(Req, #tavern{content_type = ContentType} = State) ->
	{ok, Binary} = cowboy_http_req:body(1500, Req),
	Payload = tavern_marshal:decode(ContentType, Binary),
	{true, Req, State#tavern{body = Payload}, success}.

call(Req, State, Fun) when is_function(Fun) ->
	call(Req, State, Fun, success).

call(Req, State, Fun, Next) when is_function(Fun) ->
	case Fun(Req, State) of
		{true, Req2, State2, success} ->
			{true, Req2, State2};
		{true, Req2, State2, Next}    ->
			call(Req2, State2, Next);
		{{_, _} = Result, Req2, State2} ->
			{Result, Req2, State2}
	end.

%% Private API
%%-spec extract_charset(binary(), binary()) -> {Mime :: binary(), Charset :: binary()}.
%%extract_charset(<<Payload/binary>>, Default) ->
%%	try
%%		{P, _} = binary:match(Payload, <<$;>>),
%%		<<Payload2:P/binary, _:8, Charset/binary>> = Payload,
%%		[_ | [Charset2]] = [A || A <- binary:split(Charset, [<<" ">>, <<$=>>, <<$;>>], [trim, global]), A =/= <<>>],
%%		{Payload2, Charset2}
%%	catch error:{badmatch,nomatch} -> Default end.

-spec sanitize_media_header(Header :: binary()) -> binary().
sanitize_media_header(Header) ->
	<< <<C>> || <<C>> <= Header, (C > 32), (C < 127)>>.

%% Validate mime predicate for a prioritized mime list.
-spec match_media_type(mime_charset(), [mime_charset()]) -> [] | [mime_charset()] | [].
match_media_type({T1, T2, _}, List) ->
	match_media_type({T1, T2}, List);

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

latin1_bin_to_lower(Bin) ->
	F = fun(Char) when Char >= 65 andalso Char =< 90 -> Char + 32; (Char) -> Char end,
	<< <<(F(C))>> || <<C>> <= Bin>>.

latin1_bin_to_upper(Bin) ->
	F = fun(Char) when Char >= 90 andalso Char =< 122 -> Char - 32; (Char) -> Char end,
	<< <<(F(C))>> || <<C>> <= Bin>>.

-ifdef(TEST).
	match_wildcard_media_type_test() ->
		AcceptList = [ {<<"text">>,       <<"html">>,[]}
		             , {<<"application">>,<<"xml">>, []}
		             , {<<"application">>,<<"json">>,[]}],
		[{<<"application">>,<<"xml">>, _}] = match_media_type(
			{<<$*>>, <<"xml">>, []}, AcceptList),
		[{<<"application">>,<<"xml">>, _}] = match_media_type(
			{<<"application">>, <<$*>>, []}, AcceptList),
		[{<<"application">>,<<"xml">>, _}] = match_media_type(
			{<<"application">>, <<"xml">>, []}, AcceptList),
		[{<<"application">>,<<"xml">>, _}] = match_media_type(
			{<<$*>>,<<$*>>, []}, AcceptList),
		[] = match_media_type({<<"abc">>, <<"123">, []}, AcceptList).
-endif().
