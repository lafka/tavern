-module(tavern_marshal).

-export([decode/2, encode/2]).

-include("rest.hrl").

-spec decode(Mime :: tavern_http:mime() | binary(), Encoded :: iolist()) -> Payload :: {ok, tavern_http:tree()}.
decode(Mime, Payload) ->
	Module = map_mime(Mime),
	{ok, _} = Module:decode(Payload).

-spec encode(Mime :: tavern_http:mime() | binary(), Payload :: tavern_http:tree()) -> {ok, Encoded :: iolist()}.
encode(_, Payload) when Payload == []; Payload == <<>>; Payload == {} ->
	{ok, <<>>};

encode(Mime, Payload) ->
	Module = map_mime(Mime),
	{ok, _} = Module:encode(Payload).

-spec map_mime(Mime :: binary() | tavern_http:mime()) -> tavern_marshal_xml | tavern_marshal_json | tavern_marshal_html | tavern_marshal_plain.
map_mime({<<Mime1/binary>>, <<Mime2/binary>>}) -> map_mime(<<Mime1/binary, $/, Mime2/binary>>);
map_mime(<<"application/xml">>)    -> tavern_marshal_xml;
map_mime(<<"application/json">>)   -> tavern_marshal_json;
map_mime(<<"text/html">>)          -> tavern_marshal_html;
map_mime(<<"text/plain">>)         -> tavern_marshal_plain.

-ifdef(TEST).
	-include_lib("eunit/include/eunit.hrl").

	decode_mimes_test_() ->
		Match = [ {payload, [{unique_id, <<"123">>}, {system_id, <<"456">>}]}
		        , {meta, [{timestamp, <<"123456789">>}]}],
		%% Test data for xml,json, there is no support for text/{html,plain
		Content = fun (<<"application/xml">>) ->
				<<"<?xml version=\"1.0\"?>\n"
				  "<root><payload>"
				  "<unique_id>123</unique_id>" "<system_id>456</system_id>"
				  "</payload>"
				  "<meta>"
				  "<timestamp>1234567879</timestamp"
				  "</meta></root>">>;
			(<<"application/json">>) ->
				<<"{\"payload\": {\"unique_id\":\"123\", \"system_id\":\"456\"},"
				  " \"meta\":{\"timestamp\":\"123456789\"}}">>
		end,
		lists:map(fun (A) -> ?_assertEquals(Content(A), Match) end, mimetypes()).

	map_empty_mime_test() ->
		tavern_marshal_plain = map_mime([]).

	map_atom_mime_test() ->
		tavern_marshal_json = map_mime('application/json').

-endif().
