-module(tavern_marshal).

-export([decode/2, encode/2]).

-include_lib("eunit/include/eunit.hrl").
-include("rest.hrl").

-spec decode(Mime :: tavern_http:mime() | binary(), Encoded :: iolist() | binary()) -> Payload :: {ok, tavern_http:tree()}.
decode(Mime, Payload) ->
	Module = map_mime(Mime),
	Module:decode(Payload).

-spec encode(Mime :: tavern_http:mime() | binary(), Payload :: tavern_http:tree()) -> {ok, Encoded :: iolist()}.
encode(_, Payload) when Payload == []; Payload == <<>>; Payload == {} ->
	{ok, <<>>};

encode(Mime, Payload) ->
	Module = map_mime(Mime),
	Module:encode(Payload).

-spec map_mime(Mime :: binary() | tavern_http:mime()) -> tavern_marshal_xml | tavern_marshal_json | tavern_marshal_html | tavern_marshal_plain.
map_mime({A,B,_}) -> map_mime({A,B});
map_mime({<<Mime1/binary>>, <<Mime2/binary>>}) -> map_mime(<<Mime1/binary, $/, Mime2/binary>>);
map_mime(<<"application/xml">>)    -> tavern_marshal_xml;
map_mime(<<"application/json">>)   -> tavern_marshal_json;
map_mime(<<"text/html">>)          -> tavern_marshal_html;
map_mime(A) when is_binary(A)      -> tavern_marshal_plain.

-ifdef(TEST).
	-define(TREE, [{<<"root">>, [{<<"child">>, <<"value">>}]}]).

	map_empty_mime_test() ->
		tavern_marshal_plain = map_mime(<<>>).

	map_atom_mime_test() ->
		tavern_marshal_json = map_mime(<<"application/json">>),
		tavern_marshal_json = map_mime({<<"application">>, <<"json">>}).

	encode_xml_tree_test() ->
		{ok, EncXML}  = encode({<<"application">>, <<"xml">>}, ?TREE),
		{ok, DecXML}  = decode({<<"application">>, <<"xml">>},  lists:flatten(EncXML)),
		?assertEqual(?TREE, DecXML).

	encode_json_tree_test() ->
		{ok, EncJSON} = encode({<<"application">>, <<"json">>}, ?TREE),
		{ok, DecJSON} = decode({<<"application">>, <<"json">>}, iolist_to_binary(EncJSON)),
		?assertEqual(?TREE, DecJSON).

	encode_invalid_data_test() ->
		{error, _} = decode({<<"application">>, <<"json">>}, <<"invalid[data">>),
		{error, _} = decode({<<"application">>, <<"xml">>},  <<"invalid#>data">>).

-endif.
