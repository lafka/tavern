-include_lib("tavern/include/rest.hrl").
-include_lib("cowboy/include/http.hrl").

-export([init/3, handle/2, terminate/2, handle_default_options/2]).

-spec init(Transport :: module(), Req :: #http_req{}, list()) -> {ok, #http_req{}, #tavern{}}.
init(Transport, Req, _Opts) ->
	tavern_http:init(Transport, Req, [?MODULE]).

-spec handle(#http_req{}, #tavern{})-> {ok, #http_req{}, #tavern{}}.
handle(Req, State) ->
	tavern_http:handle(?MODULE, Req, State).

-spec terminate(#http_req{}, #tavern{})-> ok.
terminate(Req, State) ->
	tavern_http:terminate(?MODULE, Req, State).

-spec handle_default_options(_, #tavern{}) -> {Status :: atom(), _, #tavern{}, binary()}.
handle_default_options(Req, #tavern{allowed_methods = Methods} = State) ->
	F = fun(X, <<>>) -> atom_to_binary(X, utf8);
		(X, Acc) -> X2 = atom_to_binary(X, utf8),
		<<Acc/binary, $,, X2/binary>> end,
	{ok, Req2} = cowboy_http_req:set_resp_header('Allow', lists:foldl(F, <<>>, Methods), Req),
	{'No Content', Req2, State, <<>>}.

