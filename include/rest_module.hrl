-include_lib("tavern/include/rest.hrl").

-export([init/3, handle/2, terminate/2, handle_default_options/2]).

init(Transport, Req, _Opts) ->
	tavern_http:init(Transport, Req, [?MODULE]).

handle(Req, State) ->
	tavern_http:handle(?MODULE, Req, State).

terminate(Req, State) ->
	tavern_http:terminate(?MODULE, Req, State).

handle_default_options(Req, #tavern{methods = Methods} = State) ->
	F = fun(X, <<>>) -> atom_to_binary(X, utf8);
		(X, Acc) -> X2 = atom_to_binary(X, utf8),
		<<Acc/binary, $,, X2/binary>> end,
	{ok, Req2} = cowboy_http_req:set_resp_header('Allow', lists:foldl(F, <<>>, Methods), Req),
	{'Ok', Req2, State, <<>>}.

