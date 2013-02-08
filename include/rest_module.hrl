-include_lib("tavern/include/rest.hrl").

-export([init/3, handle/2, terminate/3, handle_default_options/2]).

-spec init(Transport :: module(), Req :: cowboy_req:req(), list()) -> {ok, cowboy_req:req(), #tavern{}}.
init(Transport, Req, _Opts) ->
	tavern_http:init(Transport, Req, [?MODULE]).

-spec handle(cowboy_req:req(), #tavern{})-> {ok, cowboy_req:req(), #tavern{}}.
handle(Req, State) ->
	tavern_http:handle(?MODULE, Req, State).

-spec terminate(atom(), cowboy_req:req(), #tavern{})-> ok.
terminate(_Reason, Req, State) ->
	tavern_http:terminate(?MODULE, Req, State).

-spec handle_default_options(_, #tavern{}) -> {Status :: atom(), _, #tavern{}, binary()}.
handle_default_options(Req, #tavern{allowed_methods = Methods} = State) ->
	F = fun(X, <<>>) -> atom_to_binary(X, utf8);
		(X, Acc) -> X2 = atom_to_binary(X, utf8),
		<<Acc/binary, $,, X2/binary>> end,
	{ok, Req2} = cowboy_req:set_resp_header('Allow', lists:foldl(F, <<>>, Methods), Req),
	{'No Content', Req2, State, <<>>}.

