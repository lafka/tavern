-include_lib("tavern/include/rest.hrl").

-export([init/3, handle/2, terminate/2, handle_default_options/2]).

-spec init(Transport :: module(), Req :: cowboy_req:req(), list()) -> {ok, cowboy_req:req(), #tavern{}}.
init(Transport, Req, _Opts) ->
	tavern_http:init(Transport, Req, [?MODULE]).

-spec handle(cowboy_req:req(), #tavern{})-> {ok, cowboy_req:req(), #tavern{}}.
handle(Req, State) ->
	tavern_http:handle(?MODULE, Req, State).

-spec terminate(cowboy_req:req(), #tavern{})-> ok.
terminate(Req, State) ->
	tavern_http:terminate(?MODULE, Req, State).

-spec handle_default_options(_, #tavern{}) -> {Status :: atom(), _, #tavern{}, binary()}.
handle_default_options(Req, #tavern{allowed_methods = Methods} = State) ->
	Req2 = cowboy_req:set_resp_header(<<"Allow">>, [<<X/binary, $,>> || X <- Methods], Req),
	{'No Content', Req2, State, []}.
