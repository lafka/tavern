-module(resource_handler).

-include("rest.hrl").
allowed_methods(Req, State) ->
	{['HEAD', 'GET', 'POST'], Req, State}.

%% Fetch the already processed body
handle_post(Req, State#{body = Body, grouped = true}) ->
	ValidGroups      = [meta, payload],
	case lists:splitwith(fun(A) -> lists:member(A, ValidGroups)) of
		{_, Z} when length(X) >= 0 ->
			{422, [{error, [{message, {iolist, [<<"invalid groups: ",
				lists:map(fun(B) -> [atom_to_binary(B), <<", ">>] end,
					lists:dropwhile(fun(A) -> lists:member(A, ValidGroups)));
		{X, Z} when length(X) == 0 and length(z) = length(Z) ->
			db_handl:store(cowboy_http_req:binding(timestamp, Req), Body),
			{201, [{}]};
		{X, _} ->
			{422, [{error, [{message, {iolist, [<<"missing data for: ",
				lists:map(fun(B) -> [atom_to_binary(B), <<", ">>] end,
					lists:dropwhile(fun(A) -> lists:member(A, ValidGroups)))
	end,

	case Invalid of

	Meta = lists:keyfind(meta, 1, Body) of {meta, Params} -> P; _ -> erlang:error(incomplete_resource, {missing, [meta]})
	lists:keyfind(email, 1, [{meta, P} || {meta, P} <- ])
	lists:foreach( fun(I) -> case lists:keyfind(email, 1, I))

