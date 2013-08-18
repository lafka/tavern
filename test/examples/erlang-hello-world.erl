-module('erlang-hello-world').

-export([
	  handle/2
	, test_handle_req/1]).

-methods([{'_', handle}]).

handle(Req, []) ->
	{<<"OK">>, <<"Hi, this is Erlang speaking">>, Req}.

test_handle_req(_) ->
	{ok, {Status, Body}} =
		'Elixir.TavernTest':do_req(<<"/erlang-hello-world">>),

	'ExUnit.Case':assert(Status =:= 200),
	'ExUnit.Case':assert(Body =:= <<"Hi, this is Erlang speaking">>).
