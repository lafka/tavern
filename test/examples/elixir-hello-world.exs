defmodule Examples.ElixirHelloWorld do
  use ExUnit.Case, async: true
  use Tavern.Handler

  @methods [_: :handle ]

  def handle_get(req, []) do
    {"OK", "Elixir wants to say hi!", req}
  end

  test "say hi!" do
    {path, port} = {"/elixir-hello-world", 12345}

    dispatch = :cowboy_router.compile([
                 {:_, [{path, Tavern.Invoker, [__MODULE__]}]}
               ])
    {:ok, _} = :cowboy.start_http(:http, 100,
                                  [port: port],
                                  [env: [dispatch: dispatch]])

    {status, body} = TavernTest.do_req "GET", path, [], port

    assert body == "Elixir wants to say hi!"
    assert status == 200
  end
end
