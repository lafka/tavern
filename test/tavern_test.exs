Code.require_file "test_helper.exs", __DIR__

defmodule TavernTest do
  use ExUnit.Case

  require :cowboy_client, as: Client

  setup_all do
    :ok
  end

  def starthttp(dispatch, port) do
	{:ok, _} = :cowboy.start_http(:http, 100,
		[port: port],
		[env: [dispatch: dispatch]])
  end

  def do_req(method, path, headers, port) do
    url           = "http://localhost:#{port}#{path}"
    {:ok, client} = Client.init []
    {:ok, client} = Client.request method, url, headers, client
    {:ok, status, headers, client} = Client.response client
    {:ok, body, _client} = Client.response_body client

    {status, headers, body}
  end

end

"""
    All tests are all written as examples for each individual
    functionality. As versions keeps rolling, regressions tests will
    be added when needed.

    Tests all files in `examples/` folder, this includes:

    + Simple hello world
    + Erlang interoperability
    + MIME preconditions (`Accepts` and `Content-Type`)
    + Default marshallers (json/xml)
    + Custom marshalling
    + Custom method handlers
    + GET / PUT / POST / DELETE
    + Authentication
    + Streaming
  """
lc f inlist (Path.wildcard "#{__DIR__}/examples/*.exs") do
      Code.require_file f
      end
