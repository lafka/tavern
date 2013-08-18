defmodule CustomTypes do
  use ExUnit.Case

  @moduledoc """
    Test for custom marshallers.

    The handler will decode all default input types in addition to
    `application/x-proto`. All output will be encoded `application/x-proto`
  """

  @doc """
    `@accepts`/`@provides` expects either a MIME tuple, or a MIME
    triplet where last element defines the callback module. Tuples
    defaults to Tavern.Marshaller as the callback.
  """
  @accepts  {:append, [{"application", "x-proto"}]}
  @provides [{"application", "x-proto"}]
  @methods  ["GET", "POST"]

  def encode({"application", "x-proto"}, terms) do
    terms[:body]
  end

  def decode({"application", "x-proto"}, buf) do
    [proto: "application/x-proto", body: buf]
  end

  def handle_get(req, []) do
    {"OK", [body: "Hello World"], req}
  end

  def handle_post(req, [proto: _, body: buf]) do
    {"Updated", [body: buf], req}
  end

  def handle_post(req, body) do
    fmt = :io_lib.format("invalid request: ~p~n", [body])
    {"Bad Request", [body: fmt], req}
  end


  def test_my_handle_put(_) do
    assert true
  end
end
