defmodule Tavern.Invoker do

  require :cowboy_req,  as: Req
  require :cowboy_http, as: CowboyHttp

  @moduledoc """
    Provides cowboy -> handler proxy,

    + Verifies Accept, Content-Type HTTP headers
    + Marshalls input/output to usable structure
    + Checks authorization (if any)
  """

  defrecord State, handler: nil,
                   consumes: nil,
                   provides: nil

  @doc """
    Checks headers before handing control to `handle/2`
  """
  def init(_transport, req, [handler]) do
    consumable = Tavern.Handler.consumable? req, handler
    acceptable = Tavern.Handler.acceptable? req, handler

    state = State.new handler: handler

    case req do
      req when not consumable ->
        Tavern.Handler.reply "Unsupported Media Type",
          Tavern.Handler.error([
            error: "unsupported argument in 'Content-Type' header"]),
          req,
          [handler: state.handler]

        {:shutdown, req, state}

      req when not acceptable ->
        Tavern.Handler.reply "Not Accepted",
          [],
          Tavern.Handler.error([
            error: "unsupported argument in 'Accept' header"]),
          req,
          state

        {:shutdown, req, state}

      req ->
        {:ok, req, state}
    end
  end


  def handle(req, state) do
    {method, req} = Req.method(req)

    # map handle_get, handle_post, handle_put, handle_delete, etc
    fun = binary_to_atom "handle_#{String.downcase method}"

    if function_exported? state.handler, fun, 2 do
      {:ok, body} = maybe_decode_body req

      {status, body, req} = apply state.handler, fun, [req, body]
      Tavern.Handler.reply status, body, req, [handler: state.handler]
    else
      Taver.Handler.reply "Internal Server Error",
        Tavern.Handler.error([
          error: "no request handler defined",
          code: 1002]),
        req,
        [handler: state.handler]
    end
  end

  def terminate(_reason, _req, _state) do
    :ok
  end

  defp maybe_decode_body(req) do
    {:ok, body} = if Req.has_body req do
      {:ok, contenttype} = CowboyHttp.content_type req
      {:ok, buf, req} = Req.body req
      Tavern.Marshaller.decode buf, contenttype
    else
      {:ok, []}
    end
  end
end