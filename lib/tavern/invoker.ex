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
    state = State.new [handler: handler,
      consumes: consume = Tavern.Handler.consumable(req, handler),
      provides: accept  = Tavern.Handler.acceptable(req, handler)]

    accept_header = lc {{a, b}, _} inlist accept do [a, "/", b] end
    req = Req.set_resp_header "Accept", iolist_to_binary(accept_header), req

    has_content = Req.has_body req

    case req do
      req when has_content and consume == [] ->
        {:ok, req} = reply_unconsumable req, state
        {:shutdown, req, state}

      req when accept == [] ->
        {:ok, req} = reply_unacceptable req, state
        {:shutdown, req, state}

      req ->
        {:ok, req, state}
    end
  end


  def handle(req, state) do
    {method, req} = Req.method(req)

    # map handle_get, handle_post, handle_put, handle_delete, etc
    fun = binary_to_atom "handle_#{String.downcase method}"


    {:ok, req} = if function_exported? state.handler, fun, 2 do
      {:ok, body} = maybe_decode_body req, state

      {status, body, req} = apply state.handler, fun, [req, body]
      Tavern.Handler.reply status, body, req, state
    else
      Tavern.Handler.reply "Internal Server Error",
        Tavern.Handler.error([
          error: "no request handler defined",
          code: 1002]),
        req,
        state
    end

    {:ok, req, state}
  end

  def terminate(_reason, _req, _state), do: :ok

  defp reply_unconsumable(req, state) do
    Tavern.Handler.reply "Unsupported Media Type",
      Tavern.Handler.error([
        error: "unsupported argument in 'Content-Type' header"]),
      req,
      state
  end

  defp reply_unacceptable(req, state) do
    Tavern.Handler.reply "Not Acceptable",
      Tavern.Handler.error([
        error: "unsupported argument in 'Accept' header"]),
      req,
      state
  end

  defp maybe_decode_body(req, state) do
    if Req.has_body req do
      {:ok, buf, req} = Req.body req
      Tavern.Marshaller.decode buf, Tavern.Handler.consumable(req, state.handler)
    else
      {:ok, ""}
    end
  end
end
