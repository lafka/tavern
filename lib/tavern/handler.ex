defmodule Tavern.Handler do
  @moduledoc """
    Provides cowboy -> handler proxy,

    + Verifies Accept, Content-Type HTTP headers
    + Marshalls input/output to usable structure
    + Checks authorization (if any)
  """

  require :cowboy_req,  as: Req

  defmacro __using__(_opts // nil) do
    quote do
      Module.register_attribute __MODULE__, :methods,  persist: true
      Module.register_attribute __MODULE__, :accepts,  persist: true
      Module.register_attribute __MODULE__, :provides, persist: true
    end
  end

  def acceptable?(_req, _handler) do
    true
  end

  def consumable?(_req, _handler) do
    true
  end

  def authorized?(_req, _handler) do
    true
  end

  def error([error: err, code: code]) do
    [error: err, code: code]
  end

  def error([error: err]) do
    [error: err, code: 1000]
  end

  def reply(status, body, req, opts) do
    defaultaccept = [{{"text", "plain", []}, 1000, []}]
    {:ok, accept, req} = Req.parse_header "accept", req, defaultaccept

    Req.reply Tavern.HTTP.status(status),
              opts[:headers] || [],
              Tavern.Marshaller.encode(body, accept, opts[:handler]),
              req
  end
end
