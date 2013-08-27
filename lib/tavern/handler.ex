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
      Module.register_attribute __MODULE__, :methods, persist: true
      Module.register_attribute __MODULE__, :consume, persist: true
      Module.register_attribute __MODULE__, :provide, persist: true
    end
  end

  @doc """
    Find valid marshallers for the given accept header
  """
  def acceptable(req, handler) do
    {:ok, accepts, _req} = accept_header req
    match_mimes accepts, Tavern.Marshaller.marshallers(handler)
  end

  def consumable(req, handler) do
    case Req.parse_header "content-type", req do
      {:ok, :undefined, _req} ->
        []

      {:ok, mime, _req} ->
        match_mimes [mime], Tavern.Marshaller.marshallers(handler)
    end
  end


  @doc """
    Intersect media range with available marshallers
  """
  def match_mimes(match, marshallers) do
#      [{{<<"plain">>,<<"text">>,[]},1000,[]}],[]
    mm = fn(a, b, acc) ->
      case match_mime {a, b}, marshallers, [] do
        [] -> acc
        m -> acc ++ m
      end
    end

    Enum.reduce match, [], fn
      ({{a, b, _}, _q, _}, acc) ->
        mm.(a, b, acc)

      ({a, b, _}, acc) when is_binary(a) and is_binary(b) ->
        mm.(a, b, acc)
    end
  end

  defp match_mime(_m, [], acc) do acc end
  defp match_mime({a, b},     [{{a, b}, mod} | rest], acc) do
    match_mime({a, b}, [{{a, b}, a, mod} | rest], acc) end

  defp match_mime({a, b},     [{{a, b},   a2, mod} | rest], acc) do
    match_mime({a, b},   rest, [{{a2,b}, mod} | acc]) end

  defp match_mime({"*", b},   [{{_a, b},   a2, mod} | rest], acc) do
    match_mime({"*", b}, rest, [{{a2,b}, mod} | acc]) end

  defp match_mime({a, "*"},   [{{a, b},   a2, mod} | rest], acc) do
    match_mime({a, "*"}, rest, [{{a2,b}, mod} | acc]) end

  defp match_mime({_a, b},     [{{"*", b}, a2, mod} | rest], acc) do
    match_mime({"*", b}, rest, [{{a2,b}, mod} | acc]) end

  defp match_mime({a, b},     [{{a, "*"}, a2, mod} | rest], acc) do
    match_mime({a, "*"}, rest, [{{a2,b}, mod} | acc]) end

  defp match_mime({"*", "*"}, all, []) do all end
  defp match_mime({a, b}, [_ | rest], acc) do match_mime({a, b}, rest, acc) end

  def authorized?(_req, _handler) do
    true
  end

  def error([error: err, code: code]) do
    [error: err, code: code]
  end

  def error([error: err]) do
    [error: err, code: 1000]
  end

  def reply(status, body, req, state, opts // []) do
    [{{a, b}, _} | _] = accept = acceptable(req, state.handler)
    buf = Tavern.Marshaller.encode body, accept

    req = Req.set_resp_header("Content-Type", "#{a}/#{b}", req)
    Req.reply Tavern.HTTP.status(status),
              opts[:headers] || [],
              buf,
              req
  end

  defp accept_header(req) do
    default_accept = [{{"text", "plain", []}, 1000, []}]
    {:ok, _accept, _req} = Req.parse_header "accept", req, default_accept
  end
end
