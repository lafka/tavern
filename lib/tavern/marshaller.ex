defmodule Tavern.Marshaller do
  @moduledoc """
    Transforms content from user input to internal representation or
    visa verse.
  """

  defmacro __using__(_opts // nil) do
    quote do
      Module.register_attribute __MODULE__, :marshal,
        accumulate: true, persist: true
    end
  end

  Module.register_attribute __MODULE__, :marshal,
    accumulate: true, persist: true

  @doc """
    Encode `body` according to the `accept` header
  """
  def encode(_body, [], _handler) do
    {:error, :no_accept_header} end

  def encode(body, accepts, handler) do
    mods = Tavern.HTTP.match_mimes accepts, (marshallers handler)

    case mods do
      [{{_,_} = mime, marshaller} | _] ->
        marshaller.encode body

      [] ->
        {:error, :no_valid_marshaller}
    end
  end

  @doc """
    Decode `buf` according to the `Content-Type` header
  """
  def decode(_buf, [], _handler) do {:error, :no_content_type_header} end

  def decode(buf, content_type, handler) do
    mods = Tavern.HTTP.match_mimes content_type, (marshallers handler)

    case mods do
      [{{_,_} = mime, marshaller} | _] ->
        marshaller.decode buf

      [] ->
        {:error, :no_valid_marshaller}
    end
  end

  defp marshallers(handler) do
    a = Keyword.get_values(handler.__info__(:attributes), :marshal)
    b = Keyword.get_values(        __info__(:attributes), :marshal)

    lc [x] inlist a ++ b do x end
  end

  @marshal {{"*", "json"}, JSON} # from JSON package

  @marshal {{"*", "xml"}, __MODULE__.XML}
  defmodule XML do
    def encode(_body) do
      ""
    end

    def decode(_buf) do
      {:ok, []}
    end
  end

  @marshal {{"*", "yaml"}, __MODULE__.YAML}
  defmodule YAML do
    def encode(_body) do
      ""
    end

    def decode(_buf) do
      {:ok, []}
    end
  end

  @marshal {{"text", "plain"}, __MODULE__.Raw}
  defmodule Raw do
    def encode(body) do
      body
    end
    def encode(body) do
      Kernel.inspect body
    end

    def decode(buf) do
      buf
    end
  end
end
