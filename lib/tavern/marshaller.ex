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
    Encode `body` using the first marshaller matched
  """
  def encode(body, []) do
    {:error, :no_valid_marshaller}
  end

  def encode(body, [{_, marshaller} = x | _rest]) do
    marshaller.encode body
  end


  @doc """
    Decode `buf` using the first marshaller matched
  """
  def decode(buf, []) do
    {:error, :no_valid_marshaller}
  end

  def decode(buf, [{_, marshaller} | _req]) do
    marshaller.decode buf
  end

  def marshallers(handler) do
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
    def encode(body) when is_binary(body) do
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
