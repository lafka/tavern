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
  def encode(_body, []), do: {:error, :no_valid_marshaller}
  def encode(body, [{_, marshaller} | _rest]), do: marshaller.encode body


  @doc """
    Decode `buf` using the first marshaller matched
  """
  def decode(_buf, []), do: {:error, :no_valid_marshaller}
  def decode(buf, [{_, marshaller} | _req]), do: marshaller.decode buf

  def marshallers(handler) do
    a = Keyword.get_values(handler.__info__(:attributes), :marshal)
    b = Keyword.get_values(        __info__(:attributes), :marshal)

    lc [x] inlist a ++ b do x end
  end

  @marshal {{"*", "xml"}, "application", __MODULE__.XML}
  defmodule XML do
    def encode(_body), do: ""

    def decode(_buf), do: {:ok, []}
  end

  @marshal {{"text", "plain"}, __MODULE__.Raw}
  defmodule Raw do
    def encode(:ignore),                   do: ""
    def encode(body) when is_binary(body), do: body
    def encode(body),                      do: Kernel.inspect body

    def decode(buf), do: buf
  end

  @marshal {{"*", "json"}, "application", __MODULE__.JSONProxy}
  defmodule JSONProxy do
    def encode(:ignore), do: ""
    def encode(body), do: JSON.encode body

    def decode(buf), do: JSON.decode buf
  end
end
