defmodule Tavern.Mixfile do
  use Mix.Project

  def project do
    [ app: :tavern,
      version: "0.2.0",
      elixir: "~> 0.10.1",
      deps: deps ]
  end

  def application do
    [applications: [:cowboy]]
  end

  defp deps do
    [{:cowboy, github: "extend/cowboy", tag: "0.8.6"},
     {:jsx, github: "talentdeficit/jsx", tag: "v1.4.3"}]
  end
end
