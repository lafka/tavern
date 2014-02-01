defmodule Tavern.Mixfile do
  use Mix.Project

  def project do
    [ app: :tavern,
      version: "0.2.0",
      deps: deps ]
  end

  def application do
    [applications: [:cowboy]]
  end

  defp deps do
    [{:cowboy, github: "extend/cowboy", tag: "0.9.0"},
     {:jsx, github: "talentdeficit/jsx", tag: "v1.4.4"}]
  end
end
