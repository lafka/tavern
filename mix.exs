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
     {:json,"0.1.0",[github: "cblage/elixir-json"]}]
  end
end
