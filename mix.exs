defmodule Telemetry.Mixfile do
  use Mix.Project

  def project do
    [app: :telemetry,
     version: "0.0.1",
     elixir: "~> 1.2",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps]
  end

  def application do
    [applications: [:lager, :exometer_core, :katja],
     mod: {:telemetry, []}]
  end

  defp deps do
    [
      {:exometer_core, github: "Feuerlabs/exometer_core", tag: "1.4"},
      {:katja,         github: "nifoc/katja", tag: "v0.9.1"},
      {:meck,          github: "eproxus/meck", tag: "0.8.2", override: true},
      {:edown,         github: "uwiger/edown", ref: "HEAD", override: true}
    ]
  end
end
