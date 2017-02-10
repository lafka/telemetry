defmodule Telemetry.Mixfile do
  use Mix.Project

  def project do
    [app: :telemetry,
     version: "0.0.1",
     elixir: "~> 1.2",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps(),
    ]
  end

  def application do
    [applications: [:lager, :exometer_core, :katja],
     mod: {:telemetry, []},
     env: [
      collectors: [
         ## Information about memory dynamically allocated by the Erlang emulator.
         ## Every callback returns memory size in bytes.
         {[:erlang, :vm, :mem, :total],                   {:gauge, []},  {{:erlang, :memory, 1}, [:total]}},
         {[:erlang, :vm, :mem, :processes],               {:gauge, []},  {{:erlang, :memory, 1}, [:processes]}},
         {[:erlang, :vm, :mem, :processes_used],          {:gauge, []},  {{:erlang, :memory, 1}, [:processes_used]}},
         {[:erlang, :vm, :mem, :system],                  {:gauge, []},  {{:erlang, :memory, 1}, [:system]}},
         {[:erlang, :vm, :mem, :atom],                    {:gauge, []},  {{:erlang, :memory, 1}, [:atom]}},
         {[:erlang, :vm, :mem, :atom_used],               {:gauge, []},  {{:erlang, :memory, 1}, [:atom_used]}},
         {[:erlang, :vm, :mem, :binary],                  {:gauge, []},  {{:erlang, :memory, 1}, [:binary]}},
         {[:erlang, :vm, :mem, :ets],                     {:gauge, []},  {{:erlang, :memory, 1}, [:ets]}},

         {[:erlang, :sys, :compat_rel],                   {:gauge, []},  {{:erlang, :system_info, 1}, [:compat_rel]}},
         {[:erlang, :sys, :creation],                     {:gauge, []},  {{:erlang, :system_info, 1}, [:creation]}},
         {[:erlang, :sys, :logical_processors],           {:gauge, []},  {{:erlang, :system_info, 1}, [:logical_processors]}},
         {[:erlang, :sys, :logical_processors_available], {:gauge, []},  {{:erlang, :system_info, 1}, [:logical_processors_available]}},
         {[:erlang, :sys, :logical_processors_online],    {:gauge, []},  {{:erlang, :system_info, 1}, [:logical_processors_online]}},
         {[:erlang, :sys, :port_count],                   {:gauge, []},  {{:erlang, :system_info, 1}, [:port_count]}},
         {[:erlang, :sys, :process_count],                {:gauge, []},  {{:erlang, :system_info, 1}, [:process_count]}},
         {[:erlang, :sys, :process_limit],                {:gauge, []},  {{:erlang, :system_info, 1}, [:process_limit]}},
         {[:erlang, :sys, :schedulers],                   {:gauge, []},  {{:erlang, :system_info, 1}, [:schedulers]}},
         {[:erlang, :sys, :schedulers_online],            {:gauge, []},  {{:erlang, :system_info, 1}, [:schedulers_online]}},
         {[:erlang, :sys, :thread_pool_size],             {:gauge, []},  {{:erlang, :system_info, 1}, [:thread_pool_size]}},

         {[:erlang, :stats, :context_switches],           {:gauge, []},  {{:erlang, :statistics, 1}, [:context_switches]}},
         {[:erlang, :stats, :io, :bytes],                 {:gauge, []},  {{:erlang, :statistics, 1}, [:io]}},
         {[:erlang, :stats, :reductions],                 {:gauge, []},  {{:erlang, :statistics, 1}, [:reductions]}},
         {[:erlang, :stats, :run_queue],                  {:gauge, []},  {{:erlang, :statistics, 1}, [:run_queue]}},
      ],
      service_prefix:  [],
      attrs:           %{},
      tags:            [],
      sample_interval: 1000
     ]
    ]
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
