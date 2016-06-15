use Mix.Config

config :katja, :host, {127, 0, 0, 1}
config :katja, :transport, :tcp
config :katja, :port, 5555
config :katja, :defaults, []
config :katja, :pool, []

config :exometer_core, :predefined, [
  {[:erlang, :vm, :mem, :total],            :gauge, []},
  {[:erlang, :vm, :mem, :processes],        :gauge, []},
  {[:erlang, :vm, :mem, :processes_used],   :gauge, []},
  {[:erlang, :vm, :mem, :system],           :gauge, []},
  {[:erlang, :vm, :mem, :atom],             :gauge, []},
  {[:erlang, :vm, :mem, :atom_used],        :gauge, []},
  {[:erlang, :vm, :mem, :binary],           :gauge, []},
  {[:erlang, :vm, :mem, :ets],              :gauge, []},

  {[:erlang, :sys, :compat_rel],                   :gauge, []},
  {[:erlang, :sys, :creation],                     :gauge, []},
  {[:erlang, :sys, :logical_processors],           :gauge, []},
  {[:erlang, :sys, :logical_processors_available], :gauge, []},
  {[:erlang, :sys, :logical_processors_online],    :gauge, []},
  {[:erlang, :sys, :port_count],                   :gauge, []},
  {[:erlang, :sys, :process_count],                :gauge, []},
  {[:erlang, :sys, :process_limit],                :gauge, []},
  {[:erlang, :sys, :scheduler_bind_type],          :gauge, []},
  {[:erlang, :sys, :scheduler_bindings],           :gauge, []},
  {[:erlang, :sys, :scheduler_id],                 :gauge, []},
  {[:erlang, :sys, :schedulers],                   :gauge, []},
  {[:erlang, :sys, :schedulers_online],            :gauge, []},
  {[:erlang, :sys, :thread_pool_size],             :gauge, []},

  # from erlang:statistics
  {[:erlang, :stats, :context_switches],    :gauge, []},
  {[:erlang, :stats, :io, :bytes, :out],    :gauge, []},
  {[:erlang, :stats, :io, :bytes, :in],     :gauge, []},
  {[:erlang, :stats, :reductions],          :gauge, []},
  {[:erlang, :stats, :run_queue],           :gauge, []},
]

config :telemetry, :service_prefix, []
config :telemetry, :attrs, %{}
config :telemetry, :tags, []

config :telemetry, :callbacks, [
  ## Information about memory dynamically allocated by the Erlang emulator.
  ## Every callback returns memory size in bytes.
  {[:erlang, :vm, :mem, :total],          {{:erlang, :memory, 1}, [:total]}},
  {[:erlang, :vm, :mem, :processes],      {{:erlang, :memory, 1}, [:processes]}},
  {[:erlang, :vm, :mem, :processes_used], {{:erlang, :memory, 1}, [:processes_used]}},
  {[:erlang, :vm, :mem, :system],         {{:erlang, :memory, 1}, [:system]}},
  {[:erlang, :vm, :mem, :atom],           {{:erlang, :memory, 1}, [:atom]}},
  {[:erlang, :vm, :mem, :atom_used],      {{:erlang, :memory, 1}, [:atom_used]}},
  {[:erlang, :vm, :mem, :binary],         {{:erlang, :memory, 1}, [:binary]}},
  {[:erlang, :vm, :mem, :ets],            {{:erlang, :memory, 1}, [:ets]}},

  {[:erlang, :sys, :compat_rel],                    {{:erlang, :system_info, 1}, [:compat_rel]}},
  {[:erlang, :sys, :creation],                      {{:erlang, :system_info, 1}, [:creation]}},
  {[:erlang, :sys, :logical_processors],            {{:erlang, :system_info, 1}, [:logical_processors]}},
  {[:erlang, :sys, :logical_processors_available],  {{:erlang, :system_info, 1}, [:logical_processors_available]}},
  {[:erlang, :sys, :logical_processors_online],     {{:erlang, :system_info, 1}, [:logical_processors_online]}},
  {[:erlang, :sys, :port_count],                    {{:erlang, :system_info, 1}, [:port_count]}},
  {[:erlang, :sys, :process_count],                 {{:erlang, :system_info, 1}, [:process_count]}},
  {[:erlang, :sys, :process_limit],                 {{:erlang, :system_info, 1}, [:process_limit]}},
  {[:erlang, :sys, :schedulers],                    {{:erlang, :system_info, 1}, [:schedulers]}},
  {[:erlang, :sys, :schedulers_online],             {{:erlang, :system_info, 1}, [:schedulers_online]}},
  {[:erlang, :sys, :thread_pool_size],              {{:erlang, :system_info, 1}, [:thread_pool_size]}},

  {[:erlang, :stats, :context_switches],    {{:erlang, :statistics, 1}, [:context_switches]}},
  {[:erlang, :stats, :io, :bytes],                  {{:erlang, :statistics, 1}, [:io]}},
  {[:erlang, :stats, :reductions],          {{:erlang, :statistics, 1}, [:reductions]}},
  {[:erlang, :stats, :run_queue],           {{:erlang, :statistics, 1}, [:run_queue]}},
]

config :telemetry, :sample_interval, 1000
