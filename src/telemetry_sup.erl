-module(telemetry_sup).
-behaviour(supervisor).

-export([
     start_link/0
   , init/1
]).

start_link() ->
   supervisor:start_link(?MODULE, []).

init(_Args) ->
   Children = [
      #{ id => telemetry_worker
       , start => {telemetry_worker, start_link, []}}
   ],
   {ok, {#{strategy => one_for_one}, Children}}.
