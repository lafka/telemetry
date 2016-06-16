-module(telemetry_worker).
-behaviour(gen_server).

-export([
     start_link/0
   , init/1

   , handle_call/3
   , handle_cast/2
   , handle_info/2

   , code_change/3
   , terminate/2
]).

start_link() ->
   gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
   ok = add_metric(application:get_env(telemetry, collectors, [])),

   self() ! collect,
   self() ! flush,

   {ok, #{incoming => []}}.

add_metric([]) -> ok;
add_metric([{Metric, {Type, Opts}, _Call} | Rest]) ->
   ok = exometer:new(Metric, Type, Opts),
   add_metric(Rest).

handle_call(Ev = {event, _Service, _Value, _Attrs, _Millis}, _From, #{incoming := Incoming} = State) ->
   {ok, NewState} = send_events([Ev | Incoming], State#{incoming => []}),
   {reply, ok, NewState}.

handle_cast(_Ev, State) -> {noreply, State}.

send_events([], State) -> {ok, State};
send_events([Ev | Events], #{incoming := _Rest} = State) ->
   {event, Service, Value, Attrs, Millis} = Ev,
   [_|ServiceFmt] = lists:flatmap(fun(Atom) when is_atom(Atom) -> [" ", atom_to_binary(Atom, utf8)];
                                 (Buf) when is_binary(Buf) -> [" ", Buf];
                                 (List) when is_list(List) -> [" ", List] end,
                              Service),

   case katja:send_event([{service, ServiceFmt},
                          {time, trunc(Millis / 1000)},
                          {tags, maps:get(tags, Attrs, [])},
                          {state, maps:get(state, Attrs, "ok")},
                          {attributes, maps:to_list(maps:without([state, tags], Attrs))},
                          {metric, Value}]) of
      ok -> send_events(Events, State);
      Err ->
         error_logger:error_msg("failed to save metrics: ~p~nreason: ~p~n", [Ev, Err]),
         send_events(Events, State)
   end.

handle_info(flush, State) ->
   Interval = application:get_env(telemetry, sample_interval, 1000),
   erlang:send_after(Interval * 5, self(), flush),

   Events = lists:map(fun({Service, Params}) ->
      {value, Val} = lists:keyfind(value, 1, Params),
      {event, Service, Val, #{tags => ["erlang-vm"]}, os:system_time(milli_seconds)}
   end, exometer:get_values([])),

   {ok, NewState} = send_events(Events, State),
   {noreply, NewState};

handle_info(collect, State) ->
   Interval = application:get_env(telemetry, sample_interval, 1000),

   _ = collect(application:get_env(telemetry, collectors, [])),

   erlang:send_after(Interval, self(), collect),
   {noreply, State}.

collect([]) -> ok;

collect([{[erlang, stats, io, bytes] = Metric, _Type, {{Mod, Fun, _Arity}, Args}} | T]) ->
   {{input, In}, {output, Out}} = apply(Mod, Fun, Args),
   exometer:update(Metric ++ [in], In),
   exometer:update(Metric ++ [out], Out),
   collect(T);

collect([{Metric, _Type, {{Mod, Fun, _Arity}, Args}} | T]) ->
   exometer:update(Metric, fmt(Metric, apply(Mod, Fun, Args))),
   collect(T).

fmt([erlang, stats, reductions], {Total, _SinceLast}) -> Total;
fmt([erlang, stats, context_switches], {Val, 0}) -> Val;
fmt(_Metric, Val) -> Val.

code_change(_Old, State, _Extra) -> {ok, State}.

terminate(_Reason, _State) -> ok.
