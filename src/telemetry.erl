-module(telemetry).
-behaviour(application).

-export([
     start/2
   , stop/1
   , emit/2, emit/3
]).

start(_, _) ->
   telemetry_sup:start_link().

stop(_Reason) ->
   ok.

emit(Metric, Value) -> emit(Metric, Value, #{}).
emit(Metric, Value, Attrs) ->
   Millis = os:system_time(milli_seconds),

   Prefix = application:get_env(telemetry, service_prefix, []),
   DefaultAttrs = application:get_env(telemetry, attrs, #{}),
   DefaultTags  = application:get_env(telemetry, tags, []),

   Tags = unique(maps:get(tags, DefaultAttrs, []) ++ maps:get(tags, Attrs, []) ++ DefaultTags),
   NewAttrs = maps:put(tags, Tags, maps:merge(DefaultAttrs, Attrs)),

   Service = Prefix ++ Metric,
   ok = gen_server:call(telemetry_worker, {event, Service, Value, NewAttrs, Millis}).

unique(L) ->
    unique([],L).
unique(R,[]) -> R;
unique(R,[H|T]) ->
    case member_remove(H,T,[],true) of
        {false,Nt} -> unique(R,Nt);
        {true,Nt} -> unique([H|R],Nt)
    end.

member_remove(_,[],Res,Bool) -> {Bool,Res};
member_remove(H,[H|T],Res,_) -> member_remove(H,T,Res,false);
member_remove(H,[V|T],Res,Bool) -> member_remove(H,T,[V|Res],Bool).
