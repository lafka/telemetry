-module(telemetry_discovery).

-export([
     connect/1
   , disconnect/1
]).

% Kind of a best guess, but given everything is started in order this
% should be fine.
connect(#{fqdn := FQDN}) ->
   case get_addr(FQDN, [inet6, inet]) of
      {ok, Addr} ->
         application:set_env(katja, transport, tcp),
         application:set_env(katja, host, Addr),

         case whereis(katja_writer) of
            Pid when is_pid(Pid) ->
               katja_writer:stop(Pid),
               katja_reader:stop(Pid);

            _ ->
               ok
         end;

      {error, nxdomain} = Err ->
         Err
   end.

get_addr(FQDN, AFs) when is_binary(FQDN) -> get_addr(binary_to_list(FQDN), AFs);
get_addr(_FQDN, []) -> {error, nxdomain};
get_addr(FQDN, [AF | Rest]) ->
   case inet:gethostbyname(FQDN, AF) of
      {ok, {hostent, _FQDN, _, AF, _, [Addr|_]}} ->
         {ok, Addr};

      {error, nxdomain} ->
         get_addr(FQDN, Rest)
   end.

disconnect(#{fqdn := _}) ->
   application:set_env(katja, host, {127, 0, 0, 1}).
