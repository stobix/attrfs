-module(utils).

-export([restart/1,chain/1]).

restart(Module) ->
 application:stop(Module),
 application:unload(Module),
 application:start(Module).


chain(Module) ->
 case restart(Module) of
   {error,{not_started,A}} ->
        chain(A),
        chain(Module);
   A -> A
 end.
