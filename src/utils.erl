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

%Can this be implemented as a function: f(F),f(G),f(H),{ok,F}=file:list_dir("src/"),G=lists:filter(fun([$\.|_]) -> false; (A) -> (string:rstr(A,".orig")==0) end,F),H=lists:map(fun(X) -> l(list_to_atom(string:substr(X,1,string:len(X)-4))) end,G).
