-module(utils).

-export([restart/1,chain/1,recompile/0,reload_files/1]).

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

%Can this be implemented as a function? 
% f(I),I=fun() -> f(F),f(G),f(H),{ok,F}=file:list_dir("src/"),G=lists:filter(fun([$\.|_]) -> false; (A) -> (string:rstr(A,".orig")==0) end,F),H=lists:map(fun(X) -> l(list_to_atom(string:substr(X,1,string:len(X)-4))) end,G),io:format("~p",[H]) end, (make:all([{d,timestamp},{d,debug},{d,test},debug_info])==up_to_date) andalso I().
recompile()->
    {ok,Files}=file:list_dir("src/"),
    (make:all()==up_to_date) andalso 
        reload_files(valid_files(Files)).


valid_files(Files) ->
    lists:filter(fun
                    ([$\.|_]) -> false; 
                    (A) -> (string:rstr(A,".orig")==0) 
                end,
                Files).

reload_files(Files) ->
    lists:map(fun(X) ->                                                                                                                                                            Name=list_to_atom(string:substr(X,1,string:len(X)-4)),
                  code:purge(Name),
                  code:load_file(Name)
              end,
              Files).

