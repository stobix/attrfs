-module(fil).
%-behaviour(gen_server).
%-import(file).
-export([pwd/0,load/0,consult/0,consult/1]).



pwd() ->
    case file:get_cwd() of
        {ok,Bla} ->
            Bla;
        A ->
            A
    end.

load() ->
    case file:open("testfil",[append,{encoding,utf8}]) of
        {ok,Fil} ->
            %A=io:get_line
            file:close(Fil);
        E -> E
    end.
            
consult() ->
    case file:consult("testfil") of
        {ok, T} -> T;
        E -> E
    end.

consult(Term) ->
    case file:open("testfil",write) of
        {ok,T} ->
            io:format(T,"~w.",[Term]),
            file:close(T);
        E -> E
    end.

                


%io:format("~p~n",[file:get_cwd()]).
