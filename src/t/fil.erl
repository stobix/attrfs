-module(fil).
%%% In this file I try different file operations.

-export([pwd/0,load/0,consult/0,consult/1,consult/2]).
-export([info_query/1]).
-export([chain_query/1]).
-export([tripfind/1]).

pwd() ->
    {ok,Bla}=file:get_cwd(),
    Bla.

load() ->
    {ok,Fil}=file:open("testfil",[append,{encoding,utf8}]),
            %A=io:get_line
    file:close(Fil).
            
consult() ->
    %{ok,T}=file:consult("testfil"),
    case(file:consult("testfil")) of
        {ok,T} -> T;
        E -> file:format_error(E)
end.

consult(Term) ->
    {ok,T}=file:open("testfil",write),
    io:format(T,"~w.\n",[Term]),
    file:close(T).

consult(Term,Mode) ->
    {ok,T}=file:open("testfil",Mode),
    io:format(T,"~w.\n",[Term]),
    file:close(T).

info_query(Name) ->
    {ok,Triplets} = file:consult("testfil"),
    find(Name,Triplets).

find(Term,[{Term,Type,Val}|_]) ->
    {ok,{Term,Type,Val}};

find(_Term,[]) ->
    {error,not_found};

find(Term,[_|B]) ->
    find(Term,B).

chain_query(Name) ->
    case info_query(Name) of 
        {ok, {Term, val, Val}} ->
            {Term, val, Val};
        {ok, {_Term, chain, NewName}} ->
            chain_query(NewName);
        E={error,_} -> 
            E
            end.
            
%chain_query2(Name) ->
%    case info_query(Name) of 
%        {ok, {Term, val, Val}} ->
%            {Term, val, Val};
%        {ok, {_Term, chain, NewName}} ->
%            chain_query(NewName);
%        E={error,_} -> 
%            E
%            end.

tripfind(Name) ->
    {ok,Triplets} = file:consult("testfil"),
    tripfind(Name,Triplets).

tripfind(Term,{A,[{Term,Type,Val}|B]}) ->
    {ok,{A,{Term,Type,Val},B}};

tripfind(Term,{A,[]}) ->
    tripfind2(Term,{[],lists:reverse(A)});

tripfind(Term,{A,[AA|B]}) ->
    tripfind(Term,{[AA|A],B}).

tripfind2(Term,{A,[{Term,Type,Val}|B]}) ->
    {ok,{A,{Term,Type,Val},B}};

tripfind2(_Term,{A,[]}) ->
    {error,not_found,A};

tripfind2(Term,{A,[AA|B]}) ->
    tripfind2(Term,{[AA|A],B}).

