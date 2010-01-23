-module(filsystem_sup).
-behaviour(supervisor).
-export([start_link/0,start_link/1]).
-export([init/1]).

start_link()->
    supervisor:start_link(?MODULE,[]).

start_link(FS)->
    supervisor:start_link(?MODULE,[FS]).


init(Args) ->
    {ok, {{one_for_one, 1, 31}, [{filsystemet, {filsystem, start_link, Args}, permanent, 5107, worker, [filsystem]}]}}.

%start_child() ->
    %supervisor:start_child(me,[]).
