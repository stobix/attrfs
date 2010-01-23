-module(filsystem_app).
-behaviour(application).

-export([start/2,stop/1]).

start(_Type,_Args) ->
        filsystem_sup:start_link().

stop(_State) ->
        ok. %spara filsystemet här, kanske?
