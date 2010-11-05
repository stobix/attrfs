-module(utils).

-export([restart/1]).

restart(Module) ->
 application:stop(Module),
 application:unload(Module),
 application:start(Module).

