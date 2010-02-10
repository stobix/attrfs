-module(xattr_sup).
-behaviour(supervisor).

%% External exports
-export([start_link/1]).

%% supervisor callbacks
-export([init/1]).

start_link(ExtProg) ->
    supervisor:start_link(?MODULE, ExtProg).

init(ExtProg) ->
    {ok, {{one_for_one, 3, 10},
          [{xattr, {xattr, start_link, [ExtProg]},
            permanent, 10, worker, [xattr]}]}}.

