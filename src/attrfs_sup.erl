-module(attrfs_sup).

-behaviour(supervisor).

-export([start_link/2,init/1]).
-include("../include/debug.hrl").

start_link(From,To) ->
    supervisor:start_link(?MODULE,{From,To}).

init({From,To}) ->
    ?DEB1("Starting attrfs_srv..."),
    {ok, {{one_for_one,3,10},
        [{attrfs,{attrfs_srv,start_link,[{To,From}]},
         permanent, 10, worker, [attrfs]}]}}.

