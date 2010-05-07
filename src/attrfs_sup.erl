-module(attrfs_sup).

-behaviour(supervisor).

-export([start_link/3,init/1]).
-include("../include/debug.hrl").

start_link(From,To,DB) ->
    supervisor:start_link(?MODULE,{From,To,DB}).

init({From,To,DB}) ->
    ?DEB1("Starting attrfs_srv..."),
    {ok, {{one_for_all,3,10},
       [
         {inode_sup,{inode_sup,start_link,[]}, 
             permanent, infinity, supervisor ,[inode]},
         {tree_sup,{tree_sup,start_link,[]}, 
             permanent, infinity, supervisor, [tree_srv]},
         {attrfs,{attrfs_srv,start_link,[{To,From,DB}]}, 
             permanent, 10, worker, [attrfs]}
       ]}}.

