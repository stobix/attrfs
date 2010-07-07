-module(attrfs_sup).

%%%=========================================================================
%%%                                 LICENSE
%%%=========================================================================
%%%
%%%  This program is free software; you can redistribute it and/or modify
%%%  it under the terms of the GNU General Public License as published by
%%%  the Free Software Foundation; either version 2 of the License, or
%%%  (at your option) any later version.
%%%
%%%  This program is distributed in the hope that it will be useful,
%%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%  GNU Library General Public License for more details.
%%%
%%%  You should have received a copy of the GNU General Public License
%%%  along with this program; if not, write to the Free Software
%%%  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%%
%%%=========================================================================
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

