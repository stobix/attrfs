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

-export([start_link/5,init/1]).
-include("../include/debug.hrl").
-include("../include/attrfs.hrl").

start_link(From,To,DB,MountOpts,LinkedIn) ->
  case supervisor:start_link(?MODULE,{From,To,DB,MountOpts,LinkedIn}) of
    {ok,PID} ->
        ?DEB1(1,"Starting inode_sup..."),
        supervisor:start_child(PID,
            {inode_sup,{inode_sup,start_link,[]}, 
                temporary, infinity, supervisor ,[inode]}),
        ?DEB1(1,"Starting tree_sup..."),
        supervisor:start_child(PID,
            {tree_sup,{tree_sup,start_link,[]}, 
                temporary, infinity, supervisor, [tree_srv]}),
        ?DEB1(1,"Starting attr_reply..."),
        supervisor:start_child(PID,
            {attr_reply,{attr_reply,start_link,[]},
                temporary, 10, worker, [attr_reply]}),
        ?DEB1(1,"Starting attrfs_srv..."),
        supervisor:start_child(PID,
            {attrfs,{attrfs_srv,start_link,[To,LinkedIn,MountOpts,From,DB]}, 
                temporary, 10, worker, [attrfs]}),
        {ok,PID};
    E -> E
    end.

init({From,To,DB,MountOpts,LinkedIn}) ->
  ?DEB1(1,"Starting attrfs_sup..."),
  {ok, {{one_for_all,3,10},[]}}.

