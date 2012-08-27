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

-export([start_link/0,init/1]).
-include("../include/debug.hrl").
-include("../include/attrfs.hrl").


assert(X) ->
        case X of
            {ok,_PID} ->
                true;
            ok ->
                true;
            E -> E
        end.

%% Fold assert over a list of messages.
%% If we get an error code, stop folding and return it; otherwise, continue folding, returning SuccessMsg if we reach the end of the list.
asserts([],SuccessMsg) -> SuccessMsg;

asserts([M|Msgs],SuccessMsg) ->
    case assert(M) of
        true ->
            asserts(Msgs,SuccessMsg);
        E ->
            E
    end.

start_link() ->
  case supervisor:start_link(?MODULE,[]) of
    {ok,PID} ->
        ?DEB1(1,"Starting inode_sup..."),
        Msg1=supervisor:start_child(PID,
            {inode_sup,{inode_sup,start_link,[]}, 
                temporary, infinity, supervisor ,[inode]}),
        ?DEB1(1,"Starting tree_sup..."),
        Msg2=supervisor:start_child(PID,
            {tree_sup,{tree_sup,start_link,[]}, 
                temporary, infinity, supervisor, [tree_srv]}),
        ?DEB1(1,"Starting attr_reply..."),
        Msg3=supervisor:start_child(PID,
            {attr_reply,{attr_reply,start_link,[]},
                temporary, 10, worker, [attr_reply]}),
        ?DEB1(1,"Starting options server"),
        Msg4=supervisor:start_child(PID,
            {options,{options,start_link,["~/.attrfsrc"]},
                temporary, 10, worker, [options]}),
        ?DEB1(1,"Starting attrfs_srv..."),
        Msg5=supervisor:start_child(PID,
            {attrfs,{attrfs_srv,start_link,[]}, 
                temporary, 10, worker, [attrfs]}),
        % XXX: Checking for success after all children has been started is kinda inefficient if the first one failed.
        %       All but the last one are lightweight, though, so it shouldn't be a problem.
        asserts([Msg1,Msg2,Msg3,Msg4,Msg5],{ok,PID});
    E -> E
  end.

init(_) ->
  ?DEB1(1,"Starting attrfs_sup..."),
  {ok, {{one_for_all,3,10},[]}}.

