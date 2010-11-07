-module(attr_open).

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

%%%=========================================================================
%%%                                  META
%%%=========================================================================
%%% @author Joel Ericson <joel.mikael.ericson@gmail.com>
%%%
%%% @copyright Copylefted using some GNU license or other.
%%%
%%% @version 1.0
-include("../include/attrfs.hrl").
-include("../include/debug.hrl").

-export([init/0,
       lookup/1,
       set/3,
       remove/1,
       forget/1]).

init()->
  ets:new(open_files,[named_table,set,public]).
%%--------------------------------------------------------------------------
%% lookup_open_file gets the open file corresponding to the inode provided.
%% returns like gb_trees:lookup
%%--------------------------------------------------------------------------
lookup(FIno) ->
  case ets:match(open_files,{{FIno,'_'},'$1'}) of
    "" -> none;
    [[File]] -> {value,File}
  end.

%%--------------------------------------------------------------------------
%% set_open_file returns a state with the open file for the provided inode
%% changed to the FileContents provided.
%%--------------------------------------------------------------------------
set(FIno,Inode,FileContents) ->
  ets:insert(open_files,{{FIno,Inode},FileContents}).

%%--------------------------------------------------------------------------
%% remove_open_file removes the file from the current context. Used for closedir.
%%--------------------------------------------------------------------------
remove(FIno) ->
  ets:match_delete(open_files,{{FIno,'_'},'_'}).

%%--------------------------------------------------------------------------
%% forget_open_file removes an open file from the table, for all Ctx-es.
%%--------------------------------------------------------------------------
forget(Inode) ->
  ets:match_delete(open_files,{{'_',Inode},'_'}).

