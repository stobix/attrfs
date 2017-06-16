 -module(attr_opendir).
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
-include_lib("newdebug/include/debug.hrl").

-export([direntries/1]).

%%--------------------------------------------------------------------------
%% Gets the children for the inode from the inode list, and runs direntrify
%% on it.
%%--------------------------------------------------------------------------
direntries(Inode) ->
  ?DEB1(8,"Creating direntries"),
  ?DEB1(9,"Getting child entries"),
  {value,Children}=attr_lookup:children(Inode),
  ?DEBL(9,"Converting children ~w for ~w",[Children,Inode]),
  direntrify(Children).


%%--------------------------------------------------------------------------
%% direntrify takes a [{Name,Inode,Type}] and returns a [fuserl:#{direntry}]
%%--------------------------------------------------------------------------

direntrify(List) ->
  direntrify(List,1).

direntrify([],_N) -> 
  ?DEB2(9,"Done converting ~b children",_N),
  [];

direntrify([{Name,Inode,_Type}|Children],N) ->
  ?DEB2(9,"Getting entry for child ~w",{Name,Inode,_Type}),
  {value,Child}=tree_srv:lookup(Inode,inodes),
  ?DEB2(9,"Getting permissions for child ~w",{Name,Inode,_Type}),
  ChildStats=Child#inode_entry.stat,
  ?DEB2(9,"Creating direntry for child ~w",{Name,Inode,_Type}),
  Direntry= #direntry{name=Name ,stat=ChildStats },
  ?DEB2(9,"Calculatig size for direntry for child ~w",{Name,Inode,_Type}),
  Direntry1=Direntry#direntry{offset=N},
  ?DEB2(9,"Appending child ~p to list",{Name,Inode,_Type}),
  [Direntry1|direntrify(Children,N+1)].

