-module(attrfs).
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

-behaviour(application).

-export([start/2,stop/1]).
-include("../include/debug.hrl").

start(_dont,_care) ->
  ?DEB2("Starting ~p",?MODULE),
  ?DEB1("  getting arguments"),
  {ok,DirFrom}=application:get_env(?MODULE,from_dir),
  ?DEB2("  dir_from: ~p",DirFrom),
  {ok,DirTo}=application:get_env(?MODULE,to_dir),
  ?DEB2("  dir_to: ~p",DirTo),
  {ok,DB}=application:get_env(?MODULE,attributes_db),
  ?DEB2("  database: ~p",DB),
  {ok,LinkedIn}=application:get_env(?MODULE,linked_in),
  ?DEB2("  linked in status: ~p", LinkedIn),
  {ok,MountOpts}=application:get_env(?MODULE,mount_opts),
  ?DEB2("   fuserl mount opts: ~p", MountOpts),
  ?DEBL("Starting ~p mirroring from ~p to ~p using database ~p",[?MODULE,DirFrom,DirTo,DB]),
  attrfs_sup:start_link(DirFrom,DirTo,DB,MountOpts,LinkedIn).

stop(_State) -> ok.

