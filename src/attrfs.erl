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
-include("../include/attrfs.hrl").

start(_dont,_care) ->
  ?DEB2("Starting ~p",?MODULE),
  ?DEB1("  getting arguments"),
  DirFrom=case application:get_env(?MODULE,from_dir) of
    {ok,DF} -> [DF];
    undefined -> vget(from_dirs)
  end,
  DirTo=vget(to_dir),
  DB=vget(attributes_db),
  LinkedIn=vget(linked_in),
  MountOpts=vget(mount_opts),


  ?DEBL("Starting ~p mirroring from ~p to ~p using database ~p",[?MODULE,DirFrom,DirTo,DB]),
  case attrfs_sup:start_link(DirFrom,DirTo,DB,MountOpts,LinkedIn) of
    ok -> {ok,self()}; % Why do I sometimes need this? Why would supervisor:start_link suddenly start returning ok instead of {ok,Pid}?
   {ok,_Pid}=A -> A;
   E -> ?DEB2("Got an error while starting! Exiting! (~p)",E),E
  end.

stop(_State) -> ok.


%%--------------------------------------------------------------------------
%% Returns the value associated with Attribute in the .app file. Exits the app if not found.
%%--------------------------------------------------------------------------
vget(Attribute) ->
  case application:get_env(?MODULE,Attribute) of
    {ok,Value} -> 
      ?DEBL("  ~p: ~p",[Attribute,Value]),
      Value;
    undefined -> 
      ?DEB2("  ~p not defined! check your config file!",Attribute),
    exit("not found")
  end.
