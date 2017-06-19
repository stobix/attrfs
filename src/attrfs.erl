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

-module(attrfs).

-behaviour(application).

-export([start/2,stop/1]).
-include_lib("newdebug/include/debug.hrl").
-include("../include/attrfs.hrl").

start(_dont,_care) ->
    newdebug:tty(true),
  ?DEB2(1,"Starting ~p",?MODULE),

  case attrfs_sup:start_link() of
    ok -> 
    ?DEB1(1,"Ok, returning self"),
    {ok,self()}; % Why do I sometimes need this? Why would supervisor:start_link suddenly start returning ok instead of {ok,Pid}?
   {ok,_Pid} ->
    ?DEB2(1,"Ok, returning ~p",_Pid), 
    {ok,_Pid};
   E -> 
    ?DEB1(err,"Got an error while starting! Exiting!"),
    E
  end.

stop(_State) -> ok.


