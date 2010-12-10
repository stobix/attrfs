-module(tree_sup).
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

-export([start_link/0]).

-export([init/1]).

-include("../include/debug.hrl").

start_link() ->
  ?DEB1({tree,1},"Starting tree server"),
  supervisor:start_link(?MODULE,[]).

init(_) ->
  {ok, {{one_for_one, 3, 10},
      [{tree_srv, {tree_srv, start_link, []},
        permanent, 10, worker, [tree_srv]}]}}.

