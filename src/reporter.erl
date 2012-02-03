-module(reporter).

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
%%% 
%%% @doc A collector of function calls, to show statistics and debug programs.
%%% 
%%% @end
%%%=========================================================================

-behaviour(gen_server).
-include("../include/debug.hrl").
-export([
        start/2,
        stop/1,
        start_link/0,
        init/0,
        init/1,
        code_change/3,
        terminate/2,
        handle_info/2,
        handle_call/3,
        handle_cast/2
        ]).

-export([
        put/1,put/2,
        get_items/0,lookup/1,get_all/0
        ]).
%%%=========================================================================
%%% application functions. 
%%%=========================================================================

start(_Type,_Args) ->
  reporter_sup:start_link().

stop(_State) ->
  ok.

%%%=========================================================================
%%% gen_server functions.
%%%=========================================================================

start_link() ->
    gen_server:start_link({local,?MODULE},?MODULE,[],[]).

init(_) -> init().

init() ->
    ?DEB1(1,"reporter started"),
    {ok,gb_trees:empty()}.

terminate(_Reason,_State) -> 
    ?DEB1(1,"reporter terminated"),
    ok.

code_change(_,_,_) -> 
    ok.
handle_info(_,_) -> 
    ok.


%%%=========================================================================
%%% main interface
%%%=========================================================================

put(Module,Function) ->
    gen_server:cast(?MODULE,{report,{Module,Function}}).

put(Anything) ->
    gen_server:cast(?MODULE,{report,Anything}).


get_items() ->
    gen_server:call(?MODULE,items).

lookup(Item) ->
    gen_server:call(?MODULE,{item,Item}).

get_all() ->
    gen_server:call(?MODULE,all).

%%%=========================================================================
%%% callbacks.
%%%=========================================================================
handle_cast({report,Token},State) ->
    ?DEBL(0,"adding ~p!",[Token]),
    Val=case gb_trees:lookup(Token,State) of
        none ->
          0;
        {value,V} ->
          V
    end,
    NewState=gb_trees:enter(Token,Val+1,State),
    {noreply,NewState}.

handle_call(items,_From,State) ->
    ?DEB1(0,"items returned"),
    {reply,gb_trees:keys(State),State};

handle_call({item,Item},_From,State) ->
    {reply,gb_trees:lookup(Item,State),State};
  
  
handle_call(all,_From,State) ->
  {reply,gb_trees:to_list(State),State}.
    
