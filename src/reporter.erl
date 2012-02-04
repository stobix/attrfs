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
        get_items/0,lookup/1,get_all/0,
        flush/0,
        get_times/1,
        get_times/3
        ]).
%%%=========================================================================
%%% application functions. 
%%%=========================================================================

start(_Type,_Args) ->
  ?DEB1(1,"Starting reporter supervisor..."),
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
    {ok,{gb_trees:empty(),gb_trees:empty()}}.

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

put(Anything,Time) ->
    gen_server:cast(?MODULE,{report,Anything,Time}).

put(Anything) ->
    gen_server:cast(?MODULE,{report,Anything,time()}).


get_items() ->
    gen_server:call(?MODULE,items).

lookup(Item) ->
    gen_server:call(?MODULE,{item,Item}).

get_all() ->
    gen_server:call(?MODULE,all).

flush() ->
    gen_server:cast(?MODULE,flush).

get_times(Item) ->
    gen_server:call(?MODULE,{get_times,Item}).

get_times(Start,End,Item) ->
    AllTimes=gen_server:call(?MODULE,{get_times,Item}),
    lists:filter(fun({Time,_}) -> biggerThan(Time,Start) and biggerThan(End,Time) end,AllTimes).

biggerThan({X1,X2,X3},{Y1,Y2,Y3}) ->
    X1>Y1 orelse (X1==Y1 andalso (X2>Y2 orelse (X2==Y2 andalso X3>=Y3))).
                        


%%%=========================================================================
%%% callbacks.
%%%=========================================================================
curr_val(Token,Tree) ->
    case gb_trees:lookup(Token,Tree) of
        none ->
            0;
        {value,V} ->
            V
    end.

increase(Token,Tree) ->
    gb_trees:enter(Token,curr_val(Token,Tree)+1,Tree).

update_tree(Fun,Key,Tree) -> gb_trees:enter(Key, Fun(gb_trees:lookup(Key, Tree)), Tree).

handle_cast(flush,{_Tree,TimeTree}) ->
    {noreply,{gb_trees:empty(),TimeTree}};

handle_cast({report,Token,Time},{Tree,TimeTree}) ->
    NewTimeEntries=case gb_trees:lookup(Token,TimeTree) of
        {value,TimeEntries} -> increase(Time,TimeEntries);
        none -> gb_trees:enter(Time,1,gb_trees:empty())
    end,
    NewTimeTree = gb_trees:enter(Token,NewTimeEntries,TimeTree),
    NewTree=increase(Token,Tree),
    {noreply,{NewTree,NewTimeTree}}.

handle_call(items,_From,State={Tree,_}) ->
    {reply,gb_trees:keys(Tree),State};

handle_call({item,Item},_From,State={Tree,_}) ->
    {reply,gb_trees:lookup(Item,Tree),State};
  
handle_call({get_times,Item},_From,State={_,TimeTree}) ->
    Reply=case gb_trees:lookup(Item,TimeTree) of
        {value,V} ->
          gb_trees:to_list(V);
        none -> none
    end,
    {reply,Reply,State};
  
handle_call(all,_From,State={Tree,_}) ->
  {reply,gb_trees:to_list(Tree),State}.
    
