-module(inode).
%%%=========================================================================
%%% Module inode
%%%=========================================================================
%%% @author Joel Ericson <kasettbok@gmail.com>
%%%
%%% @copyright Copylefted using some GNU license or other.
%%%
%%% @version 0.9
%%%-------------------------------------------------------------------------
%%% @doc This module provides a simple interface for leasing unique numbers (Inodes).
%%% @end
%%%=========================================================================
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

-behaviour(gen_server).

-export([get/0,release/1]).
-export([get/1,is_named/1,is_numbered/1,is_used/1]).
-export([reset/0,reset/1,list_bound/0]).
-export([rename/2]).
-export([count_occupied/0]).


-export([start_link/0,start_link/1,init/1]).
-export([handle_call/3,handle_cast/2]).
-export([terminate/2]).

-export([start/2,stop/1]).

-vsn(0.9).

-include("../include/debug.hrl").

%%%=========================================================================
%%% Type specifications
%%%=========================================================================
%%% 
%%% @type uniqe_integer() = non_neg_integer(). An integer whose value is not the same as any other currently in use.
%%% @end
%%%=========================================================================

%%%=========================================================================
%%% application functions.
%%%=========================================================================

start(_Type,_Args) ->
  inode_sup:start_link().

stop(_State) ->
  ok.


%%%=========================================================================
%%% gen_server functions
%%%=========================================================================

start_link() -> start_link(1).

start_link(SmallestNumber) ->
  ?DEB1("Starting inode server"),
  gen_server:start_link({local,?MODULE},?MODULE,SmallestNumber,[]).

init(SmallestNumber) ->
  {ok,{SmallestNumber,[],[]}}.

terminate(_Reason,_State) -> ok.

%%%=========================================================================
%%% exports
%%%=========================================================================

%%----------------------------------------------
%% @doc counts the number of occupied inodes.
%% @spec () -> non_neg_integer().
%% @end
%%----------------------------------------------
count_occupied() ->
  gen_server:call(?MODULE,count).

%%----------------------------------------------
%% @doc Returns an unused integer.
%% @spec () -> Number::unique_integer().
%% @end
%%----------------------------------------------
get() ->
  gen_server:call(?MODULE,get).

%%----------------------------------------------
%% @doc On first run, assosicate a unique integer with Name.
%%     On sequential runs, return the integer associated with Name.
%% @spec (term())-> Number::non_neg_integer().
%% @end
%%----------------------------------------------
get(Name) ->
  case is_numbered(Name) of
    false -> 
      Number=?MODULE:get(),
      ?DEBL("   binding ~p to ~p",[Name,Number]),
      gen_server:cast(?MODULE,{register,Name,Number}),
      Number;
    Number -> Number
  end.



%%----------------------------------------------
%% @doc Checks whether Number has a named associated with it. Returs either false, or the associated name.
%% @spec (non_neg_integer()) -> false|Name::term().
%% @end
%%----------------------------------------------
is_named(Number) ->
  gen_server:call(?MODULE,{is_named,Number}).

rename(OldName,NewName) ->
  gen_server:cast(?MODULE,{rename,OldName,NewName}).

%%----------------------------------------------
%% @doc Checks whether Name has a number associated with it. Returs either false, or the associated number.
%% @spec (term()) -> false|Name::uniqe_integer().
%% @end
%%----------------------------------------------
is_numbered(Name) ->
  gen_server:call(?MODULE,{is_numbered,Name}).

%%----------------------------------------------
%% @doc Checks whether Number is currently in use. Returns either false or true.
%% @spec (term()) -> bool().
%% @end
%%----------------------------------------------
is_used(Number) ->
  gen_server:call(?MODULE,{is_used,Number}).

%%----------------------------------------------
%% @doc Makes the number Number no longer occupied.
%%      Releases any name association with Number, and makes Number free to get returned by a call to get/0 or get/1.
%% @spec (term()) -> false|Name::unique_integer().
%% @end
%%----------------------------------------------
release(Number) ->
  gen_server:cast(?MODULE,{return,Number}).

%%----------------------------------------------
%% @doc Resets the counter. Forgets all name bindings.
%% @spec () -> ok.
%% @end
%%----------------------------------------------
reset() ->
  gen_server:cast(?MODULE,{reset,1}).

%%----------------------------------------------
%% @doc Resets the counter. Forgets all name bindings. Starts anew at number Number.
%% @spec (Number) -> ok.
%% @end
%%----------------------------------------------
reset(Number) ->
  gen_server:cast(?MODULE,{reset,Number}).

%%----------------------------------------------
%% @doc Lists all integers bound to a term.
%% @spec () -> [{term(),unique_integer()}].
%% @end
%%----------------------------------------------
list_bound() ->
  gen_server:call(?MODULE,list).

%%%=========================================================================
%%% gen_server callback functions.
%%%=========================================================================

handle_call(count,_From,State={CurrHigh,Frees,Reserved}) ->
  Reply=CurrHigh-length(Frees)+length(Reserved)-1,
  {reply,Reply,State};

handle_call(list,_From,State={_CurrHigh,_Frees,Reserved}) ->
  {reply,Reserved,State};

handle_call({is_used,Number},_From,State={CurrentHighest,Frees,Reserved}) ->
  Reply=not lists:member(Number,Frees) 
    orelse not lists:keymember(Number,2,Reserved)
    orelse Number >= CurrentHighest,
  {reply,Reply,State};


handle_call({is_numbered,Name},_From,State={_CurrHigh,_Frees,Reserved}) ->
  Reply=
    case lists:keyfind(Name,1,Reserved) of
      false -> false;
      {Name,Number} -> Number
    end,
  {reply,Reply,State};

handle_call({is_named,Number},_From,State={_CurrHigh,_Frees,Reserved}) ->
  Reply=
    case lists:keyfind(Number,2,Reserved) of
      false -> false;
      {Name,Number} -> Name
    end,
  {reply,Reply,State};

handle_call(get,_From,{CurrentHighest,[],_Reserved}) ->
  {reply, CurrentHighest,{CurrentHighest+1,[],_Reserved}};

handle_call(get,_From,{CurrentHighest,[Free|Frees],_Reserved}) ->
  {reply, Free, {CurrentHighest,Frees,_Reserved}}.

handle_cast({rename,OldName,NewName},Status={CurrentHighest,Frees,Reserved}) ->
  case lists:keytake(OldName,1,Reserved) of
    {value,{OldName,OldIno},NewReserved} ->
      ?DEBL("   ~p found. New list ~p.",[OldName,NewReserved]),
      {noreply,{CurrentHighest,Frees,[{NewName,OldIno}|NewReserved]}};
    false ->
      ?DEBL("   ~p not bound! Not binding ~p.",[OldName,NewName]),
      {noreply,Status}
  end;

handle_cast({reset,N},_) ->
  {noreply,{N,[],[]}};

handle_cast({register,Name,Number},{CurrentHighest,Frees,Reserved}) ->
  {noreply,{CurrentHighest,Frees,[{Name,Number}|Reserved]}};

handle_cast({return,NewFree},{CurrentHighest,Frees,Reserved}) ->
  NewReserved=lists:keydelete(NewFree,2,Reserved),
  {noreply,{CurrentHighest,[NewFree|Frees],NewReserved}}.

