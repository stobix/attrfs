-module(inode).
%%%=========================================================================
%%% Module inode
%%%=========================================================================
%%% @author Joel Ericson <kasettbok@gmail.com>
%%%
%%% @copyright Copylefted using some GNU license or other.
%%%
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

-ifdef(test).

-include_lib("eunit/include/eunit.hrl").

-endif.

-behaviour(gen_server).

-export([get/1,release/2]).
-export([n2i/2,i2n/2,name/3,number/2]).
-export([get/2]).
-export([is_named/2,is_numbered/2]).
-export([is_used/2]).
-export([reset/1,reset/2,list_bound/1]).
-export([rename/3]).
-export([count_occupied/1]).
-export([initiate/1,initiate/2]).


-export([start_link/0,start_link/1,init/1]).
-export([handle_call/3,handle_cast/2]).
-export([terminate/2]).
-export([handle_info/2,code_change/3]).

-export([start/2,stop/1]).

-vsn("0.2.1").

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
  ?DEB1({inode,1},"Starting inode server"),
  gen_server:start_link({local,?MODULE},?MODULE,SmallestNumber,[]).

init(_SmallestNumber) ->
  {ok,gb_trees:empty()}.
%  {ok,{SmallestNumber,[],[]}}.

terminate(_Reason,_State) -> ok.

code_change(_,_,_) -> ok.
handle_info(_,_) -> ok.

%%%=========================================================================
%%% exports
%%%=========================================================================


%%----------------------------------------------
%% @doc initates a new inode server.
%% @spec (Server::string()|atom()|integer()) -> ok
%%       If server is already initiated, nothing is done.
%% @end
%%----------------------------------------------
initiate(Server) ->
  gen_server:cast(?MODULE,{init,1,Server}).

%%----------------------------------------------
%% @doc initates a new inode server that starts numbering inodes at N.
%%       If server is already initiated, nothing is done.
%% @spec (N::integer(),Server::string()|atom()|integer()) -> ok
%% @end
%%----------------------------------------------
initiate(N,Server) ->
  gen_server:cast(?MODULE,{init,N,Server}).


%%----------------------------------------------
%% @doc counts the number of occupied inodes.
%% @spec (server()) -> non_neg_integer().
%% @end
%%----------------------------------------------
count_occupied(Server) ->
  gen_server:call(?MODULE,{count,Server}).

%%----------------------------------------------
%% @doc Returns an unused integer.
%% @spec (server()) -> Number::unique_integer().
%% @end
%%----------------------------------------------
get(Server) ->
  gen_server:call(?MODULE,{get,Server}).

%%----------------------------------------------
%% @doc On first run, assosicate a unique integer with Name.
%%     On sequential runs, return the integer associated with Name.
%% @spec (term(),server())-> Number::non_neg_integer().
%% @end
%%----------------------------------------------
get(Name,Server) ->
  case is_numbered(Name,Server) of
    false -> 
      Number=?MODULE:get(Server),
      ?DEBL({inode,2},"binding ~p to ~p",[Name,Number]),
      gen_server:cast(?MODULE,{register,Name,Number,Server}),
      Number;
    Number -> Number
  end.



%%----------------------------------------------
%% @doc Returns the inode number associated with Name.
%%     Crashes if no association exists.
%% @spec (term(),server())-> Number::non_neg_integer()|{error, {not_numbered,Name}}.
%% @end
%%----------------------------------------------
n2i(Name,Server) ->
  case is_numbered(Name,Server) of
    false ->
      {error, {not_numbered,Name}};
    True ->
      {ok,True}
  end.

%%----------------------------------------------
%% @doc Returns the inode number associated with Name.
%%     Crashes if no association exists.
%% @spec (term(),server())-> Name::Term()|{error, {not_named,Number}}.
%% @end
%%----------------------------------------------

i2n(Number,Server) ->
  case is_named(Number,Server) of
    false ->
      {error, {not_named,Number}};
    True ->
      {ok,True}
  end.


%%----------------------------------------------
%% @doc Creates a link between a name and an inode number.
%%     Crashes if the inode number is already bound.
%% @spec (term(),server())-> {ok, Number::non_neg_integer()} | { error, {is_numbered,Name}}.
%% @end
%%----------------------------------------------
number(Name,Server) ->
  case is_numbered(Name,Server) of
    false ->
      {ok,?MODULE:get(Name,Server)};
    True ->
      {error, {is_numbered, {Name,True}}}
  end.

%%----------------------------------------------
%% @doc Creates a link between a name and an inode number.
%%     Crashes if the inode number is already bound.
%% @spec (non_neg_integer(),term(),server())-> ok | {error, {is_named,Name,NamedNumber::non_neg_intege()},Number}.
%% @end
%%----------------------------------------------
name(Number,Name,Server) ->
  case is_named(Number,Server) of
    false ->
      gen_server:cast(?MODULE,{register,Name,Number,Server});
    True ->
      {error,{is_named,{Number,True},Name}}
  end.

%%----------------------------------------------
%% @doc Checks whether Number has a named associated with it. Returs either false, or the associated name.
%% @spec (non_neg_integer(),server()) -> false|Name::term().
%% @end
%%----------------------------------------------
is_named(Number,Server) ->
  gen_server:call(?MODULE,{is_named,Number,Server}).

%%----------------------------------------------
%% @doc Substitute the OldName-OldInode link with a NewName-OldInode link.
%% @spec (term(),server())-> ok
%% @end
%%----------------------------------------------
rename(OldName,NewName,Server) ->
  gen_server:cast(?MODULE,{rename,OldName,NewName,Server}).

%%----------------------------------------------
%% @doc Checks whether Name has a number associated with it. Returs either false, or the associated number.
%% @spec (term(),server()) -> false|Name::uniqe_integer().
%% @end
%%----------------------------------------------
is_numbered(Name,Server) ->
  gen_server:call(?MODULE,{is_numbered,Name,Server}).

%%----------------------------------------------
%% @doc Checks whether Number is currently in use. Returns either false or true.
%% @spec (term(),server()) -> bool().
%% @end
%%----------------------------------------------
is_used(Number,Server) ->
  gen_server:call(?MODULE,{is_used,Number,Server}).

%%----------------------------------------------
%% @doc Makes the number Number no longer occupied.
%%      Releases any name association with Number, and makes Number free to get returned by a call to get/0 or get/1.
%% @spec (term(),server()) -> false|Name::unique_integer().
%% @end
%%----------------------------------------------
release(Number,Server) ->
  gen_server:cast(?MODULE,{return,Number,Server}).

%%----------------------------------------------
%% @doc Resets the counter. Forgets all name bindings.
%% @spec (server()) -> ok.
%% @end
%%----------------------------------------------
reset(Server) ->
  gen_server:cast(?MODULE,{reset,1,Server}).

%%----------------------------------------------
%% @doc Resets the counter. Forgets all name bindings. Starts anew at number Number.
%% @spec (Number,server()) -> ok.
%% @end
%%----------------------------------------------
reset(Number,Server) ->
  gen_server:cast(?MODULE,{reset,Number,Server}).

%%----------------------------------------------
%% @doc Lists all integers bound to a term.
%% @spec (server()) -> [{term(),unique_integer()}].
%% @end
%%----------------------------------------------
list_bound(Server) ->
  gen_server:call(?MODULE,{list,Server}).

%%%=========================================================================
%%% gen_server callback functions.
%%%=========================================================================

handle_call({count,Server},_From,State) ->
  {value,S}=gb_trees:lookup(Server,State),
  {reply,count_(S),State};


handle_call({list,Server},_From,State) ->
  {value,S}=gb_trees:lookup(Server,State),
  {reply,list_(S),State};


handle_call({is_used,Number,Server},_From,State) ->
  {value,S}=gb_trees:lookup(Server,State),
  {reply,is_used_(Number,S),State};



handle_call({is_numbered,Name,Server},_From,State) ->
  {value,S}=gb_trees:lookup(Server,State),
  {reply,is_numbered_(Name,S),State};



handle_call({is_named,Number,Server},_From,State) ->
  {value,S}=gb_trees:lookup(Server,State),
  {reply,is_named_(Number,S),State};


%-- modifying calls/casts --

handle_call({get,Server},_From,State) ->
  {value,S}=gb_trees:lookup(Server,State),
  {Reply,NewS}=get__(S),
  {reply,Reply,gb_trees:enter(Server,NewS,State)}.

handle_cast({rename,OldName,NewName,Server},State) ->
  {value,S}=gb_trees:lookup(Server,State),
  {noreply,gb_trees:enter(Server,rename__(OldName,NewName,S),State)};


handle_cast({reset,N,Server},State) ->
  {value,S}=gb_trees:lookup(Server,State),
  {noreply,gb_trees:enter(Server,reset__(N,S),State)};

handle_cast({register,Name,Number,Server},State) ->
  {value,S}=gb_trees:lookup(Server,State),
  {noreply,gb_trees:enter(Server,register__(Name,Number,S),State)};

handle_cast({return,NewFree,Server},State) ->
  {value,S}=gb_trees:lookup(Server,State),
  {noreply,gb_trees:enter(Server,return__(NewFree,S),State)};


handle_cast({init,N,Server},State) ->
  case gb_trees:lookup(Server,State) of
    {value,_} -> {noreply,State};
    none -> {noreply,gb_trees:enter(Server,init__(N),State)}
  end.

%-- internal functions --

count_({CurrHigh,Frees,Reserved}) ->
  CurrHigh-length(Frees)+length(Reserved)-1.


list_({_,_,Reserved}) ->
  Reserved.

is_used_(Number,{CurrentHighest,Frees,Reserved})->
  not lists:member(Number,Frees) 
    orelse not lists:keymember(Number,2,Reserved)
    orelse Number >= CurrentHighest.

is_numbered_(Name,{_,_,Reserved}) ->
    case lists:keyfind(Name,1,Reserved) of
      false -> false;
      {Name,Number} -> Number
    end.

is_named_(Number,{_,_,Reserved}) ->
    case lists:keyfind(Number,2,Reserved) of
      false -> false;
      {Name,Number} -> Name
    end.

get__({CurrentHighest,[],Reserved}) ->
  {CurrentHighest,{CurrentHighest+1,[],Reserved}};

get__({CurrentHighest,[Free|Frees],Reserved}) ->
  {Free, {CurrentHighest,Frees,Reserved}}.

rename__(OldName,NewName,{CurrentHighest,Frees,Reserved}=Status) ->
  case lists:keytake(OldName,1,Reserved) of
    {value,{OldName,OldIno},NewReserved} ->
      {CurrentHighest,Frees,[{NewName,OldIno}|NewReserved]};
    false ->
      Status
  end.

reset__(N,_) ->
  {N,[],[]}.

init__(N) ->
  {N,[],[]}.

register__(Name,Number,{CurrentHighest,Frees,Reserved}) ->
  {CurrentHighest,Frees,[{Name,Number}|Reserved]}.

return__(NewFree, {CurrentHighest,Frees,Reserved}) ->
  NewReserved=lists:keydelete(NewFree,2,Reserved),
  {CurrentHighest,[NewFree|Frees],NewReserved}.


-ifdef(EUNIT).

get_test_() ->
    initiate(get_test),
    I=?MODULE:get(foo,get_test),
  ?_test([
    ?assertMatch(I,inode:get(foo,get_test)),
    ?assertMatch({ok,I},n2i(foo,get_test))
  ]).

name_number_test_() ->
  initiate(name_number_test),
  reset(name_number_test),
  Name=foo,Number=1,
  name(Number,Name,name_number_test),
  ?_test([
    ?assertMatch({ok,Number},n2i(Name,name_number_test)),
    ?assertMatch({ok,Name},i2n(Number,name_number_test)),
    ?assertMatch({error,{is_numbered,{Name,Number}}},number(Name,name_number_test)),
    ?assertMatch({error,{is_named,{Number,Name},Name}},name(Number,Name,name_number_test)),
    ?assertMatch(ok,rename(Name,Name,name_number_test)),
    ?assertMatch({ok,Number},n2i(Name,name_number_test)),
    ?assertMatch({ok,Name},i2n(Number,name_number_test))
  ]).
  

-endif.
