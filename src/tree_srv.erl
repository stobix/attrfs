-module(tree_srv).
-vsn("$Header$").
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

-export([enter/3,store/2,new/1,lookup/2,to_list/1,clear/1,delete_any/2]).


-export([start_link/0,init/1]).
-export([handle_call/3,handle_cast/2]).
-export([terminate/2]).

-include("../include/debug.hrl").

%%%=========================================================================
%%% Type specifications
%%%=========================================================================
%%% 
%%% @type uniqe_integer() = non_neg_integer(). An integer whose value is not the same as any other currently in use.
%%% @end
%%%=========================================================================



%%%=========================================================================
%%% gen_server functions
%%%=========================================================================

start_link() ->
  ?DEB1("Starting tree server"),
  gen_server:start_link({local,?MODULE},?MODULE,[],[]).

init(_) ->
  ?DEB1("Tree server started"),
  {ok,[]}.

terminate(_Reason,_State) -> ok.

%%%=========================================================================
%%% exports
%%%=========================================================================


%%----------------------------------------------
%% @doc Stores a gb_trees tree under id TreeID for later use.
%% Returns with an error if the TreeID is already in use.
%% @spec (term(),gb_trees())-> ok|{error,exists}
%% @end
%%----------------------------------------------
store(TreeID,Tree) ->
  gen_server:call(?MODULE,{store_tree,TreeID,Tree}).

% TODO: Make this work in a parallel environment.
%%----------------------------------------------
%% @doc Updates the entry Entry with key Key in the tree TreeID using gb_trees:enter
%% @spec (term(),term(),gb_trees())-> ok.
%% @end
%%----------------------------------------------
enter(Key,Entry,TreeID) ->
  gen_server:cast(?MODULE,{update,Key,Entry,TreeID}). 


%%----------------------------------------------
%% @doc creates a new tree and associates it with TreeID.
%% @spec (term(),term(),gb_trees())-> ok|{error,exists}
%% @end
%%----------------------------------------------
new(TreeID) ->
  gen_server:call(?MODULE,{store_tree,TreeID,gb_trees:empty()}).

%%----------------------------------------------
%% @doc returns the entry Entry with key Key in the tree TreeID.
%% @spec (term(),term(),gb_trees())-> {value,Entry}|none.
%% @end
%%----------------------------------------------
lookup(Key,TreeID) ->
  gen_server:call(?MODULE,{get,Key,TreeID}).

to_list(TreeID) ->
  gen_server:call(?MODULE,{to_list,TreeID}).

clear(TreeID) ->
  gen_server:cast(?MODULE,{clear,TreeID}).

delete_any(Key,TreeID) ->
  gen_server:cast(?MODULE,{remove,Key,TreeID}).

%%%=========================================================================
%%% gen_server callback functions.
%%%=========================================================================

handle_call({to_list,TreeID},_From,Trees) ->
  {TreeID,Tree}=lists:keyfind(TreeID,1,Trees),
  {reply,gb_trees:to_list(Tree),Trees};

handle_call({get,Key,TreeID},_From,Trees) ->
  {TreeID,Tree}=lists:keyfind(TreeID,1,Trees),
  % TODO: In the parallel version, keep track of which keys are taken, and implement some kind of semaphoric thingie.
  {reply,gb_trees:lookup(Key,Tree),Trees};

handle_call({store_tree,TreeID,Tree},_From,Trees) ->
  ?DEBL("  storing a tree ~p into ~p",[TreeID,Trees]),
  case lists:keymember(TreeID,1,Trees) of
    false -> {reply,ok,[{TreeID,Tree}|Trees]};
    true ->  {reply,{error,exists},Trees}
  end.



handle_cast({remove,Key,TreeID},Trees) ->
  {TreeID,Tree}=lists:keyfind(TreeID,1,Trees),
  NewTree=gb_trees:delete_any(Key,Tree),
  NewTrees=lists:keymerge(1,[{TreeID,NewTree}],Trees),
  {noreply,NewTrees};

handle_cast({clear,TreeID},Trees) ->
  case lists:keymember(TreeID,1,Trees) of
    false -> {noreply,[{TreeID,gb_trees:empty()}|Trees]};
    true -> {noreply,lists:keymerge([{TreeID,gb_trees:empty()}],1,Trees)}
  end;

handle_cast({update,Key,Entry,TreeID},Trees) ->
  case lists:keyfind(TreeID,1,Trees) of
    false -> {noreply,Trees};
    {TreeID,Tree} ->
      NewTree=gb_trees:enter(Key,Entry,Tree),
      NewTrees=lists:keymerge(1,[{TreeID,NewTree}],Trees),
      {noreply,NewTrees}
  end.

