-module(tree_srv).
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

-behaviour(gen_server).

-export([enter/3,store/2,new/1,lookup/2,to_list/1,clear/1]).


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
    gen_server:call(?MODULE,{new,TreeID}).

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


handle_call({new,TreeID},_From,Trees) ->
    case lists:keymember(TreeID,1,Trees) of
        false -> {reply,ok,[{TreeID,gb_trees:empty()}|Trees]};
        true ->  {reply,{error,exists},Trees}
    end;

handle_call({store_tree,TreeID,Tree},_From,Trees) ->
    case lists:keymember(TreeID,1,Trees) of
        false -> {reply,ok,[{TreeID,Tree}|Trees]};
        true ->  {reply,{error,exists},Trees}
    end.


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
            



