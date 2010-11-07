-module(attr_open).

-include("../include/attrfs.hrl").
-include("../include/debug.hrl").

-export([init/0,
       lookup/1,
       set/3,
       remove/1,
       forget/1]).

init()->
  ets:new(open_files,[named_table,set,public]).
%%--------------------------------------------------------------------------
%% lookup_open_file gets the open file corresponding to the inode provided.
%% returns like gb_trees:lookup
%%--------------------------------------------------------------------------
lookup(FIno) ->
  case ets:match(open_files,{{FIno,'_'},'$1'}) of
    "" -> none;
    [[File]] -> {value,File}
  end.

%%--------------------------------------------------------------------------
%% set_open_file returns a state with the open file for the provided inode
%% changed to the FileContents provided.
%%--------------------------------------------------------------------------
set(FIno,Inode,FileContents) ->
  ets:insert(open_files,{{FIno,Inode},FileContents}).

%%--------------------------------------------------------------------------
%% remove_open_file removes the file from the current context. Used for closedir.
%%--------------------------------------------------------------------------
remove(FIno) ->
  ets:match_delete(open_files,{{FIno,'_'},'_'}).

%%--------------------------------------------------------------------------
%% forget_open_file removes an open file from the table, for all Ctx-es.
%%--------------------------------------------------------------------------
forget(Inode) ->
  ets:match_delete(open_files,{{'_',Inode},'_'}).

