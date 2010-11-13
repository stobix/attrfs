-module(attr_tools).

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
-include("../include/attrfs.hrl").
-include("../include/debug.hrl").

-export([keymergeunique/2,
         remove_from_start/2,
         take_while/3,
         test_access/3,
         merge_duplicates/1,
         dir/1,
         datetime_to_epoch/1,
         statify_file_info/1,
         flatten1/1
        ]).


%%--------------------------------------------------------------------------
%% flatten1 removes one list layer from a file. [[a],[[B]]] -> [a,[B]].
%%--------------------------------------------------------------------------
flatten1([[A]|As]) ->
  [A|flatten1(As)];

flatten1([A|As]) ->
  [A|flatten1(As)];

flatten1([]) ->
  [].

%%--------------------------------------------------------------------------
%seems like lists:keymerge won't do what I ask it, so I build my own...
%%--------------------------------------------------------------------------
keymergeunique(Tuple,TupleList) ->
  keymerge(Tuple,TupleList,[]).

keymerge(Tuple,[],FilteredList) ->
  [Tuple|FilteredList];

keymerge(Tuple={Key,_Value},[{Key,_}|TupleList],FilteredList) ->
  keymerge(Tuple,TupleList,FilteredList);

keymerge(Tuple={_Key,_Value},[OtherTuple|TupleList],FilteredList) ->
  keymerge(Tuple,TupleList,[OtherTuple|FilteredList]).

%%--------------------------------------------------------------------------
%% (This one I stole from fuserlproc.)
%% this take_while runs the function F until it returns stop
%% Good for taking items from list where the number of list items taken
%% are dependent on the individual size of each item.
%%--------------------------------------------------------------------------
take_while (_, _, []) -> 
  [];
take_while (F, Acc, [ H | T ]) ->
  case F (H, Acc) of
    { continue, NewAcc } ->
      [ H | take_while (F, NewAcc, T) ];
    stop ->
      []
  end.

%%--------------------------------------------------------------------------
%% removes string2 from the beginning of string1, if applicable. Returns what was left of string1
%%--------------------------------------------------------------------------
remove_from_start(String1,[]) -> String1;
remove_from_start([],_String2) -> []; %String2;
remove_from_start([C1|String1],[C2|String2]) when C1 == C2 ->
  remove_from_start(String1,String2);
remove_from_start([C1|String1],[C2|_String2]) when C1 /= C2 ->
  [C1|String1].

%%--------------------------------------------------------------------------
%% @spec (boolean(),A,B) -> A|B.
%% @doc Takes a boolean and redefines the meanings of true and false.
%% Example: transmogrify(is_foo(X),good,{error,no_foo}) will return either 
%% good or {error, no_foo}, depending on whether is_foo(X) returns true or 
%% false.
%%--------------------------------------------------------------------------
transmogrify(TrueOrFalse,NewTrue,NewFalse) ->
  if 
    TrueOrFalse -> 
      ?DEBL("   transmogrifying ~p into ~p",[TrueOrFalse,NewTrue]),
      NewTrue;
    true -> 
      ?DEBL("   transmogrifying ~p into ~p",[TrueOrFalse,NewFalse]),
      NewFalse
  end.

%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
test_access(Inode,Mask,Ctx) ->
  ?DEB1("   checking access..."),
  case tree_srv:lookup(Inode,inodes) of
    {value, Entry} ->
      % Can I use the mask like this?
      case Mask of
        ?F_OK ->
          ?DEB1("   file existing"),
          ok;
        _ -> 
          transmogrify(has_rwx_access(Entry,Mask,Ctx),ok,eacces)
      end;
    none ->
      ?DEB1("   file does not exist!"),
      enoent
  end.

%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
has_rwx_access(Entry,Mask,Ctx) ->
  #stat{st_mode=Mode,st_uid=Uid,st_gid=Gid}=Entry#inode_entry.stat,
  has_other_perms(Mode,Mask)
    orelse Gid==Ctx#fuse_ctx.gid andalso has_group_perms(Mode,Mask)
    orelse Uid==Ctx#fuse_ctx.uid andalso has_user_perms(Mode,Mask).

%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
has_other_perms(Mode,Mask) ->
  Mode band ?S_IRWXO band Mask/=0.

%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
has_group_perms(Mode,Mask) ->
  Mode band ?S_IRWXG bsr 3 band Mask/=0.

%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
has_user_perms(Mode,Mask) ->
  Mode band ?S_IRWXU bsr 6 band Mask/=0.


%%--------------------------------------------------------------------------
%% Takes in a [{"foo","bar},{"foo","baz"},{"etaoin","shrdlu"}] and returns 
%%  a [{"foo","bar,baz"},{"etaoin","shrdlu"}]
%%--------------------------------------------------------------------------
%% Algorithm: enter each tuple into a gb_tree, merging if item exists. Finally convert to list and return.
%% or enter each tuple into a dict, adding new values as I go along.
%% or create a tree with my own insertion algorithm.

merge_duplicates(List) ->
  ?DEB2(" indata: ~p",List),
  Tree=make_unduplicate_tree(List),
  gb_trees:to_list(Tree).


%%--------------------------------------------------------------------------
%% Takes a [{Key,Val}] and transforms it into a gb_tree where each key only has one val.
%% If the list contains several identical keys, their values are combined using "," as a separator.
%%--------------------------------------------------------------------------
make_unduplicate_tree(List) ->
  make_unduplicate_tree(List,gb_trees:empty()).

make_unduplicate_tree([],Tree) -> Tree;

make_unduplicate_tree([{Key,Value}|Items],Tree) ->
  Values=
    case gb_trees:lookup(Key,Tree) of
      {value,Vals} ->
        Vals++","++Value;
      none ->
        Value
    end,
  NewTree=gb_trees:enter(Key,Values,Tree),
  make_unduplicate_tree(Items,NewTree).

%%--------------------------------------------------------------------------
%% makes the stat a directory stat
%%--------------------------------------------------------------------------
dir(Stat) ->
  NewMode= ?STD_DIR_MODE,
  ?DEBL("   transforming mode ~.8B into mode ~.8B",[Stat#stat.st_mode,NewMode]),
  Stat#stat{st_mode=NewMode}.

%%--------------------------------------------------------------------------
%% datetime_to_epoch takes a {{Y,M,D},{H,M,S}} and transforms it into seconds elapsed from 1970/1/1 00:00:00, GMT
%%--------------------------------------------------------------------------
datetime_to_epoch(DateTime) ->
  calendar:datetime_to_gregorian_seconds(DateTime) - calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}).

%%--------------------------------------------------------------------------
%% statify_file_info transforms a file.#file_info{} into a fuserl.#stat{}
%%--------------------------------------------------------------------------
statify_file_info(#file_info{size=Size,type=_Type,atime=Atime,ctime=Ctime,mtime=Mtime,access=_Access,mode=Mode,links=Links,major_device=MajorDevice,minor_device=MinorDevice,inode=Inode,uid=UID,gid=GID}) ->
  ?DEBL("    converting file info for ~p (, which is of size ~p) to fuse stat info",[Inode,Size]),
  #stat{
    st_dev= {MajorDevice,MinorDevice},
    st_ino=Inode,
    st_mode=Mode,
    st_nlink=Links,
    st_uid=UID,
    st_gid=GID,
    %st_rdev
    st_size=Size,
    %st_blksize,
    %st_blocks,
    st_atime=datetime_to_epoch(Atime),
    st_mtime=datetime_to_epoch(Mtime),
    st_ctime=datetime_to_epoch(Ctime)
  }.

