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

-include("../include/attrfs.hrl").
-include_lib("newdebug/include/debug.hrl").

-export([keymergeunique/2,
         remove_from_start/2,
         take_while/3,
         test_access/3,
         merge_duplicates/1,
         dir/1,
         datetime_to_epoch/1,
         statify_file_info/1,
         flatten1/1,
         append_child/2,
         get_or_default/2,
         curr_time_stat/0
        ]).


%%--------------------------------------------------------------------------
%% flatten1 removes one list layer from a file. [[a],[[B]]] -> [a,[B]].
%%--------------------------------------------------------------------------
flatten1([[]]) ->
  [];
flatten1([[A]|As]) ->
  [A|flatten1(As)];
flatten1([A=[_|_s]|As]) ->
  A++flatten1(As);
flatten1([A|As]) ->
  [A|flatten1(As)];


flatten1([]) ->
  [].

-ifdef(EUNIT).
flatten1_test_() ->
 ?_test(
 [
  ?assertMatch([],flatten1([[]])),
  ?assertMatch([a],flatten1([[a]])),
  ?assertMatch([[bar],[baz]],flatten1([[[bar],[baz]]])),
  ?assertMatch(["etaoin","shrdlu"],flatten1([["etaoin","shrdlu"]]))
 ]).
-endif.

%%--------------------------------------------------------------------------
%seems like lists:keymerge won't do what I ask it, so I build my own...
%%--------------------------------------------------------------------------
keymergeunique(Tuple,TupleList) ->
  keymergeunique(Tuple,TupleList,[]).

keymergeunique(Tuple,[],FilteredList) ->
  [Tuple|FilteredList];

keymergeunique(Tuple={Key,_Value,_Type},[{Key,_,_}|TupleList],FilteredList) ->
  keymergeunique(Tuple,TupleList,FilteredList);

keymergeunique(Tuple={_Key,_Value,_Type},[OtherTuple|TupleList],FilteredList) ->
  keymergeunique(Tuple,TupleList,[OtherTuple|FilteredList]).

-ifdef(EUNIT).
keymergeunique_test_() ->
  ?_assertMatch([{b,e,f},{a,b,c}],keymergeunique({b,e,f},[{b,c,d},{a,b,c}])).
-endif.

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

-ifdef(EUNIT).
remove_from_start_test_() ->
 ?_test(
  [
    ?assertMatch("foo",remove_from_start("foo","bar")),
    ?assertMatch("oo",remove_from_start("foo","f"))
  ]).
-endif.

%%--------------------------------------------------------------------------
%% @spec (boolean(),A,B) -> A|B.
%% @doc Takes a boolean and redefines the meanings of true and false.
%% Example: transmogrify(is_foo(X),good,{error,no_foo}) will return either 
%% good or {error, no_foo}, depending on whether is_foo(X) returns true or 
%% false.
%%--------------------------------------------------------------------------
transmogrify(Boolean,NewTrue,NewFalse) ->
  if 
    Boolean -> 
      ?DEBL(5,"transmogrifying ~p into ~p",[Boolean,NewTrue]),
      NewTrue;
    true -> 
      ?DEBL(5,"transmogrifying ~p into ~p",[Boolean,NewFalse]),
      NewFalse
  end.

-ifdef(EUNIT).
transmogrify_test_() ->
  ?_assertMatch(true,transmogrify(transmogrify(true,false,true),false,true)).
-endif.

%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
test_access(Inode,Mask,Ctx) ->
  ?DEB1(5,"checking access..."),
  case tree_srv:lookup(Inode,inodes) of
    {value, Entry} ->
      % Can I use the mask like this?
      case Mask of
        ?F_OK ->
          ?DEB1(7,"file existing"),
          ok;
        _ -> 
          transmogrify(has_rwx_access(Entry,Mask,Ctx),ok,eacces)
      end;
    none ->
      ?DEB1(7,"file does not exist!"),
      enoent
  end.

%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
has_rwx_access(Entry,Mask,Ctx) ->
  #stat{st_mode=Mode,st_uid=Uid,st_gid=Gid}=Entry#inode_entry.stat,
  has_other_perms(Mode,Mask)
    orelse (Gid==Ctx#fuse_ctx.gid andalso has_group_perms(Mode,Mask))
    orelse (Uid==Ctx#fuse_ctx.uid andalso has_user_perms(Mode,Mask)).


-ifdef(EUNIT).
has_rwx_access_rwx_test_() ->
  StatURWX=#stat{st_mode=8#700,st_uid=1,st_gid=2},
  StatGRWX=#stat{st_mode=8#070,st_uid=1,st_gid=2},
  StatORWX=#stat{st_mode=8#007,st_uid=1,st_gid=2},
  U__=#inode_entry{stat=StatURWX},
  _G_=#inode_entry{stat=StatGRWX},
  __O=#inode_entry{stat=StatORWX},
  GU=#fuse_ctx{gid=2,uid=1},
  _U=#fuse_ctx{gid=3,uid=1},
  G_=#fuse_ctx{gid=2,uid=3},
  __=#fuse_ctx{gid=3,uid=3},
  RWX=8#755,
  ___=0,
  _F=false,
  T_=true,
 [{"postive tests",?_test([
  ?assertMatch(T_,has_rwx_access(U__,RWX,GU)),
  ?assertMatch(T_,has_rwx_access(_G_,RWX,GU)),
  ?assertMatch(T_,has_rwx_access(__O,RWX,GU)),

  ?assertMatch(T_,has_rwx_access(U__,RWX,_U)),
  ?assertMatch(_F,has_rwx_access(_G_,RWX,_U)),
  ?assertMatch(T_,has_rwx_access(__O,RWX,_U)),

  ?assertMatch(_F,has_rwx_access(U__,RWX,G_)),
  ?assertMatch(T_,has_rwx_access(_G_,RWX,G_)),
  ?assertMatch(T_,has_rwx_access(__O,RWX,G_)),

  ?assertMatch(_F,has_rwx_access(U__,RWX,__)),
  ?assertMatch(_F,has_rwx_access(_G_,RWX,__)),
  ?assertMatch(T_,has_rwx_access(__O,RWX,__))])},

  {"negative tests",?_test([
  ?assertMatch(_F,has_rwx_access(U__,___,GU)),
  ?assertMatch(_F,has_rwx_access(_G_,___,GU)),
  ?assertMatch(_F,has_rwx_access(__O,___,GU)),

  ?assertMatch(_F,has_rwx_access(U__,___,_U)),
  ?assertMatch(_F,has_rwx_access(_G_,___,_U)),
  ?assertMatch(_F,has_rwx_access(__O,___,_U)),

  ?assertMatch(_F,has_rwx_access(U__,___,G_)),
  ?assertMatch(_F,has_rwx_access(_G_,___,G_)),
  ?assertMatch(_F,has_rwx_access(__O,___,G_)),

  ?assertMatch(_F,has_rwx_access(U__,___,__)),
  ?assertMatch(_F,has_rwx_access(_G_,___,__)),
  ?assertMatch(_F,has_rwx_access(__O,___,__))])}
 ].
-endif.


-ifdef(EUNIT).
perms_test_() ->
  ARWX=8#777,
  A___=0,
  _F=false,
  T_=true,
  [{"other",?_test([
  ?assertMatch(_F,has_other_perms(ARWX,A___)),
  ?assertMatch(T_,has_other_perms(ARWX,ARWX))
  ])},

  {"group",?_test([
  ?assertMatch(_F,has_group_perms(ARWX,A___)),
  ?assertMatch(T_,has_group_perms(ARWX,ARWX))
  ])},

  {"user",?_test([
  ?assertMatch(_F,has_user_perms(ARWX,A___)),
  ?assertMatch(T_,has_user_perms(ARWX,ARWX))
  ])}].
-endif.

%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
has_other_perms(Mode,Mask) ->
  (Mode band ?S_IRWXO band Mask)/=0.

%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
has_group_perms(Mode,Mask) ->
  (Mode band ?S_IRWXG band Mask)/=0.

%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
has_user_perms(Mode,Mask) ->
  (Mode band ?S_IRWXU band Mask)/=0.


%%--------------------------------------------------------------------------
%% Takes in a [{"foo","bar"},{"foo","baz"},{"etaoin","shrdlu"}] and returns 
%%  a [{"foo","bar,baz"},{"etaoin","shrdlu"}]
%%--------------------------------------------------------------------------
%% Algorithm: enter each tuple into a gb_tree, merging if item exists. Finally convert to list and return.
%% or enter each tuple into a dict, adding new values as I go along.
%% or create a tree with my own insertion algorithm.

merge_duplicates(List) ->
  Tree=make_unduplicate_tree(List),
  gb_trees:to_list(Tree).

-ifdef(EUNIT).
merge_duplicates_test_() ->
  Single={"a","b"},
  Duplicate1={"c","d"},
  Duplicate2={"c","e"},
  Merged={"c","d,e"},
  InData=[Duplicate1,Duplicate2,Single],
  OutData=[Single,Merged],
  ?_assertMatch(OutData,merge_duplicates(InData)).
-endif.
%%--------------------------------------------------------------------------
%% Takes a [{Key,Val}] and transforms it into a gb_tree where each key only has one val.
%% If the list contains several identical keys, their values are combined using ","as a separator.
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
  ?DEBL(5,"transforming mode ~.8B into mode ~.8B",[Stat#stat.st_mode,NewMode]),
  Stat#stat{st_mode=NewMode}.


%%--------------------------------------------------------------------------
%% returns a stat with the current time in it.
%%--------------------------------------------------------------------------
curr_time_stat() ->
  {MegaNow,NormalNow,_}=now(),
  Now=MegaNow*1000000+NormalNow,
    #stat{
      st_atime=Now,
      st_ctime=Now,
      st_mtime=Now
    }.

%%--------------------------------------------------------------------------
%% datetime_to_epoch takes a {{Y,M,D},{H,M,S}} and transforms it into seconds elapsed from 1970/1/1 00:00:00, GMT
%%--------------------------------------------------------------------------
datetime_to_epoch(DateTime) ->
  calendar:datetime_to_gregorian_seconds(DateTime) - calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}).

-ifdef(EUNIT).
datetime_to_epoch_test_() ->
  ?_assertMatch(1234567890,datetime_to_epoch({{2009,02,13},{23,31,30}})). %best date ever!
-endif.

%%--------------------------------------------------------------------------
%% statify_file_info transforms a file.#file_info{} into a fuserl.#stat{}
%%--------------------------------------------------------------------------
statify_file_info(#file_info{size=Size,type=_Type,atime=Atime,ctime=Ctime,mtime=Mtime,access=_Access,mode=Mode,links=Links,major_device=MajorDevice,minor_device=MinorDevice,inode=Inode,uid=UID,gid=GID}) ->
  ?DEBL(7,"converting file info for ~p (, which is of size ~p) to fuse stat info",[Inode,Size]),
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

%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
append_child(NewChild={_ChildName,_ChildIno,_ChildType},ParentEntry=#inode_entry{}) ->
  PName=ParentEntry#inode_entry.name,
  ?DEBL(7,"»append_child: Child: ~p Parent: ~p",[element(1,NewChild),ParentEntry#inode_entry.name]),
  Children=ParentEntry#inode_entry.contents,
  ?DEB2(9,"children: ~p",Children),
  NewChildren=attr_tools:keymergeunique(NewChild,Children),
  ?DEB2(9,"merged children: ~p",NewChildren),
  NewParentEntry=ParentEntry#inode_entry{contents=NewChildren},
  ?DEB1(9,"created new parent entry"),
  {ok,ParentIno}=inode:n2i(PName,ino),
  ?DEB2(9,"parent inode: ~p",ParentIno),
  tree_srv:enter(ParentIno,NewParentEntry,inodes),
  ?DEB1(9,"new parent inserted"),
  ok;

append_child(NewChild={_ChildName,_ChildIno,_ChildType},ParentIno) ->
  ?DEBL(7,"»append_child: ~p (~p)",[NewChild,ParentIno]),
  case tree_srv:lookup(ParentIno,inodes) of
    {value,ParentEntry} -> 
        ?DEB1(9,"got parent entry"),
        append_child(NewChild,ParentEntry);
    none ->
        ?DEB1(9,"DID NOT get parent entry"),
        throw({error,{parent_unbound,ParentIno}})
  end.


%%--------------------------------------------------------------------------
%% Returns the value associated with Attribute, if found, and Default if not.
%%--------------------------------------------------------------------------
get_or_default(Attribute,Default) ->
  case options:get(attrfs,Attribute) of
    {ok,Value} ->
      ?DEBL(9,"~p: ~p",[Attribute,Value]),
      Value;
    undefined ->
      ?DEBL(9,"~p not defined. Defaulting to ~p", [Attribute,Default]),
      Default
  end.
