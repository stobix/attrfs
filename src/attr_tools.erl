-module(attr_tools).

-include("../include/attrfs.hrl").
-include("../include/debug.hrl").

-export([keymergeunique/2,
         remove_from_start/2,
         take_while/3,
         test_access/3
        ]).

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


