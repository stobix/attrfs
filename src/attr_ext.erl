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
%%% @doc This module deals with extended attribute data base handling.
%%%
%%% @version 1.0

-module(attr_ext).

-export([add_new_attribute/4,
         rehash_ext_from_db/2,
         append_attribute/3,
         generate_ext_info_io/1,
         ext_info_to_ext_io/1]).


-include("../include/attrfs.hrl").
-include_lib("newdebug/include/debug.hrl").


%%--------------------------------------------------------------------------
%% Rereads and rebuilds the extended info for the Path provided,
%%  storing the information in the stat entry of the Inode entry.
%% Input: Inode, Path
%%--------------------------------------------------------------------------
rehash_ext_from_db(Inode,Path) ->
  {ExtInfo,ExtIo,ExtAmount}=generate_ext_info_io(Path),
  {value,Entry}=tree_srv:lookup(Inode,inodes),
  Stat=Entry#inode_entry.stat,
  NewStat=Stat#stat{st_nlink=ExtAmount+1},
  NewEntry=Entry#inode_entry{stat=NewStat,ext_info=ExtInfo,ext_io=ExtIo},
  tree_srv:enter(Inode,NewEntry,inodes).

%%--------------------------------------------------------------------------
%% Adds the extended attribute Attr (in list form) to file with path Path,
%%  inode FIno and file entry FEntry
%% Input: Path, FIno, FEntry, Attr
%%--------------------------------------------------------------------------
add_new_attribute(Path,FIno,FEntry,Attr) ->
	%% Since most calls to this function comes from functions that have
	%% already looked up the file entry for the inode, sending both the
	%% FIno and FEntry here saves a lookup call.
  ?DEB1({ext,3},">add_new_attribute"),
  % Add the new attribute, if non-existent.
  ?DEBL({ext,5},"inserting (~p) ~p into database, if nonexistent",[Path,Attr]),
  length(dets:match(?ATTR_DB,{Path,Attr}))==0 andalso
    (ok=dets:insert(?ATTR_DB,{Path,Attr})),
  ?DEBL({ext,5},"database entry: ~p",[dets:match(?ATTR_DB,{Path,Attr})]),
  #inode_entry{stat=Stat,name=FName}=FEntry,
  ?DEBL({ext,5},"appending attribute ~p for {~p,~p} to the file system",[Attr,FName,FIno]),
  append_attribute(Attr,FName,?UEXEC(Stat)),
  ?DEB1({ext,5},"creating new ext info"),
  rehash_ext_from_db(FIno,Path).




%%--------------------------------------------------------------------------
%%  Adds the extended attribute Attr to the file with internal name FName and
%%   file stat Stat, recursively creating attribute directory parents as needed.
%% Input: Attr, FName, Stat
%%--------------------------------------------------------------------------
append_attribute(Attr,FName,Stat) ->
  ?DEBL({ext,3},"appending ~p/~p",[Attr,FName]),
  append_attribute_dir(Attr,FName,FName,Stat).


%% this function creates directory parents recursively if unexistent, updating
%% the lists of children along the way.

%%--------------------------------------------------------------------------
%%  Adds the extended attribute Parent to the file with internal name FName and
%%   file stat Stat, recursively creating directory parents as needed.
%% Input: Parent, ChildInoName, ChildName,Stat
%%--------------------------------------------------------------------------
append_attribute_dir(Parent,ChildInoName,ChildName,Stat) ->
  ?DEBL({ext,6},"append ~p ~p ~p",[Parent,ChildInoName,ChildName]),
  {ok,ChildIno}=numberer:n2i(ChildInoName,ino),
  % get creates if non-existant.
  ParentIno=numberer:get(Parent,ino),
  {value,ChildEntry}=tree_srv:lookup(ChildIno,inodes),
  ?DEB1({ext,8},"got child entry"),
  ChildType=ChildEntry#inode_entry.type,
  ChildTriplet={ChildName,ChildIno,ChildType},
  % Append child to parent, creating parent entry recursively if neccessary.
  case tree_srv:lookup(ParentIno,inodes) of
    % No entry found, creating new attribute entry before appending child.
    none ->
      ?DEBL({ext,8},"adding new attribute folder ~p with the child ~p",[Parent,ChildName]),
      PEntry=
        #inode_entry{
          type=attribute_dir,
          name=Parent,
          contents=[ChildTriplet],
          stat=attr_tools:dir(Stat#stat{st_ino=ParentIno}),
          ext_info=[],
          ext_io=ext_info_to_ext_io([])
        },
      ?DEB1({ext,8},"entering new entry into server"),
      tree_srv:enter(ParentIno,PEntry,inodes),
	  % This works since attribute folder names are defined recursively.      
	  [PName|GrandParent]=Parent, 
      ?DEBL({ext,8},"checking parent of parent (~p)",[GrandParent]),
      append_attribute_dir(GrandParent,Parent,PName,Stat);
	% Parent already created, just appending child.
    {value,PEntry} ->
      ?DEBL({ext,8},"merging ~p into ~p",[element(1,ChildTriplet),PEntry#inode_entry.name]),
      attr_tools:append_child(ChildTriplet,PEntry)
  end.

%%--------------------------------------------------------------------------
%% Input: Path
%% Output: {Ext info for path from database,Number of ext info items}
%%--------------------------------------------------------------------------
generate_ext_info(Path) ->
  ?DEB2({ext,3},"generating ext info for ~s",Path),
  % Find the path in the data base
  ExtInfo0=dets:match(?ATTR_DB,{Path,'$1'}),
  % Count the number of items
  ExtAmount=lists:foldr(fun(_,N) -> N+1 end,0,ExtInfo0),
  % Flatten the list
  ExtInfo1=convert(ExtInfo0),
  % Return flattened list and number of items
  {attr_tools:merge_duplicates(ExtInfo1),ExtAmount}. 

%%--------------------------------------------------------------------------
%% ext list form -> ext tuple form
%% Input: list of lists of rpn-ish list representations of key-value objects
%% Output: list of key-value-tuples
%% Example: [[["foo","bar","baz"]]] -> [{"baz/bar","foo"}]
%%--------------------------------------------------------------------------
convert(LList) ->
    convert1(attr_tools:flatten1(LList)).

%%--------------------------------------------------------------------------
%% Input: list of lists
%% Output: list of key-value-tuples
%%--------------------------------------------------------------------------
convert1([]) -> [];
convert1([A|As]) ->
    [keyvalue(A)|convert1(As)].



%%--------------------------------------------------------------------------
%% Converts lists of form [H|T] to tuples of form {H,T}.
%% T may be empty, and if so, an empty list is inserted on the T position
%%--------------------------------------------------------------------------
% A key AND value less attribute will make this function crash, and it should!
% A valueless key will have an empty string as value
keyvalue([A|[]]) ->
    {A,""};

% All other attributes are converted from [value,words,key,of,list,long] to {"long/list/of/key/words",value}
keyvalue(A) ->
    lists:foldl(
        fun(X,{[],[]}) ->
            ?DEB2({ext,5},"init: ~p",X),
            {[],X};
           (X,{[],Z}) ->
            {X,Z};
           (X,{Y,Z}) ->
            ?DEBL({ext,5},"recurs: ~p,~p,~p",[X,Y,Z]),
            {attr_tools:binary_concat([X,<<"/">>,Y]),Z}
        end,
        {[],[]},
        A).


%%--------------------------------------------------------------------------
%% Generates extended info in both the internal info tuple format and the fuse io
%%  string format for the specified file path
%%--------------------------------------------------------------------------
generate_ext_info_io(Path) ->
  ?DEB2({ext,3},"generating ext info for ~ts",Path),
  {ExtInfo,ExtAmount}=generate_ext_info(Path),
  ?DEB2({ext,5},"generating ext io for ~p",ExtInfo),
  ExtIo=ext_info_to_ext_io(ExtInfo),
  {ExtInfo,ExtIo,ExtAmount}.

%%--------------------------------------------------------------------------
%% Ext info is a list of key-value-tuples, ext io is a string representation of a binary
%%--------------------------------------------------------------------------
ext_info_to_ext_io(InternalExtInfoTupleList) ->
  ?DEB1({ext,6},"Creating ext_io"),
  ext_info_to_ext_io(InternalExtInfoTupleList,<<>>).

%%--------------------------------------------------------------------------
%% All items processed, return {length of string, string}
%% TODO: can I change the string to an io_string() ?
%%--------------------------------------------------------------------------
ext_info_to_ext_io([],B) -> 
  ?REPORT(ext_info_to_ext_io),
  B0=attr_tools:binary_concat([B,<<0>>]),
  B0len=size(B0),
  ?DEB1({ext,8},"Done creating ext_io"),
  ?DEBL({ext,8},"Final string: \"~p\", size: ~p",[B0,B0len]),
  {B0len,B0};

%%--------------------------------------------------------------------------
%% Generating extended info string for the current attribute, and
%%  adding it to the string to be sent to fuse.
%% TODO: can I change the string to an io_string() ?
%%--------------------------------------------------------------------------
ext_info_to_ext_io([{Name,_}|InternalExtInfoTupleList],Acc) ->
  ?DEB2({ext,8},"Adding zero to end of name ~p, and \"user.\" to the start",Name),
  Name0= attr_tools:binary_concat([<<"user.">>,Name,<<0>>]),
  ?DEB1({ext,8},"Appending name to namelist"),
  NewString=attr_tools:binary_concat([Acc,Name0]),
  ?DEB1({ext,8},"Recursion"),
  ext_info_to_ext_io(InternalExtInfoTupleList,NewString).


