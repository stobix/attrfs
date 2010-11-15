-module(attr_ext).

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
-export([add_new_attribute/4,
         rehash_ext_from_db/2,
         append_attribute/3,
         generate_ext_info_io/1,
         ext_info_to_ext_io/1,
         keyvalue/1]).


-include("../include/attrfs.hrl").
-include("../include/debug.hrl").


%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
rehash_ext_from_db(Inode,Path) ->
  {ExtInfo,ExtIo}=generate_ext_info_io(Path),
  {value,Entry}=tree_srv:lookup(Inode,inodes),
  NewEntry=Entry#inode_entry{ext_info=ExtInfo,ext_io=ExtIo},
  tree_srv:enter(Inode,NewEntry,inodes).

%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
add_new_attribute(Path,FIno,FEntry,Attr) ->
  ?DEB1("  >add_new_attribute"),
  % Add the new attribute, if non-existent.
  ?DEBL("   inserting (~p) ~p into database, if nonexistent",[Path,Attr]),
  length(dets:match(?ATTR_DB,{Path,Attr}))==0 andalso
    (ok=dets:insert(?ATTR_DB,{Path,Attr})),
  ?DEBL("   database entry: ~p",[dets:match(?ATTR_DB,{Path,Attr})]),
  #inode_entry{stat=Stat,name=FName}=FEntry,
  ?DEBL("   appending ~p for {~p,~p} to the file system",[Attr,FName,FIno]),
  append_attribute(Attr,FName,?UEXEC(Stat)),
  ?DEB1("   creating new ext info"),
  rehash_ext_from_db(FIno,Path).




%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
append_attribute(Parent,Name,Stat) ->
  ?DEBL("    appending ~p/~p",[Parent,Name]),
  append([Name|Parent],Stat),
  %%XXX: Is this still valid?
  tree_srv:enter(Parent,inode:get(Parent,ino),keys),
  %tree_srv:enter(getkey(Parent),inode:get(Parent,ino),keys),
%  AttributesFolderIno=inode:get(?ATTR_FOLDR,ino),
  AttributesFolderIno=inode:get([],ino),
  ?DEB1("   getting attribute folder inode entry"),
  {value,AttrEntry}=tree_srv:lookup(AttributesFolderIno,inodes),
  ?DEB1("   getting attribute folder children"),
  AttrChildren=tree_srv:to_list(keys), 
  ?DEB1("   creating new inode entry"),
  NewAttrEntry=AttrEntry#inode_entry{children=AttrChildren},
  ?DEB2("   children of new attr entry: ~p",NewAttrEntry#inode_entry.children),
  tree_srv:enter(AttributesFolderIno,NewAttrEntry,inodes).



%% Is this function useful?
getkey([A|[]]) -> A;
getkey([A,B])->getkey(B).


%% this function creates directory parents recursively if unexistent, updating
%% the lists of children along the way.

append([],Stat) ->
  ?DEB1("  done checking parent dirs");


append(ChildName=[Child|Parent],Stat) ->
  ChildIno=inode:get(ChildName,ino),
  ParentIno=inode:get(Parent,ino),
  case tree_srv:lookup(Parent,inodes) of
    % No entry found, creating new attribute entry.
    none ->
      ?DEBL("   adding new attribute folder ~p",[ChildName]),
      PEntry=
        #inode_entry{
          type=attribute_dir,
          name=Parent,
          children=[{ChildName,ChildIno}],
          stat=attr_tools:dir(Stat#stat{st_ino=ParentIno}),
          ext_info=[],
          ext_io=ext_info_to_ext_io([])
        },
      ?DEB1("  entering new entry into server"),
      tree_srv:enter(ParentIno,PEntry,inodes),
      ?DEB1("  checking parent"),
      append(Parent,Stat);
    {value,OldEntry} ->
      ?DEBL("   merging ~p into ~p",[{ChildName,ChildIno},OldEntry#inode_entry.children]),
      NewChildren=attr_tools:keymergeunique({ChildName,ChildIno},OldEntry#inode_entry.children),
      NewEntry=OldEntry#inode_entry{children=NewChildren},
      tree_srv:enter(ParentIno,NewEntry,inodes)
  end.

%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
generate_ext_info(Path) ->
  ?DEB2("   generating ext info for ~p",Path),
  ExtInfo0=dets:match(?ATTR_DB,{Path,'$1'}), 
  ExtInfo1=convert(ExtInfo0),
  attr_tools:merge_duplicates(ExtInfo1). % Going from a list of lists of tuples to a list of tuples.


convert(LList) ->
    convert1(attr_tools:flatten1(LList)).


convert1([]) -> [];
convert1([A|As]) ->
    [keyvalue(A)|convert1(As)].

% A key AND value less attribute will make this function crash, and it should.

% A valueless key will have an empty string as value
keyvalue([A|[]]) ->
    {A,""};

% All other attributes are converted from [long,list,of,key,words,value] to {"long/list/of/key/words",value}
keyvalue(A) ->
    RA=lists:reverse(A),
    lists:foldl(
        fun(X,{[],[]}) ->
            ?DEB2("init: ~p",X),
            {[],X};
           (X,{[],Z}) ->
            {X,Z};
           (X,{Y,Z}) ->
            ?DEBL("recurs: ~p,~p,~p",[X,Y,Z]),
            {X++"/"++Y,Z}
        end,
        {[],[]},
        RA).


%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
generate_ext_info_io(Path) ->
  ?DEB2("   generating ext info for ~p",Path),
  ExtInfo=generate_ext_info(Path),
  ?DEB2("   generating ext io for ~p",ExtInfo),
  ExtIo=ext_info_to_ext_io(ExtInfo),
  {ExtInfo,ExtIo}.

%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
ext_info_to_ext_io(InternalExtInfoTupleList) ->
  ?DEB1("   Creating ext_io"),
  ext_info_to_ext_io(InternalExtInfoTupleList,[]).

%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
ext_info_to_ext_io([],B) -> 
  B0=B++"\0",
  B0len=length(B0),
  ?DEB1("   Done creating ext_io"),
  ?DEBL("   Final string: \"~p\", size: ~p",[B0,B0len]),
  {B0len,B0};

%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
ext_info_to_ext_io([{Name,_}|InternalExtInfoTupleList],String) ->
  Name0=
    case string:str(Name,"system")==1 of
      false ->
        ?DEB2("    Adding zero to end of name ~p, and \"user.\" to the start",Name),
        "user."++Name++"\0";
      true -> 
        ?DEB2("    Adding zero to end of name ~p",Name),
        Name++"\0"
    end,
  ?DEB1("    Appending name to namelist"),
  NewString=String++Name0,
  ?DEB1("    Recursion"),
  ext_info_to_ext_io(InternalExtInfoTupleList,NewString).


