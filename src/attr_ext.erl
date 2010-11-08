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
         ext_info_to_ext_io/1]).

-include("../include/attrfs.hrl").
-include("../include/debug.hrl").


%%--------------------------------------------------------------------------
%% Later on, this function will not only insert the attribute in the database, but add the file to the corresponding attribute folders as well.
%%--------------------------------------------------------------------------
add_new_attribute(Path,FIno,FEntry,Attr) ->
  ?DEB1("  >add_new_attribute"),
  % Add the new attribute pair, if non-existent.
  ?DEBL("   inserting (~p)~p into database, if nonexistent",[Path,Attr]),
  length(dets:match(?ATTR_DB,{Path,Attr}))==0 andalso
    (ok=dets:insert(?ATTR_DB,{Path,Attr})),
  ?DEBL("   new database entry: ~p",[dets:match(?ATTR_DB,{Path,Attr})]),
  #inode_entry{stat=Stat,name=FName}=FEntry,
  ?DEBL("   appending ~p for {~p,~p} to the file system",[Attr,FName,FIno]),
  append_attribute(Attr,FName,?UEXEC(Stat)),
  ?DEB1("   creating new ext info"),
  rehash_ext_from_db(FIno,Path).



%%--------------------------------------------------------------------------
%% Updates 
%%--------------------------------------------------------------------------
rehash_ext_from_db(Inode,Path) ->
  {ExtInfo,ExtIo}=generate_ext_info_io(Path),
  {value,Entry}=tree_srv:lookup(Inode,inodes),
  NewEntry=Entry#inode_entry{ext_info=ExtInfo,ext_io=ExtIo},
  tree_srv:enter(Inode,NewEntry,inodes).

%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
append_attribute({Key,Val},Name,Stat) ->
  ?DEBL("    appending ~p/~p",[{Key,Val},Name]),
  append_value_dir(Key,Val,Name,Stat),
  ?DEBL("    appending ~p/~p",[Key,Val]),
  append_key_dir(Key,Val,Stat),
  tree_srv:enter(Key,inode:get(Key,ino),keys),
  AttributesFolderIno=inode:get(?ATTR_FOLDR,ino),
  ?DEB1("   getting attribute folder inode entry"),
  {value,AttrEntry}=tree_srv:lookup(AttributesFolderIno,inodes),
  ?DEB1("   getting attribute folder children"),
  AttrChildren=tree_srv:to_list(keys), 
  ?DEB1("   creating new inode entry"),
  NewAttrEntry=AttrEntry#inode_entry{children=AttrChildren},
  ?DEB2("   children of new attr entry: ~p",NewAttrEntry#inode_entry.children),
  tree_srv:enter(AttributesFolderIno,NewAttrEntry,inodes).

%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
append_key_dir(KeyDir,ValDir,Stat) ->
  ChildIno=inode:get({KeyDir,ValDir},ino),
  MyInode=inode:get(KeyDir,ino),
  NewEntry=
    case tree_srv:lookup(MyInode,inodes) of
      % No entry found, creating new attribute entry.
      none ->
        ?DEBL("   adding new attribute key folder ~p",[KeyDir]),
        #inode_entry{
          type=#attribute_dir{atype=key},
          name=KeyDir,
          children=[{ValDir,ChildIno}],
          %XXX: Give the user some way of setting a standard 
          stat=attr_tools:dir(Stat#stat{st_ino=MyInode}), 
          ext_info=[],
          ext_io=ext_info_to_ext_io([])
        };
      {value,OldEntry} ->
        ?DEBL("   merging ~p into ~p",[{ValDir,ChildIno},OldEntry#inode_entry.children]),
        OldEntry#inode_entry{
          children=
            attr_tools:keymergeunique({ValDir,ChildIno},OldEntry#inode_entry.children)
        }
    end,
  tree_srv:enter(MyInode,NewEntry,inodes).

%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
append_value_dir(Key,Value,ChildName,Stat) ->
  ChildIno=inode:get(ChildName,ino),
  MyInode=inode:get({Key,Value},ino),
  NewEntry=
    case tree_srv:lookup(MyInode,inodes) of
      % No entry found, creating new attribute entry.
      none ->
        ?DEBL("   adding new attribute value folder ~p",[{Key,Value}]),
        #inode_entry{
          type=#attribute_dir{atype=value},
          name={Key,Value},
          children=[{ChildName,ChildIno}],
          stat=attr_tools:dir(Stat#stat{st_ino=MyInode}),
          ext_info=[],
          ext_io=ext_info_to_ext_io([])
        };
      {value,OldEntry} ->
        ?DEBL("   merging ~p into ~p",[{ChildName,ChildIno},OldEntry#inode_entry.children]),
        OldEntry#inode_entry{
          children=
            attr_tools:keymergeunique({ChildName,ChildIno},OldEntry#inode_entry.children)
        }
    end,
  ?DEB1("  entering new entry into server"),
  tree_srv:enter(MyInode,NewEntry,inodes).


% this function assumes that the _child_ exists, and creates parents as neccessary. 

append_dir(ChildName=[Child|Parent],Stat) ->
  ChildIno=inode_get(ChildName,ino),
  ParentIno=inode_get(Parent,ino),
    case tree_srv:lookup(Parent,inodes) of
      % No entry found, creating new attribute entry.
      none ->
        ?DEBL("   adding new attribute value folder ~p",[ChildName]),
        #inode_entry{
          type=attribute_dir,
          name=Parent,
          children=[{ChildName,ChildIno}],
          stat=attr_tools:dir(Stat#stat{st_ino=ParentIno}),
          ext_info=[],
          ext_io=ext_info_to_ext_io([])
        };
      {value,OldEntry} ->
        ?DEBL("   merging ~p into ~p",[{ChildName,ChildIno},OldEntry#inode_entry.children]),
        OldEntry#inode_entry{
          children=
            attr_tools:keymergeunique({ChildName,ChildIno},OldEntry#inode_entry.children)
        }
    end,
  ?DEB1("  entering new entry into server"),
  tree_srv:enter(MyInode,NewEntry,inodes).
%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
generate_ext_info(Path) ->
  ?DEB2("   generating ext info for ~p",Path),
  ExtInfo0=dets:match(?ATTR_DB,{Path,'$1'}), 
  attr_tools:merge_duplicates(lists:flatten(ExtInfo0)). % Going from a list of lists of tuples to a list of tuples.



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


