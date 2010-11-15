-module(attr_rename).

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

-export([rename/4]).
%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
%% rename internal functions
%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------


%% make_rename_reply filters according to the old parent type, the old attribute name, the new attribute type 

rename(ParentIno,NewParentIno,OldName,NewName) ->
    {value,PEntry}=tree_srv:lookup(ParentIno,inodes),
    PType=PEntry#inode_entry.type,
    PName=PEntry#inode_entry.name,
    ?DEBL("   parent_type: ~p",[PType]),
    case tree_srv:lookup(NewParentIno,inodes) of
      none ->
        ?DEB1("   new parent nonexistent!"),
        enoent; 
      {value,NPEntry} ->
        NPType=NPEntry#inode_entry.type,
        NPName=NPEntry#inode_entry.name,
        ?DEBL("   new parent type: ~p",[NPType]),
        case inode:is_numbered(OldName,ino) of
          false -> 
            ?DEB1("   file nonexistent!"),
            enoent;
          FIno ->
            {value,FEntry}=tree_srv:lookup(FIno,inodes),
            FType=FEntry#inode_entry.type,
            rename_internal(
              PType,NPType,FType,PName,NPName,
              NewParentIno,NPEntry,FIno,FEntry,NewName)
        end
    end.

%% rename_internal(OldParentType,NewParentType,NewFileType,

rename_internal(#external_dir{},attribute_dir,#external_file{},PName,NPName,NewAttribIno,NewAttribEntry,FileIno,FileEntry,NewValueName) ->
  copy_file(NewAttribIno,FileIno,FileEntry);

rename_internal(attribute_dir,attribute_dir,attribute_dir,PName,NPName,NewAttribIno,NewAttribEntry,FileIno,ValueEntry,NewValueName) ->
  move_attribute_dir(NewAttribEntry,ValueEntry,NewValueName);

rename_internal(attribute_dir,attribute_dir,#external_file{},PName,NPName,NewAttribIno,NewAttribEntry,FileIno,FileEntry,NewValueName) ->
  copy_file(NewAttribIno,FileIno,FileEntry);

rename_internal(_,_,_,PName,NPName,NewAttribIno,NewAttribEntry,FileIno,FileEntry,NewValueName) ->
  enotsup.


%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------

%% Since the attribute folder already exists, things needn't get overly coplicated here...
copy_file(NPIno,FIno,FEntry) ->
  Path=(FEntry#inode_entry.type)#external_file.path,
  Attribute=inode:is_named(NPIno,ino),
  ?DEBL("   copying file ~p into ~p",[FEntry#inode_entry.name,Attribute]),
  attr_ext:add_new_attribute(Path,FIno,FEntry,Attribute).

%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
move_file(FileInode,FileEntry,OldParentEntry,NewParentEntry) ->
  FilePath=(FileEntry#inode_entry.type)#external_file.path,
  OldAttribName=OldParentEntry#inode_entry.name,
  attr_remove:remove_attribute(FilePath,FileInode,OldAttribName),
  NewAttribName=NewParentEntry#inode_entry.name,
  attr_ext:add_new_attribute(FilePath,FileInode,FileEntry,NewAttribName).


%%--------------------------------------------------------------------------
% This function moves OldValueEntry to NewParentEntry,Using NewValueName as its new name.
% This also means changing the attributes of all files in OldValueEntry and all subdirs.
% NewParentEntry: the entry where to move the attribute dir
% OldValueEntry: the entry of the value dir of the attribute to be moved
% NewValueName: the new value name.
%%--------------------------------------------------------------------------
move_attribute_dir(NewParentEntry,OldValueEntry,NewValueName) ->
  ?DEB1("   moving value dir"),
  OldAttribName=[OldValueName|OldKeyName]=OldValueEntry#inode_entry.name,
  NewParentName=NewParentEntry#inode_entry.name,
  NewAttribName=[NewValueName,NewParentName], 
  NewValueEntry=OldValueEntry#inode_entry{name=NewAttribName},
  ?DEBL("   moving ~p to ~p",[OldAttribName,NewAttribName]),
  % I need to move the children before removing the entries of the parents from the current dir.
  lists:foreach(
    fun({ChildName,ChildInode}) ->
      {value,ChildEntry}=tree_srv:lookup(ChildInode,inodes),
      ChildType=ChildEntry#inode_entry.type,
      move_child(ChildType,ChildInode,ChildEntry,OldValueEntry,NewValueEntry)

    end,
    OldValueEntry#inode_entry.children
  ),
  ?DEBL("    removing ~p from the ~p directory", [OldValueName,OldKeyName]),
  %XXX: Yes, this is a little bit ugly. Maybe do a global change about 
  %     whether to use inodes or entries as arguments sometime?
  KeyIno=inode:is_numbered(OldKeyName,ino),
  ValueIno=inode:is_numbered(OldValueName,ino),
  attr_remove:remove_empty_dir(KeyIno,OldValueName),
  ?DEBL("    adding ~p to ~p directory", [NewValueName,NewParentName]),
  tree_srv:enter(ValueIno,NewValueEntry,inodes),
  ?DEB2("    adding ~p to inode list",NewAttribName),
  append_child({NewValueName,ValueIno},KeyIno),
  ?DEB1("    moving inode number"),
  inode:rename(OldAttribName,NewAttribName,ino),
  ok.


%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
move_child(#external_file{},ChildInode,ChildEntry,OldParentEntry,NewParentEntry) ->
  move_file(ChildInode,ChildEntry,OldParentEntry,NewParentEntry);

move_child(attribute_dir,ChildInode,ChildEntry,_OldParentEntry,NewParentEntry) ->
  % Moving a value subdir does not change the name of the dir itself, hence the Entry#inode_entry.name as NewName.
  move_attribute_dir(NewParentEntry,ChildEntry,ChildEntry#inode_entry.name).



%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
append_child(NewChild={_ChildName,_ChildIno},ParentIno) ->
  {value,ParentEntry}=tree_srv:lookup(ParentIno,inodes),
  Children=ParentEntry#inode_entry.children,
  NewChildren=attr_tools:keymergeunique(NewChild,Children),
  NewParentEntry=ParentEntry#inode_entry{children=NewChildren},
  tree_srv:enter(ParentIno,NewParentEntry,inodes).


