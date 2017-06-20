
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

-module(attr_rename).
-compile({parse_transform, cut}).

-include("../include/attrfs.hrl").
-include_lib("newdebug/include/debug.hrl").

-export([rename/4]).
%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
%% rename internal functions
%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------


%% make_rename_reply filters according to the old parent type, the old attribute name, the new attribute type 

rename(ParentIno,NewParentIno,OldName,NewName) ->
    ?DEB1(3,">rename"),
    {value,PEntry}=tree_srv:lookup(ParentIno,inodes),
    PType=PEntry#inode_entry.type,
    ?DEBL(5,"parent_type: ~p",[PType]),
    case tree_srv:lookup(NewParentIno,inodes) of
      none ->
        ?DEB1(5,"new parent nonexistent!"),
        enoent; 
      {value,NPEntry} ->
        NPType=NPEntry#inode_entry.type,
        ?DEBL(5,"new parent type: ~p",[NPType]),
        case lists:keyfind(OldName,1,PEntry#inode_entry.contents) of
          false -> 
            ?DEB1(5,"file nonexistent!"),
            enoent;
          {_,FIno,_} ->
            {value,FEntry}=tree_srv:lookup(FIno,inodes),
            FType=FEntry#inode_entry.type,
            rename_internal(
              PType,NPType,FType,
              ParentIno,NewParentIno,NPEntry,FIno,FEntry,NewName)
        end
    end.

%% rename_internal(OldParentType,NewParentType,NewFileType,

rename_internal(#external_dir{},attribute_dir,#external_file{},_,NewAttribIno,_,FileIno,FileEntry,_) ->
  copy_file(NewAttribIno,FileIno,FileEntry);

rename_internal(attribute_dir,attribute_dir,attribute_dir,_,_,NewAttribEntry,_,ValueEntry,NewValueName) ->
  move_attribute_dir(NewAttribEntry,ValueEntry,NewValueName);

rename_internal(attribute_dir,attribute_dir,#external_file{},_,NewAttribIno,_,FileIno,FileEntry,_) ->
  copy_file(NewAttribIno,FileIno,FileEntry);

rename_internal(attribute_dir,attribute_dir,internal_file,ParentIno,NewParentIno,NewParentEntry,FileIno,FileEntry,NewName) ->
  move_internal_file(FileIno,FileEntry,ParentIno,NewName,NewParentIno,NewParentEntry);

rename_internal(_,_,_,_,_,_,_,_,_) ->
  enotsup.


%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------

% Prefixes: File, Parent, NewParent
move_internal_file(FIno,FEntry,PIno,NewName,PIno,NPEntry) ->
  Name=FEntry#inode_entry.name,
  {value,{Name,FIno,Type},Children}=lists:keytake(Name,1,NPEntry#inode_entry.contents),
  NewNPEntry=NPEntry#inode_entry{contents=[{NewName,FIno,Type}|Children]},
  NewFEntry=FEntry#inode_entry{name=NewName},
  tree_srv:enter(FIno,NewFEntry,inodes),
  tree_srv:enter(PIno,NewNPEntry,inodes);

% Prefixes: File, Parent, NewParent
move_internal_file(FIno,FEntry,PIno,NewName,NPIno,NPEntry) ->
  Name=FEntry#inode_entry.name,
  {value,PEntry}=tree_srv:lookup(PIno,inodes),
  PName=PEntry#inode_entry.name,
  Children=NPEntry#inode_entry.contents,
  NewNPEntry=NPEntry#inode_entry{contents=[{NewName,FIno,FEntry#inode_entry.type}|Children]},
  NewFEntry=FEntry#inode_entry{name=NewName},
  tree_srv:enter(FIno,NewFEntry,inodes),
  tree_srv:enter(NPIno,NewNPEntry,inodes),
  attr_remove:remove_child_from_parent(Name,PName).

%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
%% Since the attribute folder already exists, things needn't get overly coplicated here...
copy_file(NPIno,FIno,FEntry) ->
  Path=(FEntry#inode_entry.type)#external_file.path,
  {ok,Attribute}=numberer:i2n(NPIno,ino),
  ?DEBL(6,"copying file ~p into ~p",[FEntry#inode_entry.name,Attribute]),
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
  ?DEB1(6,"moving value dir"),
  OldAttribName=[OldValueName|OldKeyName]=OldValueEntry#inode_entry.name,
  NewParentName=NewParentEntry#inode_entry.name,
  NewAttribName=[NewValueName|NewParentName], 
  NewValueEntry=OldValueEntry#inode_entry{name=NewAttribName},
  ?DEBL(8,"moving ~p to ~p",[OldAttribName,NewAttribName]),
  % I need to move the children before removing the entries of the parents from the current dir.
  lists:foreach(
    fun({_ChildName,ChildInode,ChildType}) ->
      {value,ChildEntry}=tree_srv:lookup(ChildInode,inodes),
      move_child(ChildType,ChildInode,ChildEntry,OldValueEntry,NewValueEntry)

    end,
    OldValueEntry#inode_entry.contents
  ),
  %XXX: Yes, this is a little bit ugly. Maybe do a global change about 
  %     whether to use inodes or entries as arguments sometime?
  ?DEBL(8,"removing ~p from the ~p directory", [OldValueName,OldKeyName]),
  ?DEBL(9,"getting inodes...",[]),
  {ok,KeyIno}=numberer:n2i(OldKeyName,ino),
  {ok,ValueIno}=numberer:n2i(OldAttribName,ino),
  ?DEBL(9,"removing...",[]),
  attr_remove:remove_empty_dir(KeyIno,OldValueName),
  ?DEBL(9,"adding ~p to ~p directory", [NewValueName,NewParentName]),
  tree_srv:enter(ValueIno,NewValueEntry,inodes),
  ?DEB2(9,"adding ~p to inode list",NewAttribName),
  attr_tools:append_child({NewValueName,ValueIno,attribute_dir},KeyIno),
  ?DEB1(9,"moving inode number"),
  numberer:rename(OldAttribName,NewAttribName,ino),
  ok.


%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
move_child(#external_file{},ChildInode,ChildEntry,OldParentEntry,NewParentEntry) ->
  move_file(ChildInode,ChildEntry,OldParentEntry,NewParentEntry);

move_child(attribute_dir,_ChildInode,ChildEntry,_OldParentEntry,NewParentEntry) ->
  % Moving a value subdir does not change the name of the dir itself, hence the Entry#inode_entry.name as NewName.
  move_attribute_dir(NewParentEntry,ChildEntry,ChildEntry#inode_entry.name).




