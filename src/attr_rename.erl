
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
-include_lib("newdebug/include/newdebug19.hrl").

-export([rename/4]).

rename(ParentIno,NewParentIno,OldName,NewName) ->
    ?DEB1({rename,3},">rename"),
    {value,ParentEntry}=tree_srv:lookup(ParentIno,inodes),
    case tree_srv:lookup(NewParentIno,inodes) of
      none ->
        ?DEB1({rename,5},"new parent nonexistent!"),
        enoent; 
      {value,NewParentEntry} ->
        case lists:keyfind(OldName,1,ParentEntry#inode_entry.contents) of
          false -> 
            ?DEB1({rename,5},"file nonexistent!"),
            enoent;
          {_,FileIno,_} ->
            {value,FileEntry}=tree_srv:lookup(FileIno,inodes),
            case {ParentEntry#inode_entry.type,NewParentEntry#inode_entry.type,FileEntry#inode_entry.type} of

              % Give the file a new attribute when moving an external file to an attribute dir from an external dir
              % or from an attribute dir
              {X,attribute_dir,#external_file{}} 
              when is_record(X,external_dir) 
              ; X==attribute_dir ->
                % Since the attribute folder already exists, things needn't get overly coplicated here...
                Path=(FileEntry#inode_entry.type)#external_file.path,
                {ok,Attribute}=numberer:i2n(NewParentIno,ino),
                ?DEBL({rename,6},"copying file ~p into ~p",[FileEntry#inode_entry.name,Attribute]),
                attr_ext:add_new_attribute(Path,FileIno,FileEntry,Attribute);

              % Move an attribute dir between attribute dirs, updating all files that had the attribute
              {attribute_dir,attribute_dir,attribute_dir} ->
                move_attribute_dir(NewParentEntry,FileEntry,NewName);

              % Move internal files between attribute dirs
              {attribute_dir,attribute_dir,internal_file} ->
                move_internal_file(FileIno,FileEntry,ParentIno,NewName,NewParentIno,NewParentEntry);

              % Other renaming operations not supported.
              {_,_,_} -> enotsup
            end
        end
    end.

%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------

% Prefixes: File, Parent, NewParent
move_internal_file(FIno,FEntry,PIno,NewName,PIno,NPEntry) ->
  ?DEB1({rename,3},">move internal 1"),
  Name=FEntry#inode_entry.name,
  {value,{Name,FIno,Type},Children}=lists:keytake(Name,1,NPEntry#inode_entry.contents),
  NewNPEntry=NPEntry#inode_entry{contents=[{NewName,FIno,Type}|Children]},
  NewFEntry=FEntry#inode_entry{name=NewName},
  tree_srv:enter(FIno,NewFEntry,inodes),
  tree_srv:enter(PIno,NewNPEntry,inodes);

% XXX This function clause can never match! What on earth were I thinking here‽

% Prefixes: File, Parent, NewParent
move_internal_file(FIno,FEntry,PIno,NewName,NPIno,NPEntry) ->
  ?DEB1({rename,3},">move internal 2"),
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
% This function moves OldValueEntry to NewParentEntry,Using NewValueName as its new name.
% This also means changing the attributes of all files in OldValueEntry and all subdirs.
% NewParentEntry: the entry where to move the attribute dir
% OldValueEntry: the entry of the value dir of the attribute to be moved
% NewValueName: the new value name.
%%--------------------------------------------------------------------------
%%
move_attribute_dir(NewParentEntry,OldValueEntry,NewValueName) ->
  ?DEB1({rename,6},"moving value dir"),
  OldAttribName=[OldValueName|OldKeyName]=OldValueEntry#inode_entry.name,
  NewParentName=NewParentEntry#inode_entry.name,
  NewAttribName=[NewValueName|NewParentName], 
  NewValueEntry=OldValueEntry#inode_entry{name=NewAttribName},
  ?DEBL({rename,6},"moving ~p to ~p",[OldAttribName,NewAttribName]),
  % I need to move the children before removing the entries of the parents from the current dir.
  lists:foreach(
    fun
      % Files have their corresponding attribute switched
      ({_ChildName,ChildInode,ChildInfo=#external_file{}}) ->
        {value,ChildEntry}=tree_srv:lookup(ChildInode,inodes),
        ChildPath=ChildInfo#external_file.path,
        OldAttribName=OldValueEntry#inode_entry.name,
        NewAttribName=NewValueEntry#inode_entry.name,
        attr_remove:remove_attribute(ChildPath,ChildInode,OldAttribName),
        attr_ext:add_new_attribute(ChildPath,ChildInode,ChildEntry,NewAttribName);

      % Attribute dir children gets moved to the new parent
      ({ChildName,ChildInode,attribute_dir}) ->
        {value,ChildEntry}=tree_srv:lookup(ChildInode,inodes),
        move_attribute_dir(NewValueEntry,ChildEntry,ChildName)
    end,
    OldValueEntry#inode_entry.contents
  ),
  %XXX: Yes, this is a little bit ugly. Maybe do a global change about 
  %     whether to use inodes or entries as arguments sometime?
  ?DEBL({rename,6},"removing ~p from the ~p directory", [OldValueName,OldKeyName]),
  ?DEBL({rename,6},"getting inodes...",[]),
  {ok,KeyIno}=numberer:n2i(OldKeyName,ino),
  {ok,ValueIno}=numberer:n2i(OldAttribName,ino),
  ?DEBL({rename,6},"removing...",[]),
  attr_remove:remove_empty_dir(KeyIno,OldValueName),
  ?DEBL({rename,6},"adding ~p to ~p directory", [NewValueName,NewParentName]),
  tree_srv:enter(ValueIno,NewValueEntry,inodes),
  ?DEB2({rename,6},"adding ~p to inode list",NewAttribName),
  attr_tools:append_child({NewValueName,ValueIno,attribute_dir},KeyIno),
  ?DEB1({rename,6},"moving inode number"),
  numberer:rename(OldAttribName,NewAttribName,ino),
  ok.

