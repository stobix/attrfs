-module(attr_remove).

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

-export([remove_attribute/3,
         remove_child_from_parent/2,
         remove_key_values/3,
         remove_empty_dir/2]).

remove_child_from_parent(ChildName,ParentName) ->
  {ok,Inode}=inode:n2i(ParentName,ino),
  {value,Entry}=tree_srv:lookup(Inode,inodes),
  Children=Entry#inode_entry.children,
  NewChildren=lists:keydelete(ChildName,1,Children),
  NewEntry=Entry#inode_entry{children=NewChildren},
  tree_srv:enter(Inode,NewEntry,inodes).
%%--------------------------------------------------------------------------
%% remove_old_attribute_value
%% Path: The external path for the file in the db.
%% Inode: The internal inode of the file.
%% Attribute: The [Value|Key] attribute to remove from the dir.
%%--------------------------------------------------------------------------
%% this function removes files from attribute folders. If a file is removed from an attribute folder, it does NOT affect the other subfolders of the attribute containing this folder.
%% Removing the file from attribs/foo/ does not remove the file from attribs/foo/bar
remove_attribute(Path,Inode,Attribute) ->
  % Database handling
  Matches=dets:match(?ATTR_DB,{Path,Attribute}),
  case length(Matches)>0 of
    true ->
      ?DEBL("Removing ~p for ~p from database", [Attribute,Path]),
      dets:match_delete(?ATTR_DB,{Path,Attribute});
    false ->
      ?DEB1("No data base entry to remove!")
  end,
  % File attribute handling
  attr_ext:rehash_ext_from_db(Inode,Path),
  % Attribute dir handling
  {ok,Name}=inode:i2n(Inode,ino),
  remove_child_from_parent(Name,Attribute).

%%--------------------------------------------------------------------------
%% remove_old_attribute_key
%% Path: The external path for the file in the db.
%% Inode: The internal inode of the file.
%% Key: The Key whose values are to be removed.
%%--------------------------------------------------------------------------
%%  * removes all entries from the attribute database one dir deeper than the  key
%%  * removes the file entry from all attributes/attr/Name/attrVal/ folders
%%  * removes the attribute from the file entry
%%  * does NOT remove the possibly empty attrName/attrVal folder from the attributes branch of the file system
%%--------------------------------------------------------------------------

% this one was used for removing attributes. Removing the attribute key removes the file from all direct subdirs where it resides
% attr -r foo fil when attribute foo has the value ",bar,baz" should remove fil from both attribs/foo attribs/foo/bar and attribs/foo/baz
remove_key_values(Path,Inode,AName) ->
  ?DEBL("    deleting ~p from ~p",[AName,Path]),
  % Database handling
  Matches=dets:match(?ATTR_DB,{Path,['$1'|AName]}),
  case length(Matches)>0 of
    true -> 
      ?DEBL("   removing the following items (if any): ~p",[Matches]),
      dets:match_delete(?ATTR_DB,{Path,['_'|AName]});
    false -> 
      ?DEB1("   found no items to remove, doing nothing"),
      ok
  end,
  ?DEBL("   items left (should be none): ~p",[dets:match(?ATTR_DB,{Path,['$1'|AName]})]),
  % Updating ext io using the filtered ext info
  attr_ext:rehash_ext_from_db(Inode,Path),
  % removing file child from attribute folder entry
  {ok,FName}=inode:i2n(Inode,ino),
  lists:foreach(
    fun(AValue) -> 
      remove_child_from_parent(FName,[AValue|AName])
    end,
    Matches
  ).

%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
remove_empty_dir(ParentIno,DirName) ->
  ?DEBL("   removing empty dir ~p from parent ~p",[DirName,ParentIno]),
  {value,ParentEntry}=tree_srv:lookup(ParentIno,inodes),
  case lists:keytake(DirName,1,ParentEntry#inode_entry.children) of
    {value,{_DeletedChild,ChildIno,_ChildType},NewChildren} ->
      NewParentEntry=ParentEntry#inode_entry{children=NewChildren},
      tree_srv:enter(ParentIno,NewParentEntry,inodes),
      tree_srv:delete_any(ChildIno,inodes),
      ok;
    _ -> 
      % Found no old entry; nothing needs to be done.
      ?DEB1("   dir not a child of parent! not removing!!"),
      enoent
  end.
