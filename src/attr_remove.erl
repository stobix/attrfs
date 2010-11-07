-module(attr_remove).

-include("../include/attrfs.hrl").
-include("../include/debug.hrl").

-export([remove_old_attribute_value/3,
         remove_child_from_parent/2,
         remove_old_attribute_key/3,
         remove_empty_dir/2]).
%%--------------------------------------------------------------------------
%% remove_old_attribute_value
%% Path: The external path for the file in the db.
%% Inode: The internal inode of the file.
%% Attribute: The {Key,Value} pair to be removed.
%%--------------------------------------------------------------------------
remove_old_attribute_value(Path,Inode,{_Key,_Value}=Attribute) ->
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
  remove_child_from_parent(inode:is_named(Inode,ino),Attribute).

remove_child_from_parent(ChildName,ParentName) ->
  Inode=inode:get(ParentName,ino),
  {value,Entry}=tree_srv:lookup(Inode,inodes),
  Children=Entry#inode_entry.children,
  NewChildren=lists:keydelete(ChildName,1,Children),
  NewEntry=Entry#inode_entry{children=NewChildren},
  tree_srv:enter(Inode,NewEntry,inodes).

%%--------------------------------------------------------------------------
%% remove_old_attribute_key
%%  * removes the {path,attribute} entry from the attribute database;
%%  * removes the file entry from the attributes/attrName/attrVal/ folder
%%  * removes the attribute from the file entry
%%  * does NOT remove the possibly empty attrName/attrVal folder from the attributes branch of the file system
%%--------------------------------------------------------------------------

remove_old_attribute_key(Path,Inode,AName) ->
  ?DEBL("    deleting ~p from ~p",[AName,Path]),
  % Database handling
  Matches=dets:match(?ATTR_DB,{Path,{AName,'$1'}}),
  case length(Matches)>0 of
    true -> 
      ?DEBL("   removing the following items (if any): ~p",[Matches]),
      dets:match_delete(?ATTR_DB,{Path,{AName,'_'}});
    false -> 
      ?DEB1("   found no items to remove, doing nothing"),
      ok
  end,
  ?DEBL("   items left (should be none): ~p",[dets:match(?ATTR_DB,{Path,{AName,'$1'}})]),
  % Updating ext io using the filtered ext info
  attr_ext:rehash_ext_from_db(Inode,Path),
  % removing file child from attribute folder entry
  FName=inode:is_named(Inode,ino),
  lists:foreach(
    fun(AValue) -> 
      remove_child_from_parent(FName,{AName,AValue})
    end,
    Matches
  ).

%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
remove_empty_dir(ParentIno,DirName) ->
  ?DEBL("   removing empty dir ~p from parent ~p",[DirName,ParentIno]),
  {value,ParentEntry}=tree_srv:lookup(ParentIno,inodes),
  case lists:keytake(DirName,1,ParentEntry#inode_entry.children) of
    {value,{_DeletedChild,ChildIno},NewChildren} ->
      NewParentEntry=ParentEntry#inode_entry{children=NewChildren},
      tree_srv:enter(ParentIno,NewParentEntry,inodes),
      tree_srv:delete_any(ChildIno,inodes),
      ok;
    _ -> 
      % Found no old entry; nothing needs to be done.
      ?DEB1("   dir not a child of parent! not removing!!"),
      enoent
  end.
