-module(attr_rename).

-include("../include/attrfs.hrl").
-include("../include/debug.hrl").

-export([make_rename_reply/6]).
%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
%% rename internal functions
%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------

make_rename_reply(#external_dir{},_OldAttribName,_OldAttribEntry,NewAttribIno,File,_NewValueName) ->
  ?DEB1("   old parent is an external dir"),
  {value,NewAttribEntry}=tree_srv:lookup(NewAttribIno,inodes),
  ?DEB1("   got new attribute folder entry"),
  case NewAttribEntry#inode_entry.type of
    #attribute_dir{atype=value} ->
      ?DEB1("   new parent is a value dir"),
      FileIno=inode:is_numbered(File,ino),
      {value,FileEntry}=tree_srv:lookup(FileIno,inodes),
      case FileEntry#inode_entry.type of
        %external dir, attribute dir, external file
        #external_file{} ->
          ?DEB1("   external dir, attribute dir, external file"),
          ?DEB1("   all ok, copying."),
          make_rename_file_reply(NewAttribIno,FileIno,FileEntry);
        _ -> 
          ?DEB1("   external dir, attribute dir, BOGUS"),
          enotsup
      end;
    _ -> 
      ?DEB1("   external dir, BOGUS, _"),
      enotsup
  end;


make_rename_reply(#attribute_dir{},_OldAttribName,OldAttribEntry,NewAttribIno,OldValueName,NewValueName) ->
  ?DEB1("   old parent is an attribute dir"),
  {value,NewAttribEntry}=tree_srv:lookup(NewAttribIno,inodes),
  case NewAttribEntry#inode_entry.type of
    #attribute_dir{atype=value} ->
      ?DEB1("   new parent is a value dir"),
      {OldValueName,FileIno}=lists:keyfind(OldValueName,1,OldAttribEntry#inode_entry.children),
      {value,FileEntry}=tree_srv:lookup(FileIno,inodes),
      case FileEntry#inode_entry.type of
        #external_file{} ->
          ?DEB1("   attribute dir, value dir, external file"),
          ?DEB1("   copying"),
          make_rename_file_reply(NewAttribIno,FileIno,FileEntry);
        #attribute_dir{} ->
          ?DEB1("   attribute dir, value dir, attribute dir"),
          ?DEB1("   moving"),
          make_rename_value_to_value_dir_reply(NewAttribEntry,FileIno,FileEntry,NewValueName);
        _ -> 
          ?DEB1("   attribute dir, value dir, BOGUS"),
          enotsup
      end;
    #attribute_dir{atype=key} ->
      ?DEB1("   new parent is a key dir, preparing to copy."),
      {OldValueName,FileIno}=lists:keyfind(OldValueName,1,OldAttribEntry#inode_entry.children),
      {value,FileEntry}=tree_srv:lookup(FileIno,inodes),
      case FileEntry#inode_entry.type of
        #attribute_dir{atype=value} ->
          ?DEB1("   attribute dir, key dir, value dir"),
          ?DEB1("   moving"),
          make_rename_value_to_value_dir_reply(NewAttribEntry,FileIno,FileEntry,NewValueName);
        _ -> 
          ?DEB1("   attribute dir, key dir, BOGUS"),
          ?DEB2("   BOGUS: ~p",FileEntry#inode_entry.type),
          enotsup
      end;
    internal_dir ->
      ?DEB1("   moving from attribute value folder to attribute key folder not yet supported"),
      enotsup;
        % This will be supported either when I check for subdirs, or when I allow files attributes with empty values.
%            case NewAttribEntry#inode_entry.name of
%                ?ATTR_FOLDR ->
%                    FileIno=inode:is_numbered(File),
%                    {value,FileEntry}=tree_srv:lookup(FileIno,inodes),
%                    case FileEntry#inode_entry.type of
%                        #attribute_dir{} ->
%                            ?DEB1("   attribute dir, \"attribs\", attribute dir"),
%                            ?DEB1("   moving"),
%                            make_rename_key_dir_reply(OldAttribEntry,FileIno,FileEntry,NewValueName);
%                        _ -> 
%                            ?DEB1("   attribute dir, \"attribs\", BOGUS"),
%                            enotsup
%                    end;
%
%                _ -> 
%                    ?DEB1("   attribute dir, BOGUS internal dir, _"),
%                    enotsup
%            end;
    _ ->
      ?DEB1("   attribute dir, BOGUS, _"),
      ?DEB2("   BOGUS: ~p",NewAttribEntry),
      enotsup
  end;

make_rename_reply(internal_dir,?ATTR_FOLDR,_OldAttribEntry,NewAttribIno,File,NewValueName) ->
  ?DEB1("   old parent is an internal dir"),
  {value,NewAttribEntry}=tree_srv:lookup(NewAttribIno,inodes),
  case NewAttribEntry#inode_entry.type of
        %% These will be supported when I make deep attributes.
%        #attribute_dir{type=value}} ->
%        #attribute_dir{type=key} ->
    internal_dir ->
      case NewAttribEntry#inode_entry.name of
        ?ATTR_FOLDR ->
          FileIno=inode:is_numbered(File,ino),
          {value,FileEntry}=tree_srv:lookup(FileIno,inodes),
          case FileEntry#inode_entry.type of
            #attribute_dir{} ->
              ?DEB1("   \"attribs\", \"attribs\", attribute dir"),
              ?DEB1("   moving"),
              make_rename_key_to_key_dir_reply(FileIno,FileEntry,NewValueName);
            _ -> 
              ?DEB1("   \"attribs\", \"attribs\", BOGUS"),
              enotsup
          end;
        _ -> 
          ?DEB1("   attribute dir, BOGUS internal dir, _"),
          enotsup
      end;
    _ ->
      ?DEB1("   attribute dir, BOGUS, _"),
      enotsup
  end;

make_rename_reply(_DirType,_OldAttribName,_OldAttribEntry,_NewAttribIno,_File,_NewValueName) ->
  ?DEB2("   ~p, _, _",_DirType),
  ?DEB1("   not allowed"),
  enotsup.

%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------

%% Since the attribute folder already exists, things needn't get overly coplicated here...
make_rename_file_reply(NewAttribIno,FileInode,FileEntry) ->
  ?DEB1("   copying file"),
  Path=(FileEntry#inode_entry.type)#external_file.path,
  Attribute=inode:is_named(NewAttribIno,ino),
  case Attribute of
    {_Key,_Value} ->
      attr_ext:add_new_attribute(Path,FileInode,FileEntry,Attribute),
      ok;
    _Key ->
      % Valueless attributes are not supported for now. 
      % Takes some restructuring to get to work, I think.
      enotsup 
  end.

make_rename_value_to_value_dir_reply(NewKeyEntry,ValueIno,OldValueEntry,NewValueName) ->
  ?DEB1("   moving value dir"),
  %TODO: if there is more than one value subdirectory under the current key for a file, fix so that this will NOT remove _all_ directories (or add them again)
  OldAttribName={OldKeyName,OldValueName}=OldValueEntry#inode_entry.name,
  NewKeyName=NewKeyEntry#inode_entry.name,
  NewAttribName={NewKeyName,NewValueName}, % To support multiple subattributes, this needs to be changed.
  NewValueEntry=OldValueEntry#inode_entry{name=NewAttribName},
  ?DEBL("   moving ~p to ~p",[OldAttribName,NewAttribName]),
  lists:foreach(
    fun({_FileName,FileInode}) ->
      {value,FileEntry}=tree_srv:lookup(FileInode,inodes),
      FilePath=(FileEntry#inode_entry.type)#external_file.path,
      attr_remove:remove_old_attribute_key(FilePath,FileInode,OldKeyName),
      attr_ext:add_new_attribute(FilePath,FileInode,FileEntry,NewAttribName)
    end,
    OldValueEntry#inode_entry.children
  ),
  ?DEBL("    removing ~p from the ~p directory", [OldValueName,OldKeyName]),
  KeyIno=inode:is_numbered(OldKeyName,ino),
  attr_remove:remove_empty_dir(KeyIno,OldValueName),
  ?DEBL("    adding ~p to ~p directory", [NewValueName,NewKeyName]),
  tree_srv:enter(ValueIno,NewValueEntry,inodes),
  ?DEB2("    adding ~p to inode list",NewAttribName),
  append_child({NewValueName,ValueIno},KeyIno),
  ?DEB1("    moving inode number"),
  inode:rename(OldAttribName,NewAttribName,ino),
  ok.

%%--------------------------------------------------------------------------
%% So, this will take an attribute directory, move it into a "key position" (as a direct child to ?ATTR_FOLDR), and consequently change the attribute names of all subfolders and subfolder file attributes, unless this is done with subsequent calls to rename by the OS.
%%--------------------------------------------------------------------------
make_rename_key_to_key_dir_reply(KeyIno,OldKeyEntry,NewKeyName) ->
  OldKeyName=OldKeyEntry#inode_entry.name,
  ?DEBL("   moving key dir ~p to ~p",[OldKeyName,NewKeyName]),
  NewKeyEntry=OldKeyEntry#inode_entry{name=NewKeyName},
  Children=OldKeyEntry#inode_entry.children,
  ?DEBL("    renaming child attribute key ~p to ~p for ~p",[OldKeyName,NewKeyName,Children]),
  lists:foreach(
    fun({AttribName,AttribInode}) ->
      {value,AttribEntry}=tree_srv:lookup(AttribInode,inodes),
      make_rename_value_to_value_dir_reply(NewKeyEntry,AttribInode,AttribEntry,AttribName)
    end,
    Children
  ),
  ?DEB2("    removing ~p from attribute directory", OldKeyName),
  AttrsIno=inode:is_numbered(?ATTR_FOLDR,ino),
  attr_remove:remove_empty_dir(AttrsIno,OldKeyName),
  ?DEB2("    adding ~p to inode tree and attribute directory", NewKeyName),
  tree_srv:enter(KeyIno,NewKeyEntry,inodes),
  append_child({NewKeyName,KeyIno},AttrsIno),
  ?DEB1("    moving inode number"),
  inode:rename(OldKeyName,NewKeyName,ino),
  ok.

%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
append_child(NewChild={_ChildName,_ChildIno},ParentIno) ->
  {value,ParentEntry}=tree_srv:lookup(ParentIno,inodes),
  Children=ParentEntry#inode_entry.children,
  NewChildren=attr_tools:keymergeunique(NewChild,Children),
  NewParentEntry=ParentEntry#inode_entry{children=NewChildren},
  tree_srv:enter(ParentIno,NewParentEntry,inodes).
