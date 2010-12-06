-module(attrfs_srv).

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
%%%-------------------------------------------------------------------------
%%% @doc This is the attrfs fuserl server.
%%% Attrfs is a file system that is used to sort files into several virtual
%%% directories according to what xattr attributes one assigns to the files.
%%% 
%%% start_link({Dir,Fluff}) recursively takes the files in the directory 
%%% Fluff and sorts them into subfolders in the directory Dir/attribs. 
%%% One can add or modify attributes by means of changing xattribs on the 
%%% files, or using move operations (once I implement it, that is.)
%%% Since most of what this module does is implement the functions 
%%% documented in fuserl, not many functions in this module are documented.
%%% @end
%%%
%%% Most functions in this module have two headers; the first is the header
%%% taken from the fuserl API callback function with the corresponding name,
%%% the second any comments I make about the function.
%%%=========================================================================
%%%=========================================================================


%%%=========================================================================
%%%                                 EXPORTS
%%%=========================================================================

%%%=========================================================================
%%% server function exports
%%%=========================================================================
-export([handle_info/2,init/1,terminate/2]).
% TODO: Do something intelligent with this one. 
% Returns "ok" now, totally ignoring its indata.
-export([code_change/3]). 
-export([start_link/1,start_link/5]).

%%%=========================================================================
%%% fuserl function exports
%%%=========================================================================
-export([access/5,
         create/7,
         flush/5,
         forget/5,
         fsync/6,
         fsyncdir/6,
         getattr/4,
         getlk/6,
         getxattr/6,
         link/6,
         listxattr/5,
         lookup/5,
         mkdir/6,
         mknod/7,
         open/5,
         opendir/5,
         read/7,
         readdir/7,
         readlink/4,
         release/5,
         releasedir/5,
         removexattr/5,
         rename/7,
         rmdir/5,
         setattr/7,
         statfs/4,
         setlk/7,
         setxattr/7,
         symlink/6,
         unlink/5,
         write/7
         ]).
         


%%%=========================================================================
%%% debug function exports
%%%=========================================================================

-export([dump_entries/1,
         dump_inodes/0,
         dump_finodes/0,
         dump_inode_entries/0]).

%%%=========================================================================
%%% temporary exports for testing functionality.
%%%=========================================================================


%%%=========================================================================
%%% Includes and behaviour
%%%=========================================================================

-behaviour(fuserl).

-include_lib("fuserl/include/fuserl.hrl").
-include_lib("kernel/include/file.hrl").

-include("../include/attrfs.hrl").
-include("../include/debug.hrl").



%%%=========================================================================
%%%                            API FUNCTIONS
%%%=========================================================================

%%%=========================================================================
%%% server functions
%%%=========================================================================

%%--------------------------------------------------------------------------
%% The analog to Module:handle_info/2 in gen_server. Note port messages are intercepted by fuserlsrv and not forwarded to the module.
%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
handle_info(_Msg,State) ->
  ?DEBL(">handle_info(~p)",[_Msg]),
  {noreply,State}.

%%--------------------------------------------------------------------------
%% The analog to Module:code_change/3 in gen_server.
%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
code_change(_,_,_) -> 
  %XXX: Maybe do something more intelligent with this?
  ?DEB1(">code_change: Doing nothing!"),
  ok.

%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
%% Mirrors MirrorDir into MountDir/real, building the attribute file system in attribs from the attributes for the files in MountDir
%%--------------------------------------------------------------------------
start_link({MountDir,MirrorDir,DB}) ->
  ?DEB1(">Starting attrfs server..."),
  start_link(MountDir,false,"",MirrorDir,DB).

%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
%% In here, I mostly check for dirs needed to start fuserlsrv.
%--------------------------------------------------------------------------
start_link(Dir,LinkedIn,MountOpts,MirrorDirs,DB) ->

  ?DEB1(">start_link"),
  ?DEB1("   checkning if dirs are ok..."),
  ?DEB2("    ~p...",Dir),
  case filelib:ensure_dir(Dir) of
    ok -> 
      ?DEB1("     path ok."),
      case filelib:is_dir(Dir) of
        true ->
          ?DEB2("       ~p exists, and is a dir. Ok.",Dir);
        false->
          ?DEB2("       ~p is not a directory, or already mounted! (Check your config, mount and fusermount)",Dir),
          ?DEB1("TERMINATING"),
          exit({error,dir_is_not_a_dir})
      end;
    E ->
      ?DEB2("     received ~p (Check your config)",E),
      ?DEB1("TERMINATING"),
      exit(E)
  end,
  ?DEB1("    checking mirror dirs..."),
  lists:foreach(
    fun(MirrorDir) ->
      case filelib:is_dir(MirrorDir) of
        true ->
          ?DEB2("      ~p exists, and is a dir. Ok.",MirrorDir);
        false->
          ?DEB2("      ~p is not a directory!(Check your config)",MirrorDir),
          ?DEB1("TERMINATING"),
          exit({error,mirror_dir_is_not_a_dir})
      end
  end,
  MirrorDirs),
  Options=[],
  ?DEB1("   starting fuserlsrv"),
  fuserlsrv:start_link(?MODULE,LinkedIn,MountOpts,Dir,{MirrorDirs,DB},Options).

%%--------------------------------------------------------------------------
%% The analog to Module:init/1 in gen_server.
%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
init({MirrorDir,DB}) ->
  attr_init:init({MirrorDir,DB}),
  State=[],
  {ok,State}.



%%--------------------------------------------------------------------------
%% The analog to Module:terminate/2 in gen_server.
%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
terminate(_Reason,_State) ->
  ?DEB1(">terminate"),
  ?DEB1("\n\nXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n\n"),
  ?DEB2("|  _Reason: ~p",_Reason),
  ?DEB2("   Closing database \"~p\"",?ATTR_DB),
  dets:close(?ATTR_DB).


%%%=========================================================================
%%% fuserl functions
%%% The rudimentary file headers in here are copied from the fuserl 
%%% documentation.
%%%=========================================================================

%%--------------------------------------------------------------------------
%%Check file access permissions. Mask is a bitmask consisting of ?F_OK, ?X_OK, ?W_OK, and ?R_OK, which are portably defined in fuserl.hrl . #fuse_reply_err{err = ok} indicates success. If noreply is used, eventually fuserlsrv:reply/2  should be called with Cont as first argument and the second argument of type access_async_reply ().
%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
access(Ctx,Inode,Mask,_Continuation,State) ->
  ?DEB1(">access"),
  ?DEB2("|  Ctx: ~p ",Ctx),
  ?DEB2("|  Inode: ~p ",Inode),
  ?DEB2("|  Mask: ~p ",Mask),
  Reply=attr_tools:test_access(Inode,Mask,Ctx),
  {#fuse_reply_err{err=Reply},State}.

%%--------------------------------------------------------------------------
%% Create and open a file. Mode is a bitmask consisting of ?S_XXXXX macros portably defined in fuserl.hrl . If noreply is used, eventually fuserlsrv:reply/2  should be called with Cont as first argument and the second argument of type create_async_reply (). 
%%--------------------------------------------------------------------------
%% Where will I allow this? Creating new real files in the real branch? Creating imaginary files elsewhere?
%% The possibility of making a .Trash in the root folder should be considered.
%%
%% A file is allowed to be created in an attribs folder iff it already exists by that name somewhere else. This makes "copying" files possible.
%%--------------------------------------------------------------------------
create(Ctx,ParentInode,Name,_Mode,Fuse_File_Info,_Continuation, State) ->
  ?DEB1(">create"), 
  ?DEB2("|  Ctx: ~p",Ctx),
  ?DEB2("|  ParentIno: ~p",ParentInode),
  ?DEB2("|  Name: ~p",Name),
  ?DEB2("|  FI: ~p",Fuse_File_Info),
  Reply=case inode:is_numbered(binary_to_list(Name),ino) of
    false -> 
      ?DEB1("No real file with that name, not supported."),
      #fuse_reply_err{err=enotsup};
    Inode -> 
      {value,Entry}=tree_srv:lookup(Inode,inodes),
      case Entry#inode_entry.type of
        #external_file{path=Path} ->
          ?DEB1("Filename linked to a real file."),
          {value,ParentEntry}=tree_srv:lookup(ParentInode,inodes),
          case ParentEntry#inode_entry.type of
            attribute_dir ->
              ?DEB1("Parent is an attribute dir. All is ok."),
              % Adding a new attribute is the same as creating an attribute folder file entry.
              attr_ext:add_new_attribute(Path,Inode,Entry,ParentEntry#inode_entry.name),
              {#fuse_reply_open{fuse_file_info=FileInfo},_}=open(Ctx,Inode,Fuse_File_Info,_Continuation,[create,State]), 
              #fuse_reply_create{
                fuse_file_info=FileInfo,
                fuse_entry_param=?ENTRY2PARAM(Entry,Inode)
              };
            _ ->
              #fuse_reply_err{err=enotsup}
            end;
        _ ->
          ?DEB1("The file with that name has the wrong type!"),
          #fuse_reply_err{err=enotsup}
      end
  end,
  {Reply,State}.


%%--------------------------------------------------------------------------
%% This is called on each close () of an opened file, possibly multiple times per open  call (due to dup () et. al.). Fi#fuse_file_info.fh will contain the descriptor set in open, if any. #fuse_reply_err{err = ok} indicates success. This return value is ultimately the return value of close () (unlike release). Does *not* necessarily imply an fsync. If noreply is used, eventually fuserlsrv:reply/2  should be called with Cont as first argument and the second argument of type flush_async_reply ().
%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
flush(_Ctx,_Inode,_Fuse_File_Info,_Continuation,State) ->
  ?DEB1(">flush"),
  ?DEB2("|  _Ctx:~p",_Ctx),
  ?DEB2("|  Inode: ~p ",_Inode),
  ?DEB2("|  FI: ~p",_Fuse_File_Info),
  {#fuse_reply_err{err=ok},State}.

%%--------------------------------------------------------------------------
%% Forget about an inode. If noreply is used, eventually fuserlsrv:reply/2  should be called with Cont as first argument and the second argument of type forget_async_reply ().
%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
forget(_Ctx,Inode,_Nlookup,_Continuation,State) ->
  ?DEBL(">forget inode: ~p",[Inode]),
  attr_open:forget(Inode),
  {#fuse_reply_none{},State}.

%%--------------------------------------------------------------------------
%% Ensure all changes are on permanent storage. If the IsDataSync is true, only the user data should be flushed, not the meta data. If noreply is used, eventually fuserlsrv:reply/2  should be called with Cont as first argument and the second argument of type fsync_async_reply ().
%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
fsync(_Ctx,_Inode,_IsDataSync,_Fuse_File_Info,_Continuation,State) ->
  ?DEBL("~s",["fsync!"]),
  {#fuse_reply_err{err=enotsup},State}.

%%--------------------------------------------------------------------------
%% Ensure all directory changes are on permanent storage. If the IsDataSync is true, only the user data should be flushed, not the meta data. If noreply is used, eventually fuserlsrv:reply/2  should be called with Cont as first argument and the second argument of type fsyncdir_async_reply ().
%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
fsyncdir(_Ctx,_Inode,_IsDataSync,_Fuse_File_Info,_Continuation,State) ->
  ?DEBL("~s",["fsyncdir!"]),
  {#fuse_reply_err{err=enotsup},State}.
    
%%--------------------------------------------------------------------------
%% Get the file attributes associated with an inode. If noreply is used, eventually fuserlsrv:reply/2  should be called with Cont as first argument and the second argument of type getattr_async_reply ().
%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
getattr(#fuse_ctx{uid=_Uid,gid=_Gid,pid=_Pid},Inode,Continuation,State) ->
  ?DEBL(">getattr inode:~p",[Inode]),
  % I must do this by spawning, lest fuserl hangs erl and fuse!!!
  spawn(fun() -> getattr_internal(Inode,Continuation) end),
  {noreply,State}.



%%--------------------------------------------------------------------------
%% Test for POSIX file lock. If noreply is used, eventually fuserlsrv:reply/2  should be called with Cont as first argument and the second argument of type getlk_async_reply ().
%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
getlk(_Ctx,_Inode,_Fuse_File_Info,_Lock,_Continuation,State) ->
  ?DEBL("~s",["getlk!\n"]),
  {#fuse_reply_err{err=enotsup},State}.


%%--------------------------------------------------------------------------
%% Get the value of an extended attribute. If Size is zero, the size of the value should be sent with #fuse_reply_xattr{}. If Size is non-zero, and the value fits in the buffer, the value should be sent with #fuse_reply_buf{}. If Size is too small for the value, the erange error should be sent. If noreply is used, eventually fuserlsrv:reply/2  should be called with Cont as first argument and the second argument of type getxattr_async_reply ().
%%--------------------------------------------------------------------------
getxattr(_Ctx,Inode,BName,Size,_Continuation,State) ->
  RawName=attr_tools:remove_from_start(binary_to_list(BName),"user."),
  ?DEBL(">getxattr, name:~p, size:~p, inode:~p",[RawName,Size,Inode]),
  Name=
    case string:str(RawName,"system")==1 of
      true -> "."++RawName;
      false -> RawName
    end,
  {value,Entry}=tree_srv:lookup(Inode,inodes),
  ?DEB1("   Got inode entry"),
  ExtInfo=Entry#inode_entry.ext_info,
  ?DEB1("   Got extinfo"),
  Reply=case lists:keyfind(Name,1,ExtInfo) of
    {Name,ExtInfoValue} ->
      ?DEB1("   Got attribute value"),
      ExtAttrib=ExtInfoValue, %Seems I shouldn't 0-terminate the strings here.
      ExtSize=length(ExtAttrib),
      ?DEBL("   Converted attribute and got (~p,~p)",[ExtAttrib,ExtSize]),
      case Size == 0 of 
        true -> ?DEB1("   They want to know our size."),#fuse_reply_xattr{count=ExtSize};
        false -> 
          case Size < ExtSize of
            true -> ?DEBL("   They are using too small a buffer; ~p < ~p ",[Size,ExtSize]),#fuse_reply_err{err=erange};
            false -> ?DEB1("   All is well, replying with attrib value."),#fuse_reply_buf{buf=list_to_binary(ExtAttrib), size=ExtSize}
          end
      end;
    false ->
      ?DEB1("   Argument nonexistent, returning error"),
      #fuse_reply_err{err=enodata}
  end,
  {Reply,State}.
    
    
%%--------------------------------------------------------------------------
%% Create a hard link. Ino is the existing inode. If noreply is used, eventually fuserlsrv:reply/2 should be called with Cont as first argument and the second argument of type link_async_reply ().
%%--------------------------------------------------------------------------
%% On my system, the new name has to be the same as the old, and thus is ignored.
%%--------------------------------------------------------------------------
link(_Ctx,_Inode,_NewParent,_NewName,_Continuation,State) ->
  ?DEBL("~s",["link!"]),
  {#fuse_reply_err{err=enotsup},State}.
    
%%--------------------------------------------------------------------------
%% List extended attribute names. If Size is zero, the total size in bytes of the attribute name list (including null terminators) should be sent via #fuse_reply_xattr{}. If the Size is non-zero, and the null character separated and terminated attribute list is Size or less, the list should be sent with #fuse_reply_buf{}. If Size is too small for the value, the erange error should be sent.
%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
listxattr(_Ctx,Inode,Size,_Continuation,State) ->
  ?DEB2(">listxattr inode:~p",Inode),
  {value,Entry}=tree_srv:lookup(Inode,inodes),
  ?DEB1("   got inode entry."),
  {ExtSize,ExtAttribs}=Entry#inode_entry.ext_io,
  ?DEBL("   got attributes and size (~p,~p)",[ExtAttribs,ExtSize]),
  Reply=
    case Size == 0 of 
      true -> 
        ?DEB1("   they want to know our size."),
        #fuse_reply_xattr{count=ExtSize};
      false -> 
        case Size < ExtSize of
          true -> 
            ?DEBL("   they are using a too small buffer; ~p < ~p ",[Size,ExtSize]),
            #fuse_reply_err{err=erange};
          false -> ?DEB1("   all is well, replying with attribs."),#fuse_reply_buf{buf=list_to_binary(ExtAttribs), size=ExtSize}
        end
    end,
  {Reply,State}.

%%--------------------------------------------------------------------------
%% Lookup a directory entry by name and get its attributes. Returning an entry with inode zero means a negative entry which is cacheable, whereas an error of enoent is a negative entry which is not cacheable. If noreply is used, eventually fuserlsrv:reply/2  should be called with Cont as first argument and the second argument of type lookup_async_reply ().
%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
lookup(_Ctx,ParentInode,BinaryChild,_Continuation,State) ->
  Child=binary_to_list(BinaryChild),
  ?DEBL(">lookup Parent: ~p Name: ~p",[ParentInode,Child]),
  Reply=
    case attr_lookup:children(ParentInode) of
      {value,Children} ->
        ?DEBL("   Got children for ~p: ~p",[ParentInode, Children]),
        case lists:keysearch(Child,1,Children) of
          {value,{_,Inode,_}} ->
            ?DEB2("   Found child ~p",Child),
            {value,Entry} = tree_srv:lookup(Inode,inodes),
            ?DEB1("   Got child inode entry, returning..."),
            #fuse_reply_entry{fuse_entry_param=?ENTRY2PARAM(Entry,Inode)};
          false ->
            ?DEB1("   Child nonexistent!"),
            #fuse_reply_err{err=enoent} % child nonexistent.
        end;
      none -> 
        ?DEB1("   Parent nonexistent!"),
        #fuse_reply_err{err=enoent} %no parent
    end,
  {Reply,State}.

%%--------------------------------------------------------------------------
%% Make a directory. Mode is a mask composed of the ?S_XXXXX macros which are (portably) defined in fuserl.hrl. If noreply is used, eventually fuserlsrv:reply/2  should be called with Cont as first argument and the second argument of type mkdir_async_reply ().
%%--------------------------------------------------------------------------
%% This will have different meanings depending on parent type:
%% * #external_dir{}
%%   creating dirs in the real file system not yet supported.
%% * attr_dir 
%%   this will create an attribute value with name [value name|attr_dir name]
%%    support deep attributes.
%% * internal_dir
%%   this means we try to create a directory inside either /, or /real 
%%    that is, either we try to create a new directory universe (not allowed),
%%    or a new real dir (maybe supported later) 
%%--------------------------------------------------------------------------

mkdir(Ctx,ParentInode,BName,MMode,_Continuation,State) ->
  Name=binary_to_list(BName),
  ?DEB1(">mkdir"),
  ?DEB2("|  Ctx:~p",Ctx),
  ?DEB2("|  PIno: ~p",ParentInode),
  ?DEB2("|  Name: ~p",Name),
  ?DEB2("|  Mode: ~p",MMode),
  Mode=MMode bor ?S_IFDIR bor 8#111, % To the mode provided with, I add the dir status and make the dir executable.
  Reply=
    case tree_srv:lookup(ParentInode,inodes) of
      none -> #fuse_reply_err{err=enoent}; 
      {value,Parent} ->
        case inode:is_numbered(Name,ino) of
          false ->
            ParentName=Parent#inode_entry.name,
            ParentType=Parent#inode_entry.type,
            attr_mkdir:make_dir(Ctx,ParentInode,ParentName,ParentType,Name,Mode);
          _ -> #fuse_reply_err{err=eexist}
        end
    end,
  {Reply,State}.



%%--------------------------------------------------------------------------
%% Create a file node. Mode is a mask composed of the ?S_XXXXX macros which are (portably) defined in fuserl.hrl. Dev is only valid if the created file is a device. If noreply is used, eventually fuserlsrv:reply/2  should be called with Cont as first argument and the second argument of type mknod_async_reply ().
%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------

mknod(_Ctx,_ParentInode,_Name,_Mode,_Dev,_Continuation,State) ->
  ?DEB1(">mknod"),
  ?DEB2("|  Ctx: ~p",_Ctx),
  ?DEB2("|  ParentIno: ~p",_ParentInode),
  ?DEB2("|  Name: ~p",_Name),
  ?DEB2("|  Mode: ~p",_Mode),
  ?DEB2("|  Dev: ~p",_Dev),
  {#fuse_reply_err{err=enotsup},State}.

%%--------------------------------------------------------------------------
%% Open an inode. If noreply is used, eventually fuserlsrv:reply/2  should be called with Cont as first argument and the second argument of type open_async_reply ().
%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
open(_Ctx,Inode,Fuse_File_Info,_Continuation,State) ->
  ?DEB1(">open"),
  ?DEB2("|  _Ctx: ~p",_Ctx),
  ?DEB2("|  Inode: ~p ",Inode),
  ?DEB2("|  FI: ~p",Fuse_File_Info),
  {value,Entry}=tree_srv:lookup(Inode,inodes),
  _Name=Entry#inode_entry.name,
  ?DEBL("   Internal file name: ~p",[_Name]),
  Reply=
    case Entry#inode_entry.type of
      #external_file{path=Path} ->
        ?DEBL("   External file path: ~p",[Path]),
            case file:open(Path,[read,binary]) of
%            case file:open(Path,[read,raw]) of
                {ok,IoDevice} ->
                    MyFino=inode:get(fino),
                    attr_open:set(MyFino,Inode,#open_external_file{io_device=IoDevice,path=Path}),
                    #fuse_reply_open{fuse_file_info=Fuse_File_Info#fuse_file_info{fh=MyFino}};
                {error,Error} ->
                    #fuse_reply_err{err=Error}
            end;
%        #fuse_reply_open{fuse_file_info=Fuse_File_Info};
      _ ->
        #fuse_reply_err{err=enotsup}
    end,
  {Reply,State}.

%%--------------------------------------------------------------------------
%% Open an directory inode. If noreply is used, eventually fuserlsrv:reply/2  should be called with Cont as first argument and the second argument of type opendir_async_reply ().
%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
opendir(_Ctx,Inode,FI=#fuse_file_info{flags=_Flags,writepage=_Writepage,direct_io=_DirectIO,keep_cache=_KeepCache,flush=_Flush,fh=_Fh,lock_owner=_LockOwner},_Continuation,State) ->
  ?DEB1(">opendir"),
  ?DEB2("|  _Ctx:~p",_Ctx),
  ?DEB2("|  Inode: ~p ",Inode),
  ?DEB2("|  FI: ~p", FI),
  ?DEB2("|  _flags: ~p",_Flags),
%    ?DEB2("   writepage ~p",_Writepage),
%    ?DEB2("   DirectIO ~p",_DirectIO),
%    ?DEB2("   KeepCache ~p",_KeepCache),
%    ?DEB2("   FileHandle ~p",_Fh),
  ?DEB2("  Getting inode entries for ~p",Inode),
  ?DEB1("  Creating directory entries from inode entries"),
  % TODO: What to do if I get several opendir calls (from the same context?) while the dir is updating?
  MyFIno=inode:get(fino),
  attr_open:set(MyFIno,Inode,attr_opendir:direntries(Inode)),
  {#fuse_reply_open{ fuse_file_info = FI#fuse_file_info{fh=MyFIno} }, State}.

%%--------------------------------------------------------------------------
%% Read Size bytes starting at offset Offset. The file descriptor and other flags are available in Fi. If noreply is used, eventually fuserlsrv:reply/2 should be called with Cont as first argument and the second argument of type read_async_reply ().
%%--------------------------------------------------------------------------
%% To read an internal file that has an external counterpart, open the external file and forward the contents to the reader.
%% To read other internal files, I need to output the Right Kind Of Data somehow. A project to do later.
%%--------------------------------------------------------------------------
read(_Ctx,_Inode,Size,Offset,Fuse_File_Info,_Continuation,State) ->
  ?DEB1(">read"),
  ?DEB2("|  _Ctx:~p",_Ctx),
  ?DEB2("|  _Inode: ~p ",_Inode),
  ?DEB2("|  Size: ~p ",Size),
  ?DEB2("|  Offset: ~p",Offset),
  ?DEB2("|  FI: ~p", Fuse_File_Info),
    #fuse_file_info{fh=FIno}=Fuse_File_Info,
    ?DEB2("   looking up fino ~p",FIno),
    {value,File}=attr_open:lookup(FIno),
    ?DEB1("   extracting io_device"),
    #open_external_file{io_device=IoDevice}=File,
    ?DEB1("   got file and iodevice, reading..."),
    Reply=case file:pread(IoDevice,Offset,Size) of
      {ok, Data} ->
        ?DEB1("   data read, returning"),
        #fuse_reply_buf{ buf = Data, size=erlang:size(Data) };
      eof ->
        ?DEB1("   eof reached, returning nodata"),
        #fuse_reply_err{ err=enodata };
      _E ->
        ?DEB2("   other error, returning ~p",_E),
        #fuse_reply_err{ err=eio }
    end,
    {Reply,State}.

%%--------------------------------------------------------------------------
%% Read at most Size bytes at offset Offset from the directory identified Inode. Size is real and must be honored: the function fuserlsrv:dirent_size/1 can be used to compute the aligned byte size of a direntry, and the size of the list is the sum of the individual sizes. Offsets, however, are fake, and are for the convenience of the implementation to find a specific point in the directory stream. If noreply is used, eventually fuserlsrv:reply/2  should be called with Cont as first argument and the second argument of type readdir_async_reply ().
%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
readdir(_Ctx,_Inode,Size,Offset,Fuse_File_Info,_Continuation,State) ->
  ?DEB1(">readdir"),
  ?DEB2("|  _Ctx:~p",_Ctx),
  ?DEB2("|  _Inode: ~p ",_Inode),
  ?DEB2("|  Offset:~p",Offset),
  ?DEB2("|  FI: ~p",Fuse_File_Info),
  #fuse_file_info{fh=FIno}=Fuse_File_Info,
  Reply=
    case attr_open:lookup(FIno) of 
      {value, OpenFile} ->
        DirEntryList = 
          case Offset<length(OpenFile) of
            true ->
              attr_tools:take_while( 
                 fun (E, { Total, Max }) -> 
                   Cur = fuserlsrv:dirent_size (E),
                   if 
                     Total + Cur =< Max ->
                       { continue, { Total + Cur, Max } };
                     true ->
                       stop
                   end
                 end,
                   { 2, Size },
                   lists:nthtail(Offset,OpenFile) 
                  );
            false ->
                []
        end,
        ?DEB2("   returning ~p",DirEntryList),
        #fuse_reply_direntrylist{ direntrylist = DirEntryList };
    none ->
        #fuse_reply_err{err=enoent} % XXX: What should this REALLY return when the file is not open for this user?
  end,
  {Reply,State}.

%%--------------------------------------------------------------------------
%% Read the contents of a symbolic link. If noreply is used, eventually fuserlsrv:reply/2  should be called with Cont as first argument and the second argument of type readlink_async_reply ().
%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
readlink(_Ctx,_Inode,_Continuation,State) ->
  ?DEBL("~s",["readlink!"]),
  {#fuse_reply_err{err=enotsup},State}.

%%--------------------------------------------------------------------------
%% Called when there are no more references to a file. For every open call there is exactly one release call. Fi#fuse_file_info.fh will contain the descriptor set in open, if any. Fi#fuse_file_info.flags will contain the same flags as for open. #fuse_reply_err{err = ok} indicates success. Errors are not reported anywhere; use flush for that. If noreply is used, eventually fuserlsrv:reply/2 should be called with Cont as first argument and the second argument of type release_async_reply ().
%%--------------------------------------------------------------------------
%% Seems like this function does not give me any context information. Maybe I can only use this function as a semafor like server, where I cannot drop a resource until every reference to it has been released?
%%--------------------------------------------------------------------------
release(_Ctx,_Inode,Fuse_File_Info,_Continuation,State) ->
  ?DEB1(">release"),
  ?DEB2("|  _Ctx:~p",_Ctx),
  ?DEB2("|  _Inode: ~p ",_Inode),
  ?DEB2("|  FI: ~p",Fuse_File_Info),
  #fuse_file_info{fh=FIno}=Fuse_File_Info,
  attr_open:remove(FIno),
  {#fuse_reply_err{err=ok},State}.

%%--------------------------------------------------------------------------
% Called when there are no more references to a directory. For every opendir  call there is exactly one releasedir call. Fi#fuse_file_info.fh will contain the descriptor set in opendir, if any. Fi#fuse_file_info.flags will contain the same flags as for opendir. #fuse_reply_err{err = ok} indicates success. If noreply is used, eventually fuserlsrv:reply/2  should be called with Cont as first argument and the second argument of type releasedir_async_reply ().
%%--------------------------------------------------------------------------
%% TODO: Release dir info from open files in here. Make sure no other process tries to get to the same info etc.
%%--------------------------------------------------------------------------
releasedir(_Ctx,_Inode,Fuse_File_Info,_Continuation,State) ->
  ?DEB1(">releasedir"),
  ?DEB2("|  _Ctx:~p",_Ctx),
  ?DEB2("|  _Inode: ~p ",_Inode),
  ?DEB2("|  FI: ~p",Fuse_File_Info),
  #fuse_file_info{fh=FIno}=Fuse_File_Info,
  attr_open:remove(FIno),
  {#fuse_reply_err{err=ok},State}.

%%--------------------------------------------------------------------------
%%Remove an extended attribute. #fuse_reply_err{err = ok} indicates success. If noreply is used, eventually fuserlsrv:reply/2  should be called with Cont as first argument and the second argument of type removexattr_async_reply ().
%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
removexattr(_Ctx,Inode,BName,_Continuation,State) ->
  Name=attr_tools:remove_from_start(binary_to_list(BName),"user."),
  ?DEB1(">removexattr"),
  ?DEB2("|  _Ctx:~p",_Ctx),
  ?DEB2("|  Inode: ~p ",Inode),
  ?DEB2("|  Name: ~p",Name),
  {value,Entry} = tree_srv:lookup(Inode,inodes),
  case Entry#inode_entry.type of
    #external_file{ path=Path } ->
  Syek=string:tokens(Name,?KEY_SEP),
  ?DEB2("   tokenized key: ~p",[Syek]),
  Keys=lists:reverse(Syek),
  ?DEBL("   key to be deleted: ~p",[Keys]),
      attr_remove:remove_key_values(Path,Inode,Keys),
      ?DEB1("   Removed attribute, if any, from database and inode entry"),
      {#fuse_reply_err{err=ok},State};
    _ ->
      ?DEB1("   Non-supported inode type"),
      {#fuse_reply_err{err=enosup},State}
  end.


%%--------------------------------------------------------------------------
%% Rename a file. #fuse_reply_err{err = ok} indicates success. If noreply is used, eventually fuserlsrv:reply/2  should be called with Cont as first argument and the second argument of type rename_async_reply ().
%%--------------------------------------------------------------------------
%% This will have different meanings depending on parent type:
%% {Parent,NewParent,Child}
%% allowed:
%% {#external_dir{},#attribute_dir{},File}
%%   Since mv from an external filesystem does not mean a call to rename, this can only mean that the user tries to move a file
%%   from a real dir to an attribs dir
%%   append attribute, and possibly (always?) value (possibly empty?) to the file. Add File to the external dir.
%% {AttributeDir,AttributeDir2,File}:
%%   append attribute and value to File from AttributeDir2 
%% {AttributeDir,AttributeDir2,AttributeDir3}:
%%   this is the only case where I'm actually moving something.
%%   remove old attribute from applicable files, and add new one.
%%   remove attribute folder from old parent and add it to the new one.
%% {InternalDirWhoseNameIsAttribs,AttributeDir2,AttributeDir3} 
%% Not allowed:
%% {_,_}
%%--------------------------------------------------------------------------

rename(_Ctx,ParentIno,BName,NewParentIno,BNewName,_Continuation,State) ->
  Name=binary_to_list(BName),
  NewName=binary_to_list(BNewName),
  ?DEBL(">rename; parent: ~p, name: ~p, new parent: ~p, new name: ~p",[ParentIno,Name,NewParentIno,NewName]),
  Reply=attr_rename:rename(ParentIno,NewParentIno,Name,NewName),
  {#fuse_reply_err{err=Reply},State}.


%%--------------------------------------------------------------------------
%% Remove a directory. #fuse_reply_err{err = ok} indicates success. If noreply is used, eventually fuserlsrv:reply/2  should be called with Cont as first argument and the second argument of type rmdir_async_reply ().
%%--------------------------------------------------------------------------
%% For now, only removing of attribute (key and/or value) dirs are supported.
%%--------------------------------------------------------------------------
rmdir(_Ctx,ParentInode,BName,_Continuation,State) ->
  ?DEB1(">rmdir"),
  ?DEB2("|  Ctx:~p",_Ctx),
  ?DEB2("|  PIno: ~p ",ParentInode),
  ?DEB2("|  BName: ~p",BName),
  {value,PEntry}=tree_srv:lookup(ParentInode,inodes),
  PType=PEntry#inode_entry.type,
  PName=PEntry#inode_entry.name,
  ?DEBL("   parent type ~p",[PType]),
  % So I don't have to lookup two entries, I match the parent type instead of the child type.
  Reply=
    case PType of
      attribute_dir ->
        Name=binary_to_list(BName),
        attr_remove:remove_child_from_parent(Name,PName),
        {ok,OldIno}=inode:n2i(Name,ino),
        inode:release(OldIno,ino),
        ok;
      _ ->
        enotsup
    end,
  {#fuse_reply_err{err=Reply},State}.

%%--------------------------------------------------------------------------
%% Set file attributes. ToSet is a bitmask which defines which elements of Attr are defined and should be modified. Possible values are defined as ?FUSE_SET_ATTR_XXXX in fuserl.hrl . Fi will be set if setattr is invoked from ftruncate under Linux 2.6.15 or later. If noreply is used, eventually fuserlsrv:reply/2  should be called with Cont as first argument and the second argument of type setattr_async_reply ().
%%--------------------------------------------------------------------------
%% XXX: Seems that this function is NOT called when chmod:ing or chgrp:ing in linux. Why, oh, why?
%%--------------------------------------------------------------------------
setattr(_Ctx,Inode,Attr,ToSet,_Fuse_File_Info,_Continuation,State) ->
  ?DEB1(">setattr"),
  ?DEB2("|  Inode: ~p ",Inode),
  ?DEB2("|  To set: ~p ",ToSet),
  ?DEB2("|  Attr: ~p ",Attr),
  ?DEB2("|  _FI: ~p",_Fuse_File_Info),
  {value,Entry}=tree_srv:lookup(Inode,inodes),
  Stat=Entry#inode_entry.stat,
  NewStat=Stat#stat{
    st_uid=
      case (?FUSE_SET_ATTR_UID band ToSet) == 0 of
        false ->
          ?DEB1("    setting UID"),
          Attr#stat.st_uid;
        true ->
          ?DEB1("    not setting UID"),
          Stat#stat.st_uid
      end
    ,st_gid=
      case (?FUSE_SET_ATTR_GID band ToSet) == 0 of
        false ->
          ?DEB1("    setting GID"),
          Attr#stat.st_gid;
        true ->
          ?DEB1("    not setting GID"),
          Stat#stat.st_gid
      end
    ,st_atime=
      case (?FUSE_SET_ATTR_ATIME band ToSet) == 0 of
        false ->
          ?DEB1("    setting atime"),
          Attr#stat.st_atime;
        true ->
          ?DEB1("    not setting atime"),
          Stat#stat.st_atime
      end
    ,st_mtime=
      case (?FUSE_SET_ATTR_MTIME band ToSet) == 0 of
        false ->
          ?DEB1("    setting mtime"),
          Attr#stat.st_mtime;
        true ->
          ?DEB1("    not setting mtime"),
          Stat#stat.st_mtime
      end
    ,st_mode=
      case (?FUSE_SET_ATTR_MODE band ToSet) == 0 of
        false ->
          ?DEB1("    setting mode"),
          Attr#stat.st_mode;
        true ->
          ?DEB1("    not setting mode"),
          Stat#stat.st_mode
      end
    ,st_size=
      case (?FUSE_SET_ATTR_SIZE band ToSet) == 0 of
        false ->
          ?DEB1("    setting size"),
          Attr#stat.st_size;
        true ->
          ?DEB1("    not setting size"),
          Stat#stat.st_size
      end
  },
  tree_srv:enter(Inode,Entry#inode_entry{stat=NewStat},inodes),
  {#fuse_reply_attr{attr=NewStat,attr_timeout_ms=100000},State}.




%%--------------------------------------------------------------------------
%% Set a POSIX file lock. Sleep indicates whether the operation is blocking (true) or nonblocking (false). #fuse_reply_err{err = ok} indicates success. If noreply is used, eventually fuserlsrv:reply/2  should be called with Cont as first argument and the second argument of type setlk_async_reply ().
%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
setlk(_Ctx,_Inode,_Fuse_File_Info,_Lock,_Sleep,_Continuation,State) ->
  ?DEBL("~s",["setlk!"]),
  {#fuse_reply_err{err=enotsup},State}.

%%--------------------------------------------------------------------------
%% Set file attributes. #fuse_reply_err{err = ok} indicates success. Flags is a bitmask consisting of ?XATTR_XXXXX macros portably defined in fuserl.hrl . If noreply is used, eventually fuserlsrv:reply/2  should be called with Cont as first argument and the second argument of type setxattr_async_reply ().
%%--------------------------------------------------------------------------
%% TODO: if attribute key already has a value for the current file, remove the file from the old value dir!
%%--------------------------------------------------------------------------
setxattr(_Ctx,Inode,BKey,BValue,_Flags,_Continuation,State) ->
  ?DEB1(">setxattr"),
  ?DEB2("|  _Ctx:~p",_Ctx),
  ?DEB2("|  Inode: ~p ",Inode),
  RawKey=binary_to_list(BKey),
  RawValue=binary_to_list(BValue),
  ?DEB2("|  Key: ~p ",RawKey),
  ?DEB2("|  Value: ~p ",RawValue),
  ?DEB2("|  _Flags: ~p ",_Flags),
  ?DEB1("   getting inode entry"),
  {value,Entry} = tree_srv:lookup(Inode,inodes),
  ?DEB1("   transforming input data"),
  Key=
    case string:str(RawKey,"system")==1 of
      true -> "."++RawKey;
      false -> attr_tools:remove_from_start(RawKey,"user.")
    end,

  Values=string:tokens(RawValue,?VAL_SEP),
  ?DEB2("   got values: ~p",Values),

  Reply=
    case Entry#inode_entry.type of
      #external_file{path=Path} ->
        ?DEB1("   entry is an external file"),
        Syek=string:tokens(Key,?KEY_SEP),
        ?DEB2("   tokenized key: ~p",[Syek]),
        Keys=lists:reverse(Syek),
        ?DEBL("   key to be inserted: ~p",[Keys]),
        case Values of
          [] -> attr_ext:add_new_attribute(Path,Inode,Entry,Keys);
          _Values ->
            lists:foreach(
              fun(Value) -> 
                Attr=[Value|Keys],
                ?DEBL("   adding attribute {~p} for file ~p to database",[Attr,Path]),
                attr_ext:add_new_attribute(Path,Inode,Entry,Attr)
              end,
              Values)
        end,
        #fuse_reply_err{err=ok};
      _ ->
        ?DEB1("   entry not an external file, skipping..."),
        #fuse_reply_err{err=enotsup}
    end,
  {Reply,State}.


%%--------------------------------------------------------------------------
%% Get file system statistics. If noreply is used, eventually fuserlsrv:reply/2  should be called with Cont as first argument and the second argument of type statfs_async_reply ().
%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
statfs(_Ctx,_Inode,_Continuation,State) ->
  ?DEB1(">statfs"),
  ?DEB2("|  _Ctx:~p",_Ctx),
  ?DEB2("|  _Inode: ~p ",_Inode),
  {#fuse_reply_statfs{
    statvfs=
      #statvfs{
        f_bsize=512, % should be the same as the file system we're mirroring.
%            f_frsize=2048, % What does this do?
        f_blocks=1234567890, % size = f_blocks*f_frsize
        f_bfree=314159265,
        f_bavail=271828182,
        f_files=1000000000000000, % at least!
        f_ffree=1000000000000000-inode:count_occupied(ino), % at least!
        f_favail=1000000000000000-inode:count_occupied(ino), % at least!
%            f_fsid=0, % how to get this right?
%            f_flag=0, % Hm...
        f_namemax=10000 % Or some other arbitary high value.
      }
    },
    State
  }.


%    {#fuse_reply_err{err=enotsup},State}.

%%--------------------------------------------------------------------------
%% Create a symbolic link. Link is the contents of the link. Name is the name to create. If noreply is used, eventually fuserlsrv:reply/2  should be called with Cont as first argument and the second argument of type symlink_async_reply ().
%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
symlink(_Ctx,_Link,_Inode,_Name,_Continuation,State) ->
  ?DEB1(">symlink (enotsup)"),
  {#fuse_reply_err{err=enotsup},State}.

%%--------------------------------------------------------------------------
%% Remove a file. #fuse_reply_err{err = ok} indicates success. If noreply is used, eventually fuserlsrv:reply/2  should be called with Cont as first argument and the second argument of type unlink_async_reply ().
%%--------------------------------------------------------------------------
%% We need to check the dir type of the parent to know what to do when
%% unlinking a file.
%% For now, let's only support unlinking files residing in attribute dirs.
%%--------------------------------------------------------------------------
unlink(_Ctx,ParentInode,BName,_Cont,State) ->
  ?DEB1(">unlink"),
  ?DEB2("|  _Ctx:~p",_Ctx),
  ?DEB2("|  PIno: ~p ",ParentInode),
  ?DEB2("|  BName: ~p",BName),
  {value,ParentEntry}=tree_srv:lookup(ParentInode,inodes),
  ParentType=ParentEntry#inode_entry.type,
  ParentName=ParentEntry#inode_entry.name,
  ?DEBL("parent type ~p",[ParentType]),
    Reply=
      case ParentType of
        attribute_dir ->
          Name=binary_to_list(BName),
          ParentChildren=ParentEntry#inode_entry.children,
          case lists:keyfind(Name,1,ParentChildren) of
            false -> 
              ?DEBL("~p not found in ~p",[Name,ParentChildren]),
              #fuse_reply_err{err=enoent};
            {Name,Inode,Type} ->
              ?DEB1("found"),
              %% Removing a file from an attribute folder is 
              %% ONLY ALMOST the same as removing the corresponding 
              %% attribute from the file; Removing a value subfolder
              %% is NOT the same as removing a whole attribute.
              %% Thus, I'm NOT calling removexattr here.
              ?DEBL("child type ~p",[Type]),
              case Type of 
                #external_file{path=Path} ->
                  ?DEBL("Removing ~p from ~p", [ParentEntry#inode_entry.name,Name]),
                  attr_remove:remove_attribute(Path,Inode,ParentName),
                  #fuse_reply_err{err=ok};
                _ ->
                  #fuse_reply_err{err=enotsup}
              end
          end;
        _ ->
          #fuse_reply_err{err=enotsup}
      end,
  {Reply,State}.

%%--------------------------------------------------------------------------
%% Write data to a file. If noreply is used, eventually fuserlsrv:reply/2  should be called with Cont as first argument and the second argument of type write_async_reply ().
%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
write(_Ctx,_Inode,_Data,_Offset,_Fuse_File_Info,_Continuation,State) ->
  ?DEB1(">write (enotsup)"),
  ?DEB2("|  _Inode: ~p",_Inode),
  ?DEB2("|  _Offset: ~p",_Offset),
  ?DEB2("|  _FI: ~p",_Fuse_File_Info),
  {#fuse_reply_err{err=enotsup},State}.

%%%=========================================================================
%%%                        NON-API FUNCTIONS 
%%%=========================================================================

%%%=========================================================================
%%% Callback function internals
%%%=========================================================================
%%--------------------------------------------------------------------------
%% Getattr internal functions
%%--------------------------------------------------------------------------
getattr_internal(Inode,Continuation) ->
  ?DEB2("  >getattr_internal inode:~p",Inode),
  case tree_srv:lookup(Inode,inodes) of
    none ->
      ?DEB1("   Non-existent file"),
      Reply=#fuse_reply_err{err=enoent};
    {value,Entry} ->
      ?DEB1("   File exists, returning info"),
      Reply=
        #fuse_reply_attr{
          attr=Entry#inode_entry.stat,
          attr_timeout_ms=5
        };
    _A -> 
      ?DEB2("   This should not be happening: ~p",_A),
      Reply=#fuse_reply_err{err=enotsup}
  end,
  ?DEB1("   Sending reply"),
  fuserlsrv:reply(Continuation,Reply).

%%%=========================================================================
%%%                       SHARED UTILITY FUNCTIONS
%%%=========================================================================

%%%=========================================================================
%%%                         DEBUG FUNCTIONS
%%%=========================================================================

%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
dump_inode_entries() ->
  lists:map(fun({Inode,#inode_entry{name=Name,children=Children}}) -> {Inode,Name,Children} end, tree_srv:to_list(inodes)).

%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
dump_entries(Table) ->
  tree_srv:to_list(Table).

%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
dump_inodes() ->
  inode:list_bound(ino).

%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
dump_finodes() ->
  inode:list_bound(fino).
