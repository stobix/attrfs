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

% No exports, this is the main server file.

%%%=========================================================================
%%% server function exports
%%%=========================================================================
-export([handle_info/2,init/1,terminate/2]).
% TODO: Do something intelligent with this one. 
% Returns "ok"now, totally ignoring its indata.
-export([code_change/3]). 
-export([start_link/0]).

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
         dump_inode_entries/0,
         which_dirs/0]).

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
  ?REPORT(handle_info),
  ?DEBL(1,">handle_info(~w)",[_Msg]),
  {noreply,State}.

%%--------------------------------------------------------------------------
%% The analog to Module:code_change/3 in gen_server.
%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
code_change(_,_,_) ->
  ?REPORT(code_change), 
  %XXX: Maybe do something more intelligent with this?
  ?DEB1(1,">code_change: Doing nothing!"),
  ok.

%%---------------------
%% Just a small utility function used by start link to filter out good initial values or crash
  vget(Attribute) ->
  case options:get(attrfs,Attribute) of
    {ok,Value} -> 
      ?DEBL(2,"~p: ~p",[Attribute,Value]),
      Value;
    undefined -> 
      ?DEB2(1,"~p not defined! check your config file!",Attribute),
    exit("not found")
  end.

%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
%% In here, I mostly check for dirs needed to start fuserlsrv.
%--------------------------------------------------------------------------
start_link() ->
  ?REPORT(start_link),
  ?DEB1(1,">start_link"),
  ?DEB1(2,"getting config options..."),

  MirrorDirs=case options:mget(?MODULE,from_dir) of
    {string,MD} -> [MD]; % One from dir defined in config file
    {ok,MD} -> [MD]; % from_dir defined in attrfs.app
    {list,MD} -> MD; % Several from dir defined in config file
    undefined -> 
        case options:get(from_dirs) of
            {ok,MD} -> MD; % from_dirs defined in attrfs.app
            undefined -> 
                ?DEB1(err,"You must define at least one from dir in your config file to run attrfs!"),
                {error,dir_not_specified}
        end
  end,
  Dir=vget(to_dir),
  DB=vget(attributes_db),
  LinkedIn=vget(linked_in),
  MountOpts=vget(mount_opts),

  ?DEB1(2,"checkning if dirs are ok..."),
  ?DEB2(2,"~p...",Dir),
  case filelib:ensure_dir(Dir) of
    ok -> 
      ?DEB1(2,"path ok."),
      case filelib:is_dir(Dir) of
        true ->
          ?DEB2(2,"~p exists, and is a dir. Ok.",Dir);
        false->
          ?DEB2(err,"~p is not a directory, or already mounted! (Check your config, mount and fusermount)",Dir),
          ?DEB1(err,"TERMINATING"),
          throw({error,dir_to_is_not_a_dir,Dir})
      end;
    E ->
      ?DEB2(1,"received ~p (Check your config)",E),
      ?DEB1(1,"TERMINATING"),
      exit(E)
  end,
  ?DEB1(2,"checking mirror dirs..."),
  lists:foreach(
    fun(MirrorDir) ->
      case filelib:is_dir(MirrorDir) of
        true ->
          ?DEB2(2,"~p exists, and is a dir. Ok.",MirrorDir);
        false->
          ?DEB2(err,"~p is not a directory!(Check your config)",MirrorDir),
          ?DEB1(err,"TERMINATING"),
          throw({error,dir_from_is_not_a_dir,MirrorDir})
      end
  end,
  MirrorDirs),
  Options=[],
  ?DEB1(2,"starting fuserlsrv"),
  fuserlsrv:start_link(?MODULE,LinkedIn,MountOpts,Dir,{MirrorDirs,DB},Options).

%%--------------------------------------------------------------------------
%% The analog to Module:init/1 in gen_server.
%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
init({MirrorDir,DB}) ->
  ?REPORT(init),
  attr_init:init({MirrorDir,DB}),
  State=[],
  {ok,State}.



%%--------------------------------------------------------------------------
%% The analog to Module:terminate/2 in gen_server.
%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
terminate(_Reason,_State) ->
  ?REPORT(terminate),
  ?DEB1(1,">terminate"),
  ?DEB1(1,"\n\nXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n\n"),
  ?DEB2(1,"|  _Reason: ~p",_Reason),
  ?DEB2(1,"Closing database \"~p\"",?ATTR_DB),
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
access(Ctx,Inode,Mask,Continuation,State) ->
  ?REPORT(access),
  ?DEB1(1,">access"),
  ?DEB2(2,"|  Ctx: ~w ",Ctx),
  ?DEB2(2,"|  Inode: ~w ",Inode),
  ?DEB2(2,"|  Mask: ~w ",Mask),
  attr_reply:watch(
      fun(Token) -> attr_async:access(Ctx,Inode,Mask,Token) end,
      Continuation,
      #fuse_reply_err{err=eacces}),
  {noreply,State}.

%%--------------------------------------------------------------------------
%% Create and open a file. Mode is a bitmask consisting of ?S_XXXXX macros portably defined in fuserl.hrl . If noreply is used, eventually fuserlsrv:reply/2  should be called with Cont as first argument and the second argument of type create_async_reply (). 
%%--------------------------------------------------------------------------
%% Where will I allow this? Creating new real files in the real branch? Creating imaginary files elsewhere?
%% The possibility of making a .Trash in the root folder should be considered.
%% What should happen if I try to create an already existing file? Error return? Open file return?
%%  Let's go for open file for now.
%%
%% A file is allowed to be created in an attribs folder iff it already exists by that name somewhere else. This makes "copying"files possible.
%%--------------------------------------------------------------------------
create(Ctx=#fuse_ctx{gid=Gid,uid=Uid},ParentInode,BName,Mode,Fuse_File_Info,_Continuation, State) ->
  ?REPORT(create),
  ?DEB1(1,">create"), 
  ?DEB2(2,"|  Ctx: ~w",Ctx),
  ?DEB2(2,"|  ParentIno: ~w",ParentInode),
  ?DEB2(2,"|  Name: ~s",BName),
  ?DEBL(2,"|  Mode: ~.8X",[Mode,"O"]),
  ?DEB2(2,"|  FI: ~w",Fuse_File_Info),
  Name=binary_to_list(BName),
  Reply=case inode:is_numbered(Name,ino) of
    false -> 
      ?DEB1(4,"No real file with that name, checking parent."),
      {value,ParentEntry}=tree_srv:lookup(ParentInode,inodes),
      case ParentEntry#inode_entry.type of
        attribute_dir ->
          ?DEB1(4,"Parent is an attribute dir. Creating internal file."),
          {ok,Inode}=inode:number(Name,ino),
          NewEntry=#inode_entry{
            type=internal_file,
            contents= <<>>,
            name=Name,
            stat=?FILE_STAT(Mode,Inode,0)#stat{st_gid=Gid,st_uid=Uid},
            ext_info=[],
            ext_io=attr_ext:ext_info_to_ext_io([])
          },
          tree_srv:insert(Inode,NewEntry,inodes),
          attr_mkdir:insert_entry(ParentInode,NewEntry),
            {#fuse_reply_open{fuse_file_info=FileInfo},_}=open(Ctx,Inode,Fuse_File_Info,_Continuation,[create,State]), 
          #fuse_reply_create{
            fuse_file_info=FileInfo,
            fuse_entry_param=?ENTRY2PARAM(NewEntry,Inode)
          };
        _ ->
          #fuse_reply_err{err=enotsup}
      end;
    Inode -> 
      {value,Entry}=tree_srv:lookup(Inode,inodes),
      case Entry#inode_entry.type of
        #external_file{path=Path} ->
          ?DEB1(4,"Filename linked to a real file."),
          {value,ParentEntry}=tree_srv:lookup(ParentInode,inodes),
          case ParentEntry#inode_entry.type of
            attribute_dir ->
              ?DEB1(4,"Parent is an attribute dir. All is ok."),
              % Adding a new attribute is the same as creating an attribute folder file entry.
              attr_ext:add_new_attribute(Path,Inode,Entry,ParentEntry#inode_entry.name),
              {#fuse_reply_open{fuse_file_info=FileInfo},_}=open(Ctx,Inode,Fuse_File_Info,_Continuation,[create,State]), 
              #fuse_reply_create{
                fuse_file_info=FileInfo,
                fuse_entry_param=?ENTRY2PARAM(Entry,Inode)
              };
            _ ->
              ?DEB1(4,"Trying to create a file outside the attribute folder! Not permitted!"),
              #fuse_reply_err{err=enotsup}
            end;
        _ ->
          ?DEB1(4,"The file with that name has the wrong type!"),
          #fuse_reply_err{err=enotsup}
      end
  end,
  {Reply,State}.


%%--------------------------------------------------------------------------
%% This is called on each close () of an opened file, possibly multiple times per open  call (due to dup () et. al.). Fi#fuse_file_info.fh will contain the descriptor set in open, if any. #fuse_reply_err{err = ok} indicates success. This return value is ultimately the return value of close () (unlike release). Does *not* necessarily imply an fsync. If noreply is used, eventually fuserlsrv:reply/2  should be called with Cont as first argument and the second argument of type flush_async_reply ().
%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
flush(_Ctx,_Inode,_Fuse_File_Info,_Continuation,State) ->
  ?REPORT(flush),
  ?DEB1(1,">flush"),
  ?DEB2(2,"|  _Ctx:~w",_Ctx),
  ?DEB2(2,"|  Inode: ~w ",_Inode),
  ?DEB2(2,"|  FI: ~w",_Fuse_File_Info),
  {#fuse_reply_err{err=ok},State}.

%%--------------------------------------------------------------------------
%% Forget about an inode. If noreply is used, eventually fuserlsrv:reply/2  should be called with Cont as first argument and the second argument of type forget_async_reply ().
%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
forget(_Ctx,Inode,_Nlookup,_Continuation,State) ->
  ?REPORT(forget),
  ?DEBL(1,">forget inode: ~w",[Inode]),
  attr_open:forget(Inode),
  {#fuse_reply_none{},State}.

%%--------------------------------------------------------------------------
%% Ensure all changes are on permanent storage. If the IsDataSync is true, only the user data should be flushed, not the meta data. If noreply is used, eventually fuserlsrv:reply/2  should be called with Cont as first argument and the second argument of type fsync_async_reply ().
%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
fsync(_Ctx,_Inode,_IsDataSync,_Fuse_File_Info,_Continuation,State) ->
  ?REPORT(fsync),
  ?DEBL(1,"~s",["fsync!"]),
  {#fuse_reply_err{err=ok},State}.

%%--------------------------------------------------------------------------
%% Ensure all directory changes are on permanent storage. If the IsDataSync is true, only the user data should be flushed, not the meta data. If noreply is used, eventually fuserlsrv:reply/2  should be called with Cont as first argument and the second argument of type fsyncdir_async_reply ().
%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
fsyncdir(_Ctx,_Inode,_IsDataSync,_Fuse_File_Info,_Continuation,State) ->
  ?REPORT(fsyncdir),
  ?DEBL(1,"~s",["fsyncdir!"]),
  {#fuse_reply_err{err=enotsup},State}.
    
%%--------------------------------------------------------------------------
%% Get the file attributes associated with an inode. If noreply is used, eventually fuserlsrv:reply/2  should be called with Cont as first argument and the second argument of type getattr_async_reply ().
%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
getattr(#fuse_ctx{uid=_Uid,gid=_Gid,pid=_Pid},Inode,Continuation,State) ->
  ?REPORT(getattr),
  ?DEBL(1,">getattr inode:~w",[Inode]),
  % I must do this by spawning, lest fuserl hangs erl and fuse!!!
  attr_reply:watch(
      fun(Token) -> attr_async:getattr(Inode,Token) end,
      Continuation,
      #fuse_reply_err{err=enotsup}),
  {noreply,State}.



%%--------------------------------------------------------------------------
%% Test for POSIX file lock. If noreply is used, eventually fuserlsrv:reply/2  should be called with Cont as first argument and the second argument of type getlk_async_reply ().
%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
getlk(_Ctx,_Inode,_Fuse_File_Info,_Lock,_Continuation,State) ->
  ?REPORT(getlk),
  ?DEBL(1,"~s",["getlk!\n"]),
  {#fuse_reply_err{err=enotsup},State}.


%%--------------------------------------------------------------------------
%% Get the value of an extended attribute. If Size is zero, the size of the value should be sent with #fuse_reply_xattr{}. If Size is non-zero, and the value fits in the buffer, the value should be sent with #fuse_reply_buf{}. If Size is too small for the value, the erange error should be sent. If noreply is used, eventually fuserlsrv:reply/2  should be called with Cont as first argument and the second argument of type getxattr_async_reply ().
%%--------------------------------------------------------------------------
getxattr(_Ctx,Inode,BName,Size,Continuation,State) ->
  ?REPORT(getxattr),
  RawName=attr_tools:remove_from_start(binary_to_list(BName),"user."),
  ?DEBL(1,">getxattr, name:~p, size:~w, inode:~w",[RawName,Size,Inode]),
  attr_reply:watch(fun(Token) -> attr_async:getxattr(Inode,RawName,Size,Token) end,
    Continuation,
    #fuse_reply_err{err=enodata}),
  {noreply,State}.
    
    
%%--------------------------------------------------------------------------
%% Create a hard link. Ino is the existing inode. If noreply is used, eventually fuserlsrv:reply/2 should be called with Cont as first argument and the second argument of type link_async_reply ().
%%--------------------------------------------------------------------------
%% On my system, the new name has to be the same as the old, and thus is ignored.
%%--------------------------------------------------------------------------
link(_Ctx,_Inode,_NewParent,_NewName,_Continuation,State) ->
  ?REPORT(link),
  ?DEBL(1,"~s",["link!"]),
  {#fuse_reply_err{err=enotsup},State}.
    
%%--------------------------------------------------------------------------
%% List extended attribute names. If Size is zero, the total size in bytes of the attribute name list (including null terminators) should be sent via #fuse_reply_xattr{}. If the Size is non-zero, and the null character separated and terminated attribute list is Size or less, the list should be sent with #fuse_reply_buf{}. If Size is too small for the value, the erange error should be sent.
%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
listxattr(_Ctx,Inode,Size,_Continuation,State) ->
  ?REPORT(listxattr),
  ?DEB2(1,">listxattr inode:~w",Inode),
  {value,Entry}=tree_srv:lookup(Inode,inodes),
  ?DEB1(4,"got inode entry."),
  {ExtSize,ExtAttribs}=Entry#inode_entry.ext_io,
  ?DEBL(4,"got attributes and size (~w,~w)",[ExtAttribs,ExtSize]),
  Reply=
    case Size == 0 of 
      true -> 
        ?DEB1(4,"they want to know our size."),
        #fuse_reply_xattr{count=ExtSize};
      false -> 
        case Size < ExtSize of
          true -> 
            ?DEBL(4,"they are using a too small buffer; ~w < ~w ",[Size,ExtSize]),
            #fuse_reply_err{err=erange};
          false -> ?DEB1(4,"all is well, replying with attribs."),#fuse_reply_buf{buf=list_to_binary(ExtAttribs), size=ExtSize}
        end
    end,
  {Reply,State}.

%%--------------------------------------------------------------------------
%% Lookup a directory entry by name and get its attributes. Returning an entry with inode zero means a negative entry which is cacheable, whereas an error of enoent is a negative entry which is not cacheable. If noreply is used, eventually fuserlsrv:reply/2  should be called with Cont as first argument and the second argument of type lookup_async_reply ().
%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
lookup(_Ctx,ParentInode,BinaryChild,Continuation,State) ->
  ?REPORT(lookup),
  Child=binary_to_list(BinaryChild),
  ?DEBL(1,">lookup Parent: ~p Name: ~s",[ParentInode,Child]),
    attr_reply:watch(
      fun(Token) -> attr_async:lookup(ParentInode,Child,Token) end,
      Continuation,
      #fuse_reply_err{err=enoent}),
  {noreply,State}.

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
  ?REPORT(mkdir),
  Name=binary_to_list(BName),
  ?DEB1(1,">mkdir"),
  ?DEB2(2,"|  Ctx:~w",Ctx),
  ?DEB2(2,"|  PIno: ~w",ParentInode),
  ?DEB2(2,"|  Name: ~p",Name),
  ?DEB2(2,"|  Mode: ~.8B",MMode),
  Mode=?M_DIR(MMode bor 8#111), % To the mode provided with, I add the dir status and make the dir executable.
  Reply=
    case tree_srv:lookup(ParentInode,inodes) of
      none -> #fuse_reply_err{err=enoent}; 
      {value,Parent} ->
        ParentName=Parent#inode_entry.name,
        ParentType=Parent#inode_entry.type,
        Rreply=attr_mkdir:make_dir(Ctx,ParentInode,ParentName,ParentType,Name,Mode),
        ?DEB2(4,"got reply ~p",Rreply),
        Rreply
    end,
  {Reply,State}.



%%--------------------------------------------------------------------------
%% Create a file node. Mode is a mask composed of the ?S_XXXXX macros which are (portably) defined in fuserl.hrl. Dev is only valid if the created file is a device. If noreply is used, eventually fuserlsrv:reply/2  should be called with Cont as first argument and the second argument of type mknod_async_reply ().
%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------

mknod(_Ctx,_ParentInode,_Name,_Mode,_Dev,_Continuation,State) ->
  ?REPORT(mknod),
  ?DEB1(1,">mknod"),
  ?DEB2(2,"|  Ctx: ~w",_Ctx),
  ?DEB2(2,"|  ParentIno: ~w",_ParentInode),
  ?DEB2(2,"|  Name: ~w",_Name),
  ?DEB2(2,"|  Mode: ~w",_Mode),
  ?DEB2(2,"|  Dev: ~w",_Dev),
  {#fuse_reply_err{err=enotsup},State}.

%%--------------------------------------------------------------------------
%% Open an inode. If noreply is used, eventually fuserlsrv:reply/2  should be called with Cont as first argument and the second argument of type open_async_reply ().
%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
open(_Ctx,Inode,Fuse_File_Info,_Continuation,State) ->
  ?REPORT(open),
  ?DEB1(1,">open"),
  ?DEB2(2,"|  _Ctx: ~w",_Ctx),
  ?DEB2(2,"|  Inode: ~w ",Inode),
  ?DEB2(2,"|  FI: ~w",Fuse_File_Info),
  {value,Entry}=tree_srv:lookup(Inode,inodes),
  _Name=Entry#inode_entry.name,
  ?DEBL(2,"Internal file name: ~p",[_Name]),
  Reply=
    case Entry#inode_entry.type of
      #external_file{path=Path} ->
        ?DEBL(4,"External file path: ~p",[Path]),
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
      duplicate_file ->
        ?DEB1(4,"duplicate file"),
        MyFino=inode:get(fino),
        attr_open:set(MyFino,Inode,#open_duplicate_file{contents=Entry#inode_entry.contents}),
        #fuse_reply_open{fuse_file_info=Fuse_File_Info#fuse_file_info{fh=MyFino}};
      internal_file ->
        ?DEB1(4,"internal file"),
        MyFino=inode:get(fino),
        attr_open:set(MyFino,Inode,#open_duplicate_file{contents=Entry#inode_entry.contents}),
        #fuse_reply_open{fuse_file_info=Fuse_File_Info#fuse_file_info{fh=MyFino}};

      _ ->
        #fuse_reply_err{err=enotsup}

    end,
  {Reply,State}.

%%--------------------------------------------------------------------------
%% Open an directory inode. If noreply is used, eventually fuserlsrv:reply/2  should be called with Cont as first argument and the second argument of type opendir_async_reply ().
%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
opendir(_Ctx,Inode,FI=#fuse_file_info{flags=_Flags,writepage=_Writepage,direct_io=_DirectIO,keep_cache=_KeepCache,flush=_Flush,fh=_Fh,lock_owner=_LockOwner},_Continuation,State) ->
  ?REPORT(opendir),
  ?DEB1(1,">opendir"),
  ?DEB2(2,"|  _Ctx:~w",_Ctx),
  ?DEB2(2,"|  Inode: ~w ",Inode),
  ?DEB2(2,"|  FI: ~w", FI),
  ?DEB2(2,"|  _flags: ~w",_Flags),
%    ?DEB2("writepage ~w",_Writepage),
%    ?DEB2("DirectIO ~w",_DirectIO),
%    ?DEB2("KeepCache ~w",_KeepCache),
%    ?DEB2("FileHandle ~w",_Fh),
  ?DEB2(4,"Getting inode entries for ~w",Inode),
  ?DEB1(4,"Creating directory entries from inode entries"),
  % TODO: What to do if I get several opendir calls (from the same context?) while the dir is updating?
  ?REPORT(opendir),
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
  ?REPORT(read),
  ?DEB1(1,">read"),
  ?DEB2(2,"|  _Ctx:~w",_Ctx),
  ?DEB2(2,"|  _Inode: ~w ",_Inode),
  ?DEB2(2,"|  Size: ~w ",Size),
  ?DEB2(2,"|  Offset: ~w",Offset),
  ?DEB2(2,"|  FI: ~w", Fuse_File_Info),
    #fuse_file_info{fh=FIno}=Fuse_File_Info,
    ?DEB2(4,"looking up fino ~w",FIno),
    {value,File}=attr_open:lookup(FIno),
    ?DEB1(4,"extracting io_device"),
    Reply=case File of
      #open_external_file{io_device=IoDevice} -> 
        ?DEB1(4,"got file and iodevice, reading..."),
        case file:pread(IoDevice,Offset,Size) of
          {ok, Data} ->
            ?DEB1(4,"data read, returning"),
            #fuse_reply_buf{buf=Data,size=erlang:size(Data)};
          eof ->
            ?DEB1(4,"eof reached, returning nodata"),
            #fuse_reply_err{err=enodata};
          _E ->
            ?DEB2(4,"other error, returning ~w",_E),
            #fuse_reply_err{err=eio}
        end;
      #open_duplicate_file{contents=Contents} ->
        Data=take(Contents,Offset,Size),
        #fuse_reply_buf{buf=Data,size=erlang:iolist_size(Data)};

      #open_internal_file{contents=Contents} ->
        Data=take(Contents,Offset,Size),
        #fuse_reply_buf{buf=Data,size=size(Data)};

      _ ->
        #fuse_reply_err{err=eio}
    end,
    {Reply,State}.

take(Contents,Offset,Size) when is_binary(Contents) ->
  ?REPORT(take),
  Len=size(Contents),
  if
    Offset<Len ->
      if
        Offset+Size>Len ->
          Take=Len-Offset;
        true ->
          Take=Size
      end,
      <<_:Offset/binary,Data:Take/binary,_/binary>>=Contents;
    true ->
      Data= <<>>
  end,
  Data;

take(Contents,Offset,Size) when is_list(Contents) ->
  ?REPORT(take),
  Binary=take(list_to_binary(Contents),Offset,Size),
  binary_to_list(Binary).

%%--------------------------------------------------------------------------
%% Read at most Size bytes at offset Offset from the directory identified Inode. Size is real and must be honored: the function fuserlsrv:dirent_size/1 can be used to compute the aligned byte size of a direntry, and the size of the list is the sum of the individual sizes. Offsets, however, are fake, and are for the convenience of the implementation to find a specific point in the directory stream. If noreply is used, eventually fuserlsrv:reply/2  should be called with Cont as first argument and the second argument of type readdir_async_reply ().
%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
readdir(_Ctx,_Inode,Size,Offset,Fuse_File_Info,_Continuation,State) ->
  ?REPORT(readdir),
  ?DEB1(1,">readdir"),
  ?DEB2(2,"|  _Ctx:~w",_Ctx),
  ?DEB2(2,"|  _Inode: ~w ",_Inode),
  ?DEB2(2,"|  Size: ~w ",Size),
  ?DEB2(2,"|  Offset:~w",Offset),
  ?DEB2(2,"|  FI: ~w",Fuse_File_Info),
  #fuse_file_info{fh=FIno}=Fuse_File_Info,
  Reply=
    case attr_open:lookup(FIno) of 
      {value, OpenDir} ->
        ?DEBL(4,"~p =< ~p",[Offset,length(OpenDir)]),
        DirEntryList = 
          case Offset=<length(OpenDir) of
            true ->
              attr_tools:take_while( 
                 fun (Element, { Total, Max }) -> 
                   Cur = fuserlsrv:dirent_size (Element),
                   if 
                     Total + Cur =< Max ->
                       { continue, { Total + Cur, Max } };
                     true ->
                       stop
                   end
                 end,
                   { 2, Size },
                   lists:nthtail(Offset,OpenDir) 
                  );
            false ->
                []
          end,
        ?DEB2(9,"returning ~p",DirEntryList),
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
  ?REPORT(readlink),
  ?DEBL(1,"~s",["readlink!"]),
  {#fuse_reply_err{err=enotsup},State}.

%%--------------------------------------------------------------------------
%% Called when there are no more references to a file. For every open call there is exactly one release call. Fi#fuse_file_info.fh will contain the descriptor set in open, if any. Fi#fuse_file_info.flags will contain the same flags as for open. #fuse_reply_err{err = ok} indicates success. Errors are not reported anywhere; use flush for that. If noreply is used, eventually fuserlsrv:reply/2 should be called with Cont as first argument and the second argument of type release_async_reply ().
%%--------------------------------------------------------------------------
%% Seems like this function does not give me any context information. Maybe I can only use this function as a semafor like server, where I cannot drop a resource until every reference to it has been released?
%%--------------------------------------------------------------------------
release(_Ctx,_Inode,Fuse_File_Info,_Continuation,State) ->
  ?REPORT(release),
  ?DEB1(1,">release"),
  ?DEB2(2,"|  _Ctx:~w",_Ctx),
  ?DEB2(2,"|  _Inode: ~w ",_Inode),
  ?DEB2(2,"|  FI: ~w",Fuse_File_Info),
  #fuse_file_info{fh=FIno}=Fuse_File_Info,
  attr_open:remove(FIno),
  {#fuse_reply_err{err=ok},State}.

%%--------------------------------------------------------------------------
% Called when there are no more references to a directory. For every opendir  call there is exactly one releasedir call. Fi#fuse_file_info.fh will contain the descriptor set in opendir, if any. Fi#fuse_file_info.flags will contain the same flags as for opendir. #fuse_reply_err{err = ok} indicates success. If noreply is used, eventually fuserlsrv:reply/2  should be called with Cont as first argument and the second argument of type releasedir_async_reply ().
%%--------------------------------------------------------------------------
%% TODO: Release dir info from open files in here. Make sure no other process tries to get to the same info etc.
%%--------------------------------------------------------------------------
releasedir(_Ctx,_Inode,Fuse_File_Info,_Continuation,State) ->
  ?REPORT(releasedir),
  ?DEB1(1,">releasedir"),
  ?DEB2(2,"|  _Ctx:~w",_Ctx),
  ?DEB2(2,"|  _Inode: ~w ",_Inode),
  ?DEB2(2,"|  FI: ~w",Fuse_File_Info),
  #fuse_file_info{fh=FIno}=Fuse_File_Info,
  attr_open:remove(FIno),
  {#fuse_reply_err{err=ok},State}.

%%--------------------------------------------------------------------------
%%Remove an extended attribute. #fuse_reply_err{err = ok} indicates success. If noreply is used, eventually fuserlsrv:reply/2  should be called with Cont as first argument and the second argument of type removexattr_async_reply ().
%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
removexattr(_Ctx,Inode,BName,_Continuation,State) ->
  ?REPORT(removexattr),
  Name=attr_tools:remove_from_start(binary_to_list(BName),"user."),
  ?DEB1(1,">removexattr"),
  ?DEB2(2,"|  _Ctx:~w",_Ctx),
  ?DEB2(2,"|  Inode: ~w ",Inode),
  ?DEB2(2,"|  Name: ~w",Name),
  {value,Entry} = tree_srv:lookup(Inode,inodes),
  case Entry#inode_entry.type of
    #external_file{ path=Path } ->
  Syek=string:tokens(Name,?KEY_SEP),
  ?DEB2(4,"tokenized key: ~w",[Syek]),
  Keys=lists:reverse(Syek),
  ?DEBL(4,"key to be deleted: ~w",[Keys]),
      attr_remove:remove_key_values(Path,Inode,Keys),
      ?DEB1(4,"Removed attribute, if any, from database and inode entry"),
      {#fuse_reply_err{err=ok},State};
    _ ->
      ?DEB1(4,"Non-supported inode type"),
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
  ?REPORT(rename),
  Name=binary_to_list(BName),
  NewName=binary_to_list(BNewName),
  ?DEBL(1,">rename; parent: ~w, name: ~s, new parent: ~w, new name: ~s",[ParentIno,Name,NewParentIno,NewName]),
  Reply=attr_rename:rename(ParentIno,NewParentIno,Name,NewName),
  {#fuse_reply_err{err=Reply},State}.


%%--------------------------------------------------------------------------
%% Remove a directory. #fuse_reply_err{err = ok} indicates success. If noreply is used, eventually fuserlsrv:reply/2  should be called with Cont as first argument and the second argument of type rmdir_async_reply ().
%%--------------------------------------------------------------------------
%% For now, only removing of attribute (key and/or value) dirs are supported.
%%--------------------------------------------------------------------------
rmdir(_Ctx,ParentInode,BName,_Continuation,State) ->
  ?REPORT(rmdir),
  ?DEB1(1,">rmdir"),
  ?DEB2(2,"|  Ctx:~w",_Ctx),
  ?DEB2(2,"|  PIno: ~p ",ParentInode),
  ?DEB2(2,"|  BName: ~p",BName),
  {value,PEntry}=tree_srv:lookup(ParentInode,inodes),
  PType=PEntry#inode_entry.type,
  PName=PEntry#inode_entry.name,
  ?DEBL(2,"parent type ~w",[PType]),
  % So I don't have to lookup two entries, I match the parent type instead of the child type.
  Reply=
    case PType of
      attribute_dir ->
        Name=binary_to_list(BName),
        {ok,Ino}=inode:n2i([Name|PName],ino),
        {value,Entry}=tree_srv:lookup(Ino,inodes),
        case Entry#inode_entry.contents of
          [] ->
            attr_remove:remove_child_from_parent(Name,PName),
            tree_srv:delete(Ino,inodes),
            inode:release(Ino,ino),
            ok;
          _Children ->
            enotempty
        end;
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
  ?REPORT(setattr),
  ?DEB1(1,">setattr"),
  ?DEB2(2,"|  Inode: ~w ",Inode),
  ?DEB2(2,"|  To set: ~.8b ",ToSet),
  ?DEB2(2,"|  Attr: ~w ",Attr),
  ?DEB2(2,"|  _FI: ~w",_Fuse_File_Info),
  {value,Entry}=tree_srv:lookup(Inode,inodes),
  Stat=Entry#inode_entry.stat,
  NewStat=Stat#stat{
    st_uid=
      case (?FUSE_SET_ATTR_UID band ToSet) == 0 of
        false ->
          ?DEB1(4,"setting UID"),
          Attr#stat.st_uid;
        true ->
          ?DEB1(4,"not setting UID"),
          Stat#stat.st_uid
      end
    ,st_gid=
      case (?FUSE_SET_ATTR_GID band ToSet) == 0 of
        false ->
          ?DEB1(4,"setting GID"),
          Attr#stat.st_gid;
        true ->
          ?DEB1(4,"not setting GID"),
          Stat#stat.st_gid
      end
    ,st_atime=
      case (?FUSE_SET_ATTR_ATIME band ToSet) == 0 of
        false ->
          ?DEB1(4,"setting atime"),
          Attr#stat.st_atime;
        true ->
          ?DEB1(4,"not setting atime"),
          Stat#stat.st_atime
      end
    ,st_mtime=
      case (?FUSE_SET_ATTR_MTIME band ToSet) == 0 of
        false ->
          ?DEB1(4,"setting mtime"),
          Attr#stat.st_mtime;
        true ->
          ?DEB1(4,"not setting mtime"),
          Stat#stat.st_mtime
      end
    ,st_mode=
      case (?FUSE_SET_ATTR_MODE band ToSet) == 0 of
        false ->
          ?DEB1(4,"setting mode"),
          Attr#stat.st_mode;
        true ->
          ?DEB1(4,"not setting mode"),
          Stat#stat.st_mode
      end
    ,st_size=
      case (?FUSE_SET_ATTR_SIZE band ToSet) == 0 of
        false ->
          ?DEB1(4,"ignoring setting size"),
        %  Attr#stat.st_size;
          Stat#stat.st_size;
        true ->
          ?DEB1(4,"not setting size"),
          Stat#stat.st_size
      end
  },
  tree_srv:enter(Inode,Entry#inode_entry{stat=NewStat},inodes),
  {#fuse_reply_attr{attr=NewStat,attr_timeout_ms=?TIMEOUT_MS},State}.




%%--------------------------------------------------------------------------
%% Set a POSIX file lock. Sleep indicates whether the operation is blocking (true) or nonblocking (false). #fuse_reply_err{err = ok} indicates success. If noreply is used, eventually fuserlsrv:reply/2  should be called with Cont as first argument and the second argument of type setlk_async_reply ().
%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
setlk(_Ctx,_Inode,_Fuse_File_Info,_Lock,_Sleep,_Continuation,State) ->
  ?REPORT(setlk),
  ?DEBL(1,"~s",["setlk!"]),
  {#fuse_reply_err{err=enotsup},State}.

%%--------------------------------------------------------------------------
%% Set file attributes. #fuse_reply_err{err = ok} indicates success. Flags is a bitmask consisting of ?XATTR_XXXXX macros portably defined in fuserl.hrl . If noreply is used, eventually fuserlsrv:reply/2  should be called with Cont as first argument and the second argument of type setxattr_async_reply ().
%%--------------------------------------------------------------------------
%% TODO: if attribute key already has a value for the current file, remove the file from the old value dir!
%%--------------------------------------------------------------------------
setxattr(_Ctx,Inode,BKey,BValue,_Flags,_Continuation,State) ->
  ?REPORT(setxattr),
  ?DEB1(1,">setxattr"),
  ?DEB2(2,"|  _Ctx:~w",_Ctx),
  ?DEB2(2,"|  Inode: ~w ",Inode),
  RawKey=binary_to_list(BKey),
  RawValue=binary_to_list(BValue),
  ?DEB2(2,"|  Key: ~p ",RawKey),
  ?DEB2(2,"|  Value: ~p ",RawValue),
  ?DEB2(2,"|  _Flags: ~w ",_Flags),
  ?DEB1(4,"getting inode entry"),
  {value,Entry} = tree_srv:lookup(Inode,inodes),
  ?DEB1(4,"transforming input data"),
  Key=
    case string:str(RawKey,"system")==1 of
      true -> "."++RawKey;
      false -> attr_tools:remove_from_start(RawKey,"user.")
    end,

  Values=string:tokens(RawValue,?VAL_SEP),
  ?DEB2(4,"got values: ~w",Values),

  Reply=
    case Entry#inode_entry.type of
      #external_file{path=Path} ->
        ?DEB1(4,"entry is an external file"),
        Syek=string:tokens(Key,?KEY_SEP),
        ?DEB2(4,"tokenized key: ~w",[Syek]),
        Keys=lists:reverse(Syek),
        ?DEBL(4,"key to be inserted: ~w",[Keys]),
        case Values of
          [] -> attr_ext:add_new_attribute(Path,Inode,Entry,Keys);
          _Values ->
            lists:foreach(
              fun(Value) -> 
                Attr=[Value|Keys],
                ?DEBL(4,"adding attribute {~p} for file ~p to database",[Attr,Path]),
                attr_ext:add_new_attribute(Path,Inode,Entry,Attr)
              end,
              Values)
        end,
        #fuse_reply_err{err=ok};
      _ ->
        ?DEB1(4,"entry not an external file, skipping..."),
        #fuse_reply_err{err=enotsup}
    end,
  {Reply,State}.


%%--------------------------------------------------------------------------
%% Get file system statistics. If noreply is used, eventually fuserlsrv:reply/2  should be called with Cont as first argument and the second argument of type statfs_async_reply ().
%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
statfs(_Ctx,_Inode,_Continuation,State) ->
  ?REPORT(statfs),
  ?DEB1(1,">statfs"),
  ?DEB2(2,"|  _Ctx:~w",_Ctx),
  ?DEB2(2,"|  _Inode: ~w ",_Inode),
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



%%--------------------------------------------------------------------------
%% Create a symbolic link. Link is the contents of the link. Name is the name to create. If noreply is used, eventually fuserlsrv:reply/2  should be called with Cont as first argument and the second argument of type symlink_async_reply ().
%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
symlink(_Ctx,_Link,_Inode,_Name,_Continuation,State) ->
  ?REPORT(symlink),
  ?DEB1(1,">symlink"),
  {#fuse_reply_err{err=enotsup},State}.

%%--------------------------------------------------------------------------
%% Remove a file. #fuse_reply_err{err = ok} indicates success. If noreply is used, eventually fuserlsrv:reply/2  should be called with Cont as first argument and the second argument of type unlink_async_reply ().
%%--------------------------------------------------------------------------
%% We need to check the dir type of the parent to know what to do when
%% unlinking a file.
%% For now, let's only support unlinking files residing in attribute dirs.
%%--------------------------------------------------------------------------
unlink(_Ctx,ParentInode,BName,_Cont,State) ->
  ?REPORT(unlink),
  ?DEB1(1,">unlink"),
  ?DEB2(2,"|  _Ctx:~w",_Ctx),
  ?DEB2(2,"|  PIno: ~w ",ParentInode),
  ?DEB2(2,"|  BName: ~p",BName),
  {value,ParentEntry}=tree_srv:lookup(ParentInode,inodes),
  ParentType=ParentEntry#inode_entry.type,
  ParentName=ParentEntry#inode_entry.name,
  ?DEBL(4,"parent type ~s",[io_lib:write(ParentType)]),
    Reply=
      case ParentType of
        attribute_dir ->
          Name=binary_to_list(BName),
          ParentChildren=ParentEntry#inode_entry.contents,
          case lists:keyfind(Name,1,ParentChildren) of
            false -> 
              ?DEBL(4,"~p not found in ~p",[Name,ParentChildren]),
              #fuse_reply_err{err=enoent};
            {Name,Inode,Type} ->
              ?DEB1(4,"found"),
              %% Removing a file from an attribute folder is 
              %% ONLY ALMOST the same as removing the corresponding 
              %% attribute from the file; Removing a value subfolder
              %% is NOT the same as removing a whole attribute.
              %% Thus, I'm NOT calling removexattr here.
              ?DEBL(4,"child type ~s",[io_lib:write(Type)]),
              PName=ParentEntry#inode_entry.name,
              case Type of 
                #external_file{path=Path} ->
                  ?DEBL(4,"Removing ~s from ~s", [ParentEntry#inode_entry.name,Name]),
                  attr_remove:remove_attribute(Path,Inode,ParentName),
                  #fuse_reply_err{err=ok};

                internal_file ->
                  ?DEBL(4,"Removing external file ~s from ~s",[ParentEntry#inode_entry.name,Name]),
                  attr_remove:remove_child_from_parent(Name,PName),
                  tree_srv:delete_any(Inode,inodes),
                  inode:release(Inode,ino),
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
%% I will only support writing to files I have created, and "/dev/null"-writing when copying, if it is possible.
%% That is, writing to an internal_file is ok, and writing to an #external_file{} in an attribute folder iff I still have the same IP or file handle as the file creator had/got.
%% Maybe I'll also support writing to real files later on.
%% For now, let's just ignore all data written to external files, and the problem will be temporarily mitigated.
%%--------------------------------------------------------------------------
write(_Ctx,Inode,Data,Offset,Fuse_File_Info,_Continuation,State) ->
  ?REPORT(write),
  ?DEB1(1,">write"),
  ?DEB2(2,"|  Inode: ~b",Inode),
  ?DEB2(2,"|  Offset: ~b",Offset),
  ?DEB2(2,"|  Data: ~p",Data),
  ?DEB2(2,"|  FI: ~w",Fuse_File_Info),
  case tree_srv:lookup(Inode,inodes) of
    {value,Entry} ->
      case Entry#inode_entry.type of
        #external_file{} -> 
			% For now, let's just pretend that we write something. 
			% Otherwise we'll have to know the parent of the node.
			% (Could be sent via the file info, if needed.)
          {#fuse_reply_write{count=size(Data)},State};
        internal_file ->
          Contents=Entry#inode_entry.contents,
          Stat=Entry#inode_entry.stat,
          Len=size(Contents),
          DLen=size(Data),
          try
            NewContents=write_contents(Offset,Contents,Data,Len,DLen),
            ?DEB1(1,"Got new contents"),
            NewStat=?ST_SIZE(Stat,size(NewContents)),
            NewEntry=Entry#inode_entry{contents=NewContents,stat=NewStat},
            tree_srv:enter(Inode,NewEntry,inodes),
            {#fuse_reply_write{count=size(Data)},State}
          catch
            E -> {#fuse_reply_err{err=E},State}
          end;
        _ ->
          {#fuse_reply_err{err=enotsup},State}
      end;
    none -> 
      {#fuse_reply_err{err=enoent},State}
  end.

% No offset, data to be written completely overwrites contents of file. 
write_contents(0,_Contents,Data,CLen,DLen) when DLen >= CLen ->
  ?DEB1(1,"1"),
  Data;

% No offset, partial overwrite.
write_contents(0,Contents,Data,_CLen,DLen) ->
  ?REPORT(write_contents),
  ?DEB1(1,"2"),
  <<_:DLen/binary,Rest/binary>>=Contents,
  <<Data/binary,Rest/binary>>;

% Offset after EOF, throw eof
write_contents(Offset,_Contents,_Data,CLen,_DLen) when Offset > CLen ->
  ?DEB1(1,"3"),
  throw(eof);

% Offset at end of file, append data
write_contents(Offset,Contents,Data,CLen,_DLen) when Offset == CLen ->
  ?DEB1(1,"4"),
  <<Contents/binary,Data/binary>>;

% Offset inside the file, insert and overwrite
write_contents(Offset,Contents,Data,CLen,DLen) when Offset < CLen ->
  ?DEB1(1,"5"),
  if
    Offset+DLen >= CLen -> 
      <<Before:Offset/binary,_>>=Contents,
      <<Before/binary,Data/binary>>;
    Offset+DLen < CLen ->
      <<Before:Offset/binary,After/binary>>=Contents,
      <<Before/binary,Data/binary,After/binary>>
  end.

  

%%%=========================================================================
%%%                         DEBUG FUNCTIONS
%%%=========================================================================

%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
dump_inode_entries() ->
  ?REPORT(dump_inode_entries),
  lists:map(fun({Inode,#inode_entry{name=Name,contents=Children}}) -> {Inode,Name,Children} end, tree_srv:to_list(inodes)).

%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
dump_entries(Table) ->
  ?REPORT(dump_entries),
  tree_srv:to_list(Table).

%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
dump_inodes() ->
  ?REPORT(dump_inodes),
  inode:list_bound(ino).

%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
dump_finodes() ->
  ?REPORT(dump_finodes),
  inode:list_bound(fino).

which_dirs() ->
  lists:map(fun(A)->attr_tools:get_or_default(A,"")end,[from_dir,from_dirs,to_dir]).
