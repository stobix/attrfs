-module(attrfs_srv).

%%%=========================================================================
%%%=========================================================================
%%% @author Joel Ericson <kasettbok@gmail.com>
%%%
%%% @copyright Copylefted using some GNU license or other.
%%%
%%% @version Almost working, yo...
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
%%% taken from the fuserl API, the second any comments I make about the
%%% function.
%%%=========================================================================
%%%=========================================================================



%%%=========================================================================
%%%                                 EXPORTS
%%%=========================================================================

%%%=========================================================================
%%% server function exports
%%%=========================================================================
-export([handle_info/2,init/1]).
-export([code_change/3]). % TODO: Do something intelligent with this one. Returns "ok" now, totally ignoring its indata.
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
         setlk/7,
         setxattr/7,
         statfs/4,
         symlink/6,
         terminate/2,
         unlink/5,
         write/7]).


%%%=========================================================================
%%% debug function exports
%%%=========================================================================

-export([keymergeunique/2]).
-export([stringdelta/2]).
-export([dump_entries/1,dump_inodes/0,dump_inode_entries/0]).


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
code_change(_,_,_) -> %XXX: Maybe do something more intelligent with this?
    ?DEBL("~s",["code_change!"]),
    ok.

%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
%% Mirrors MirrorDir into MountDir/real, building the attribute file system in attribs from the attributes for the files in MountDir
%%--------------------------------------------------------------------------
start_link({MountDir,MirrorDir,DB}) ->
    ?DEB1("Starting attrfs server..."),
    start_link(MountDir,true,"",MirrorDir,DB).

%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
%% Things are initiated here instead of init so that no calls to the file system will be made before the file system is constructed.
%% Maybe this is an unneccessary precaution?
%%--------------------------------------------------------------------------
start_link(Dir,LinkedIn,MountOpts,MirrorDir,DB) ->
    Options=[],
    ?DEBL("   opening attribute database file ~p as ~p", [DB, ?ATTR_DB]),
    {ok,_}=dets:open_file(?ATTR_DB,[{type,bag},{file,DB}]),
    ?DEB2("   mirroring dir ~p",MirrorDir),
    tree_srv:new(inodes),
    tree_srv:new(keys),
    tree_srv:new(open_files),
    ?DEB1("   created inode and key trees"),
    inode:reset(),
    RootIno=inode:get(root),
    RealIno=inode:get("real"),
    AttribIno=inode:get("attribs"),
    ?DEBL("   inodes;\troot:~p, real:~p, attribs:~p",[RootIno,RealIno,AttribIno]),
    ?DEB1("   creating root entry"),
    RootEntry=#inode_entry{
        name=root
       ,children=[{"real",RealIno},{"attribs",AttribIno}]
       ,type=internal_dir %XXX: Really ok to let this have the same type as attribute dirs?
       ,stat=#stat{
           st_mode=8#755 bor ?S_IFDIR
          ,st_ino=inode:get(root)
        }
       ,ext_info=[]
       ,ext_io=ext_info_to_ext_io([])
    },
    ?DEB1("   updating root inode entry"),
    tree_srv:enter(RootIno,RootEntry,inodes),
    ?DEB2("   making inode entries for ~p",MirrorDir),
    AttributeEntry=#inode_entry{
        name="attribs",
       children=tree_srv:to_list(keys),
       type=internal_dir,
       stat=#stat{ 
               % For now I'll set all access here, and limit access on a per-user-basis.
               % Maybe even make this folder "magic", so that different users think that they own it?
               % More on this when I start using the Ctx structure everywhere.
            st_mode=8#777 bor ?S_IFDIR,
            st_ino=AttribIno
        },
       ext_info=[],
       ext_io=ext_info_to_ext_io([])
    },
    tree_srv:enter(AttribIno,AttributeEntry,inodes),
    % This mirrors all files and folders, recursively, from the external folder MirrorDir to the internal folder "real", adding attribute folders with appropriate files when a match between external file and internal database entry is found.
    make_inode_list({MirrorDir,"real"}),
    ?DEB1("   attribute inode list made"),
    % Since InodeList1 is created first, and both InodeList1 and 
    % AttributeBranchList are sorted, this will produce an ordered list.
    State=#state{open_files=gb_trees:empty()},
    assert_started(fuserl),
    ?DEB1("   fuserl started, starting fuserlsrv"),
    fuserlsrv:start_link(?MODULE,LinkedIn,MountOpts,Dir,State,Options).

%%--------------------------------------------------------------------------
%% The analog to Module:init/1 in gen_server.
%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
init(State=#state{}) ->
    {ok, State}.

%%--------------------------------------------------------------------------
%% The analog to Module:terminate/2 in gen_server.
%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
terminate(_Reason,_State) ->
    ?DEBL(">terminate ~p",[_Reason]),
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
    ?DEBL(">access inode: ~p, mask: ~p, context: ~p",[Inode,Mask,Ctx]),
    Reply=test_access(Inode,Mask,Ctx),
    {#fuse_reply_err{err=Reply},State}.

%%--------------------------------------------------------------------------
%% Create and open a file. Mode is a bitmask consisting of ?S_XXXXX macros portably defined in fuserl.hrl . If noreply is used, eventually fuserlsrv:reply/2  should be called with Cont as first argument and the second argument of type create_async_reply (). 
%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
create(_Ctx,_Parent,_Name,_Mode,_Fuse_File_Info,_Continuation, State) ->
    ?DEBL("~s",["create!"]),
    {#fuse_reply_err{err=enotsup},State}.

%%--------------------------------------------------------------------------
%% This is called on each close () of an opened file, possibly multiple times per open  call (due to dup () et. al.). Fi#fuse_file_info.fh will contain the descriptor set in open, if any. #fuse_reply_err{err = ok} indicates success. This return value is ultimately the return value of close () (unlike release). Does *not* necessarily imply an fsync. If noreply is used, eventually fuserlsrv:reply/2  should be called with Cont as first argument and the second argument of type flush_async_reply ().
%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
flush(_Ctx,_Inode,_Fuse_File_Info,_Continuation,State) ->
    ?DEBL("~s",["flush!"]),
    {#fuse_reply_err{err=enotsup},State}.

%%--------------------------------------------------------------------------
%% Forget about an inode. If noreply is used, eventually fuserlsrv:reply/2  should be called with Cont as first argument and the second argument of type forget_async_reply ().
%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
forget(_Ctx,_Inode,_Nlookup,_Continuation,State) ->
    ?DEBL(">forget inode: ~p ctx: ~p ",[_Inode,_Ctx]),
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
    Name=stringdelta(binary_to_list(BName),"user."),
    ?DEBL(">getxattr, name:~p, size:~p, inode:~p",[Name,Size,Inode]),
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
                        true -> ?DEBL("   They are using a too small buffer; ~p < ~p ",[Size,ExtSize]),#fuse_reply_err{err=erange};
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
    Reply=case Size == 0 of 
        true -> ?DEB1("   they want to know our size."),#fuse_reply_xattr{count=ExtSize};
        false -> 
            case Size < ExtSize of
                true -> ?DEBL("   they are using a too small buffer; ~p < ~p ",[Size,ExtSize]),#fuse_reply_err{err=erange};
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
    Reply=case lookup_children(ParentInode) of
        {value,Children} ->
            ?DEBL("   Got children for ~p: ~p",[ParentInode, Children]),
            case lists:keysearch(Child,1,Children) of
                {value,{_,Inode}} ->
                    ?DEB2("   Found child ~p",Child),
                    {value,Entry} = tree_srv:lookup(Inode,inodes),
                    ?DEB1("   Got child inode entry"),
                    Stat=Entry#inode_entry.stat,
                    ?DEB1("   Got child inode entry stat file info, returning"),
                    Param=#fuse_entry_param{
                        ino=Inode,
                        generation=1, %for now.
                        attr=Stat,
                        attr_timeout_ms=1000,
                        entry_timeout_ms=1000
                            },
                    #fuse_reply_entry{fuse_entry_param=Param};
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
%% * attr_dir where name is a string
%%   this will create an attribute value with name {attr_dir name, value name}
%% * attr_dir where name is a {string,string}
%%   creating a dir inside a value dir will not be supported as long as I don't 
%%    support deep attributes.
%% * internal_dir
%%   this means we try to create a directory inside either /, /real or /attribs, 
%%    that is, either we try to create a new directory universe (not allowed),
%%    a new real dir (maybe supported later) 
%%    or a new attribute dir (certainly allowed)
%%--------------------------------------------------------------------------

mkdir(Ctx,ParentInode,BName,MMode,_Continuation,State) ->
    Name=binary_to_list(BName),
    ?DEBL(">mkdir ctx: ~p pIno: ~p name: ~p mode: ~p ",[Ctx,ParentInode,Name,MMode]),
    Mode=MMode bor ?S_IFDIR,
    Reply=case tree_srv:lookup(ParentInode,inodes) of
        none -> #fuse_reply_err{err=enoent}; 
        {value,Parent} ->
            case tree_srv:lookup(inode:is_numbered(Name),inodes) of
                none ->
                    ParentName=Parent#inode_entry.name,
                    ParentType=Parent#inode_entry.type,
                    make_dir(Ctx,ParentInode,ParentName,ParentType,Name,Mode);
                _ -> #fuse_reply_err{err=eexist}
            end
    end,
    {Reply,State}.



%%--------------------------------------------------------------------------
%% Create a file node. Mode is a mask composed of the ?S_XXXXX macros which are (portably) defined in fuserl.hrl. Dev is only valid if the created file is a device. If noreply is used, eventually fuserlsrv:reply/2  should be called with Cont as first argument and the second argument of type mknod_async_reply ().
%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------

mknod(_Ctx,_ParentInode,_Name,_Mode,_Dev,_Continuation,State) ->
    ?DEBL("~s",["mknod!"]),
    {#fuse_reply_err{err=enotsup},State}.

%%--------------------------------------------------------------------------
%% Open an inode. If noreply is used, eventually fuserlsrv:reply/2  should be called with Cont as first argument and the second argument of type open_async_reply ().
%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
open(_Ctx,_Inode,_Fuse_File_Info,_Continuation,State) ->
    ?DEBL("~s",["open!"]),
    {#fuse_reply_err{err=enotsup},State}.

%%--------------------------------------------------------------------------
%% Open an directory inode. If noreply is used, eventually fuserlsrv:reply/2  should be called with Cont as first argument and the second argument of type opendir_async_reply ().
%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
opendir(Ctx,Inode,_FI=#fuse_file_info{flags=_Flags,writepage=_Writepage,direct_io=_DirectIO,keep_cache=_KeepCache,flush=_Flush,fh=_Fh,lock_owner=_LockOwner},_Continuation,_State) ->
    ?DEBL(">opendir token:~p, file info:~p ",[{Ctx,Inode},_FI]),
    ?DEB2("   flags ~p",_Flags),
%    ?DEB2("   writepage ~p",_Writepage),
%    ?DEB2("   DirectIO ~p",_DirectIO),
%    ?DEB2("   KeepCache ~p",_KeepCache),
%    ?DEB2("   FileHandle ~p",_Fh),
    ?DEB2("  Getting inode entries for ~p",Inode),
    ?DEB1("  Creating directory entries from inode entries"),
    % TODO: What to do if I get several opendir calls (from the same context?) while the dir is updating?
    set_open_file({Ctx,Inode},direntries(Inode)),
    {#fuse_reply_open{ fuse_file_info = _FI }, _State}.

%%--------------------------------------------------------------------------
%% Read Size bytes starting at offset Offset. The file descriptor and other flags are available in Fi. If noreply is used, eventually fuserlsrv:reply/2 should be called with Cont as first argument and the second argument of type read_async_reply ().
%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
read(_Ctx,_Inode,_Size,_Offset,_Fuse_File_Info,_Continuation,State) ->
    ?DEBL(">read",[]),
    {#fuse_reply_err{err=enotsup},State}.

%%--------------------------------------------------------------------------
%% Read at most Size bytes at offset Offset from the directory identified Inode. Size is real and must be honored: the function fuserlsrv:dirent_size/1 can be used to compute the aligned byte size of a direntry, and the size of the list is the sum of the individual sizes. Offsets, however, are fake, and are for the convenience of the implementation to find a specific point in the directory stream. If noreply is used, eventually fuserlsrv:reply/2  should be called with Cont as first argument and the second argument of type readdir_async_reply ().
%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
readdir(Ctx,Inode,Size,Offset,_Fuse_File_Info,_Continuation,State) ->
  ?DEBL(">readdir token:~p offset:~p file info: ~p",[{Ctx,Inode},Offset,_Fuse_File_Info]),
  Reply=case lookup_open_file({Ctx,Inode}) of 
    {value, OpenFile} ->
        DirEntryList = 
        case Offset<length(OpenFile) of
            true ->
                take_while 
                  (fun (E, { Total, Max }) -> 
                     Cur = fuserlsrv:dirent_size (E),
                     if 
                       Total + Cur =< Max ->
                         { continue, { Total + Cur, Max } };
                       true ->
                         stop
                     end
                   end,
                   { 0, Size },
                   lists:nthtail(Offset,OpenFile) 
                  );
            false ->
                []
        end,
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
release(_Ctx,_Inode,_Fuse_File_Info,_Continuation,State) ->
    ?DEBL("~s",["release!"]),
    {#fuse_reply_err{err=enotsup},State}.

%%--------------------------------------------------------------------------
% Called when there are no more references to a directory. For every opendir  call there is exactly one releasedir call. Fi#fuse_file_info.fh will contain the descriptor set in opendir, if any. Fi#fuse_file_info.flags will contain the same flags as for opendir. #fuse_reply_err{err = ok} indicates success. If noreply is used, eventually fuserlsrv:reply/2  should be called with Cont as first argument and the second argument of type releasedir_async_reply ().
%%--------------------------------------------------------------------------
%% TODO: Release dir info from open files in here. Make sure no other process tries to get to the same info etc.
%%--------------------------------------------------------------------------
releasedir(Ctx,Inode,_Fuse_File_Info,_Continuation,State) ->
    ?DEBL(">releasedir token:~p file info:~p",[{Ctx,Inode},_Fuse_File_Info]),
    remove_open_file({Ctx,Inode}),
    {#fuse_reply_err{err=ok},State}.

%%--------------------------------------------------------------------------
%%Remove an extended attribute. #fuse_reply_err{err = ok} indicates success. If noreply is used, eventually fuserlsrv:reply/2  should be called with Cont as first argument and the second argument of type removexattr_async_reply ().
%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
removexattr(_Ctx,Inode,BName,_Continuation,State) ->
    Name=stringdelta(binary_to_list(BName),"user."),
    ?DEBL(">removexattr inode: ~p name: ~p",[Inode,Name]),
    {value,Entry} = tree_srv:lookup(Inode,inodes),
    case Entry#inode_entry.type of
        #external_file{ path=Path } ->
            remove_old_attribute_key(Path,Inode,Name),
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
    ?DEBL(">rename; parent: ~p, name: ~p, new parent: ~p",[ParentIno,Name,NewParentIno]),
    {value,ParentInoEntry}=tree_srv:lookup(ParentIno,inodes),
    ?DEBL("   parent_type: ~p",[ParentInoEntry#inode_entry.type]),
    Reply=case tree_srv:lookup(NewParentIno,inodes) of
        none -> 
            ?DEB1("   new parent nonexistent!"),
            enoent;
        {value,NewAttribEntry} ->
            ?DEBL("   new parent type: ~p",[NewAttribEntry#inode_entry.type]),
            make_rename_reply(
                ParentInoEntry#inode_entry.type,
                ParentInoEntry#inode_entry.name,
                ParentInoEntry,NewParentIno,Name,NewName)
    end,
    {#fuse_reply_err{err=Reply},State}.


%%--------------------------------------------------------------------------
%% Remove a directory. #fuse_reply_err{err = ok} indicates success. If noreply is used, eventually fuserlsrv:reply/2  should be called with Cont as first argument and the second argument of type rmdir_async_reply ().
%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
rmdir(_Ctx,ParentInode,BName,_Continuation,State) ->
    ?DEBL("rmdir, ctx: ~p, inode: ~p, name: ~p",[_Ctx,ParentInode,BName]),
    {value,PEntry}=tree_srv:lookup(ParentInode,inodes),
    PType=PEntry#inode_entry.type,
    PName=PEntry#inode_entry.name,
    ?DEBL("parent type ~p",[PType]),
    Reply=case PType of
        #attribute_dir{} ->
            Name=binary_to_list(BName),
            remove_child_from_parent(Name,PName),
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
setattr(_Ctx,Inode,_Attr,_ToSet,_Fuse_File_Info,_Continuation,State) ->
    ?DEBL(">setattr inode: ~p~n    attr: ~p~n    to_set: ~p~n    fuse_file_info: ~p",[Inode,_Attr,_ToSet,_Fuse_File_Info]),
    {value,Entry}=tree_srv:lookup(Inode,inodes),
    Stat=Entry#inode_entry.stat,
    NewStat=Stat#stat{
        st_uid=
            case (?FUSE_SET_ATTR_UID band _ToSet) == 0 of
                false ->
                    ?DEB1("    setting UID"),
                    _Attr#stat.st_uid;
                true ->
                    ?DEB1("    not setting atime"),
                    Stat#stat.st_uid
            end
        ,st_gid=
            case (?FUSE_SET_ATTR_GID band _ToSet) == 0 of
                false ->
                    ?DEB1("    setting GID"),
                    _Attr#stat.st_gid;
                true ->
                    ?DEB1("    not setting atime"),
                    Stat#stat.st_gid
            end
        ,st_atime=
            case (?FUSE_SET_ATTR_ATIME band _ToSet) == 0 of
                false ->
                    ?DEB1("    setting atime"),
                    _Attr#stat.st_atime;
                true ->
                    ?DEB1("    not setting atime"),
                    Stat#stat.st_atime
            end
        ,st_mtime=
            case (?FUSE_SET_ATTR_MTIME band _ToSet) == 0 of
                false ->
                    ?DEB1("    setting mtime"),
                _Attr#stat.st_mtime;
            true ->
                ?DEB1("    not setting mtime"),
                Stat#stat.st_mtime
            end
        ,st_mode=
            case (?FUSE_SET_ATTR_MODE band _ToSet) == 0 of
                false ->
                    ?DEB1("    setting mode"),
                _Attr#stat.st_mode;
            true ->
                ?DEB1("    not setting mode"),
                Stat#stat.st_mode
            end
        ,st_size=
            case (?FUSE_SET_ATTR_SIZE band _ToSet) == 0 of
                false ->
                    ?DEB1("    setting size"),
                _Attr#stat.st_size;
            true ->
                ?DEB1("    not setting size"),
                Stat#stat.st_size
            end
                },
    tree_srv:enter(Inode,Entry#inode_entry{stat=NewStat}),
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
    ?DEBL("setxattr inode:~p key:~p value:~p flags: ~p",[Inode,BKey,BValue,_Flags]),
    ?DEB1("   getting inode entry"),
    {value,Entry} = tree_srv:lookup(Inode,inodes),
    ?DEB1("   transforming input data"),
    Key=binary_to_list(BKey),
    Value=binary_to_list(BValue),
    Reply=case Entry#inode_entry.type of
        #external_file{path=Path} ->
            ?DEBL("   adding attribute {~p,~p} for file ~p to database",[Key,Value,Path]),
            add_new_attribute(Path,Inode,Entry,{stringdelta(Key,"user."),Value}),
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
    ?DEBL(">statfs Ctx:~p Ino:~p",[_Ctx,_Inode]),
    {#fuse_reply_statfs{
        statvfs=#statvfs{
            f_bsize=1024, % should be the same as the file system we're mirroring.
            f_frsize=2048, % What does this do?
            f_blocks=1234567890, % size = f_blocks*f_frsize
            f_bfree=314159265,
            f_bavail=271828182,
            f_files=1000000000000000, % at least!
            f_ffree=1000000000000000-inode:count_occupied(), % at least!
            f_favail=1000000000000000-inode:count_occupied(), % at least!
            f_fsid=0, % how to get this right?
            f_flag=0, % Hm...
            f_namemax=10000 % Or some other arbitary high value.
        }},State}.


%    {#fuse_reply_err{err=enotsup},State}.

%%--------------------------------------------------------------------------
%% Create a symbolic link. Link is the contents of the link. Name is the name to create. If noreply is used, eventually fuserlsrv:reply/2  should be called with Cont as first argument and the second argument of type symlink_async_reply ().
%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
symlink(_Ctx,_Link,_Inode,_Name,_Continuation,State) ->
    ?DEBL("~s",["symlink!"]),
    {#fuse_reply_err{err=enotsup},State}.

%%--------------------------------------------------------------------------
%% Remove a file. #fuse_reply_err{err = ok} indicates success. If noreply is used, eventually fuserlsrv:reply/2  should be called with Cont as first argument and the second argument of type unlink_async_reply ().
%%--------------------------------------------------------------------------
%% We need to check the dir type of the parent to know what to do when
%% unlinking a file.
%% For now, let's only support unlinking files residing in attribute dirs.
%%--------------------------------------------------------------------------
unlink(_Ctx,ParentInode,BName,_Cont,State) ->
    ?DEBL("unlink ctx: ~p, parent inode:~p, name:~p",[_Ctx,ParentInode,BName]),
    {value,PEntry}=tree_srv:lookup(ParentInode,inodes),
    PType=PEntry#inode_entry.type,
    PName=PEntry#inode_entry.name,
    ?DEBL("parent type ~p",[PType]),
    Reply=case PType of
        #attribute_dir{atype=value} ->
            Name=binary_to_list(BName),
            PChildren=PEntry#inode_entry.children,
            case lists:keyfind(Name,1,PChildren) of
                false -> 
                    ?DEBL("~p not found in ~p",[Name,PChildren]),
                    #fuse_reply_err{err=enoent};
                {Name,Inode} ->
                    ?DEB1("~p found"),
                    %% Removing a file from an attribute folder is 
                    %% ONLY ALMOST the same as removing the corresponding 
                    %% attribute from the file; Removing a value subfolder
                    %% is NOT the same as removing a whole attribute.
                    %% Thus, I'm NOT calling removexattr here.
                    {value,Entry}=tree_srv:lookup(Inode,inodes),
                    Type=Entry#inode_entry.type,
                    ?DEBL("child type ~p",[Type]),
                    case Type of 
                        #external_file{path=Path} ->
                            ?DEBL("Removing ~p from ~p", [PEntry#inode_entry.name,Name]),
                            remove_old_attribute_value(Path,Inode,PName),
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
    ?DEBL("~s",["write!"]),
    {#fuse_reply_err{err=enotsup},State}.

%%%=========================================================================
%%%                        NON-API FUNCTIONS 
%%%=========================================================================

%%%=========================================================================
%%% Callback function internals
%%%=========================================================================


%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
%% Getattr internal functions
%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------

%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
getattr_internal(Inode,Continuation) ->
    ?DEB2("  >getattr_internal inode:~p",Inode),
    case tree_srv:lookup(Inode,inodes) of
        none ->
            ?DEB1("   Non-existent file"),
            Reply=#fuse_reply_err{err=enoent};
        {value,Entry} ->
            ?DEB1("   File exists, returning info"),
                Reply=#fuse_reply_attr{
                    attr=Entry#inode_entry.stat,
                    attr_timeout_ms=5
                };
            _A -> 
                ?DEB2("   This should not be happening: ~p",_A),
                Reply=#fuse_reply_err{err=enotsup}
    end,
    ?DEB1("   Sending reply"),
    fuserlsrv:reply(Continuation,Reply).

%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
%% Mkdir internal functions
%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
make_dir(_Ctx,_ParentInode,_ParentName,#external_dir{},_Name,_Mode) ->
    ?DEB1("   external dir, not supported"),
    #fuse_reply_err{err=enotsup};


make_dir(Ctx,ParentInode,ParentName,DirType=#attribute_dir{},Name,Mode) ->
    AttrDirType=DirType#attribute_dir.atype,
    make_attr_child_dir(Ctx,ParentInode,AttrDirType,ParentName,Name,Mode);


make_dir(Ctx,ParentInode,ParentName,internal_dir,Name,Mode) ->
    make_internal_child_dir(Ctx,ParentInode,ParentName,Name,Mode).

%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
make_attr_child_dir(_Ctx,_ParentInode,value,_ValueParentName,_Name,_Mode) ->
    ?DEB1("   NOT creating a subvalue dir"),
    %this is where I create a subvalue dir, when these are supported.
    % subvalue dir name {{Key,Val},Name}, I guess.
    % Inode=make_general_dir(Ctx,ParentInode,Name,Mode),
    #fuse_reply_err{err=enotsup};

make_attr_child_dir(Ctx,ParentInode,key,Key,Name,Mode) ->
    ?DEB1("   creating an attribute value dir"),
    % create value directory here.
    MyInode=inode:get({Key,Name}),
    Stat=make_general_dir(Ctx,ParentInode,MyInode,{Key,Name},Mode,#attribute_dir{atype=value}),
    ?DEB1("   returning new dir"),
    #fuse_reply_entry{
        fuse_entry_param=
            #fuse_entry_param{
                ino=MyInode,
                generation=1,
                attr=Stat,
                attr_timeout_ms=1000,
                entry_timeout_ms=1000
            }
    }.

make_internal_child_dir(_Ctx,_ParentInode,root,_Name,_Mode) ->
    ?DEB1("   creating of new directory type not supported"),
    #fuse_reply_err{err=enotsup};

make_internal_child_dir(_Ctx,_ParentInode,"real",_Name,_Mode) ->
    ?DEB1("   creating of external dirs not supported"),
    #fuse_reply_err{err=enotsup};

make_internal_child_dir(Ctx,ParentInode,"attribs",Name,Mode) ->
    % This is where I add an attribute key folder.
    ?DEB1("   creating an attribute key dir"),
    MyInode=inode:get(Name),
    Stat=make_general_dir(Ctx,ParentInode,MyInode,Name,Mode,#attribute_dir{atype=key}),
    ?DEB1("   returning new dir"),
    tree_srv:enter(Name,MyInode,keys),
    #fuse_reply_entry{
        fuse_entry_param=
            #fuse_entry_param{
                ino=MyInode,
                generation=0,
                attr=Stat,
                attr_timeout_ms=1000,
                entry_timeout_ms=1000
            }
    };

make_internal_child_dir(_Ctx,_ParentInode,_,_Name,_Mode) ->
            #fuse_reply_err{err=enotsup}.

%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
make_general_dir(Ctx,ParentInode,MyInode,Name,Mode,DirType) ->
    ?DEBL("   creating new directory entry called ~p",[Name]),
    {MegaNow,NormalNow,_} = now(),
    Now=MegaNow*1000000+NormalNow,
    ?DEBL("   atime etc: ~p",[Now]),
    #fuse_ctx{uid=Uid,gid=Gid}=Ctx,
    DirStat=#stat{
        st_mode=Mode,
        st_ino=MyInode,
%        st_nlink=1, % FIXME: Make this accurate.
        st_uid=Uid,
        st_gid=Gid,
        st_atime=Now,
        st_ctime=Now,
        st_mtime=Now
        },
   DirEntry=#inode_entry{
       name=Name,
       stat=DirStat,
       ext_info=[],
       ext_io=ext_info_to_ext_io([]),
       type=DirType,
       children=[]
       },
   insert_entry(ParentInode,DirEntry),
   DirStat.

 

%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
%% rename internal functions
%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------

%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
make_rename_reply(#external_dir{},_OldAttribName,_OldAttribEntry,NewAttribIno,File,_NewValueName) ->
    ?DEB1("   old parent is an external dir"),
    {value,NewAttribEntry}=tree_srv:lookup(NewAttribIno,inodes),
    ?DEB1("   got new attribute folder entry"),
    case NewAttribEntry#inode_entry.type of
        #attribute_dir{atype=value} ->
            ?DEB1("   new parent is a value dir"),
            FileIno=inode:is_numbered(File),
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
            {OldValueName,FileIno}=lists:keyfind(OldValueName,1,NewAttribEntry#inode_entry.children),
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
%                "attribs" ->
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

make_rename_reply(internal_dir,"attribs",_OldAttribEntry,NewAttribIno,File,NewValueName) ->
    ?DEB1("   old parent is an internal dir"),
    {value,NewAttribEntry}=tree_srv:lookup(NewAttribIno,inodes),
    case NewAttribEntry#inode_entry.type of
        %% These will be supported when I make deep attributes.
%        #attribute_dir{type=value}} ->
%        #attribute_dir{type=key} ->
        internal_dir ->
            case NewAttribEntry#inode_entry.name of
                "attribs" ->
                    FileIno=inode:is_numbered(File),
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

%% Since the attribute folder already exists, things needn't get overly coplicated here...
make_rename_file_reply(NewAttribIno,FileInode,FileEntry) ->
    ?DEB1("   copying file"),
    Path=(FileEntry#inode_entry.type)#external_file.path,
    Attribute=inode:is_named(NewAttribIno),
    case Attribute of
        {_Key,_Value} ->
            add_new_attribute(Path,FileInode,FileEntry,Attribute),
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
            remove_old_attribute_key(FilePath,FileInode,OldKeyName),
            add_new_attribute(FilePath,FileInode,FileEntry,NewAttribName)
        end,
        OldValueEntry#inode_entry.children),
    ?DEBL("    removing ~p from the ~p directory", [OldValueName,OldKeyName]),
    KeyIno=inode:is_numbered(OldKeyName),
    remove_empty_dir(KeyIno,OldValueName),
    ?DEBL("    adding ~p to ~p directory", [NewValueName,NewKeyName]),
    tree_srv:enter(ValueIno,NewValueEntry,inodes),
    ?DEB2("    adding ~p to inode list",NewAttribName),
    append_child({NewValueName,ValueIno},KeyIno),
    ?DEB1("    moving inode number"),
    inode:rename(OldAttribName,NewAttribName),
    ok.

%%--------------------------------------------------------------------------
%% So, this will take an attribute directory, move it into a "key position" (as a direct child to "attribs"), and consequently change the attribute names of all subfolders and subfolder file attributes, unless this is done with subsequent calls to rename by the OS.
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
        Children),
    ?DEB2("    removing ~p from attribute directory", OldKeyName),
    AttrsIno=inode:is_numbered("attribs"),
    remove_empty_dir(AttrsIno,OldKeyName),
    ?DEB2("    adding ~p to inode tree and attribute directory", NewKeyName),
    tree_srv:enter(KeyIno,NewKeyEntry,inodes),
    append_child({NewKeyName,KeyIno},AttrsIno),
    ?DEB1("    moving inode number"),
    inode:rename(OldKeyName,NewKeyName),
    ok.

%%%=========================================================================
%%% Shared utility functions
%%%=========================================================================

%%--------------------------------------------------------------------------
%insert entry Entry with into the file system tree under ParentInode. Returns new inode.
%%--------------------------------------------------------------------------
insert_entry(ParentInode,ChildEntry) ->
    ?DEBL("    inserting new entry as child for ~p",[ParentInode]),
    {value,ParentEntry}=tree_srv:lookup(ParentInode,inodes),

    InoName=ChildEntry#inode_entry.name,
    ChildInode=inode:get(InoName),
    ChildName=case ChildEntry#inode_entry.type of
        #attribute_dir{atype=value} ->
            {_,Name} = InoName,
            Name;
        _ -> InoName
    end,
    NewChildren=[{ChildName,ChildInode}|ParentEntry#inode_entry.children],
    tree_srv:enter(ParentInode,ParentEntry#inode_entry{children=NewChildren},inodes),
    tree_srv:enter(ChildInode,ChildEntry,inodes),
    ChildInode.

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

%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
append_child(NewChild={_ChildName,_ChildIno},ParentIno) ->
    {value,ParentEntry}=tree_srv:lookup(ParentIno,inodes),
    Children=ParentEntry#inode_entry.children,
    NewChildren=keymergeunique(NewChild,Children),
    NewParentEntry=ParentEntry#inode_entry{children=NewChildren},
    tree_srv:enter(ParentIno,NewParentEntry,inodes).

%%--------------------------------------------------------------------------
%% datetime_to_epoch takes a {{Y,M,D},{H,M,S}} and transforms it into seconds elapsed from 1970/1/1 00:00:00, GMT
%%--------------------------------------------------------------------------
datetime_to_epoch(DateTime) ->
    calendar:datetime_to_gregorian_seconds(DateTime) - calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}).

%%--------------------------------------------------------------------------
%% statify_file_info transforms a file.#file_info{} into a fuserl.#stat{}
%%--------------------------------------------------------------------------
statify_file_info(#file_info{size=Size,type=_Type,atime=Atime,ctime=Ctime,mtime=Mtime,access=_Access,mode=Mode,links=Links,major_device=MajorDevice,minor_device=MinorDevice,inode=Inode,uid=UID,gid=GID}) ->
    ?DEBL("    converting file info for ~p to fuse stat info",[Inode]),
    #stat{
        st_dev= {MajorDevice,MinorDevice}
         ,st_ino=Inode
         ,st_mode=Mode
         ,st_nlink=Links
         ,st_uid=UID
         ,st_gid=GID
        %,st_rdev
         ,st_size=Size
        %,st_blksize
        %,st_blocks
         ,st_atime=datetime_to_epoch(Atime)
         ,st_mtime=datetime_to_epoch(Mtime)
         ,st_ctime=datetime_to_epoch(Ctime)
         }.



%%--------------------------------------------------------------------------
%% lookup_children returns the children for the inode, if the inode is 
%% present. Returns like gb_trees:lookup
%%--------------------------------------------------------------------------
lookup_children(Inode) ->
    case tree_srv:lookup(Inode,inodes) of
        {value, Entry} -> {value,Entry#inode_entry.children};
        none -> none
    end.

%%--------------------------------------------------------------------------
%% get_open_files returns the opened files for the state
%%--------------------------------------------------------------------------
get_open_files({Ctx,_Inode}) ->
    case tree_srv:lookup(Ctx,open_files) of
        {value,OpenFiles} ->
            OpenFiles;
        _ -> gb_trees:empty()
    end.
%    State#state.open_files.

%%--------------------------------------------------------------------------
%% set_open_files returns a state with the open files updated
%%--------------------------------------------------------------------------
set_open_files({Ctx,_Inode},OpenFiles) ->
    tree_srv:enter(Ctx,OpenFiles,open_files).

%%--------------------------------------------------------------------------
%% get_open_file gets the open file corresponding to the inode provided.
%% returns like gb_trees:lookup
%%--------------------------------------------------------------------------
lookup_open_file({_Ctx,Inode}=Token) ->
    gb_trees:lookup(Inode,get_open_files(Token)).

%%--------------------------------------------------------------------------
%% set_open_file returns a state with the open file for the provided inode
%% changed to the FileContents provided.
%%--------------------------------------------------------------------------
set_open_file({_Ctx,Inode}=Token,FileContents) ->
    OpenFiles=get_open_files(Token),
    NewOpenFiles=gb_trees:enter(Inode,FileContents,OpenFiles),
    set_open_files(Token,NewOpenFiles).

remove_open_file({_Ctx,Inode}=Token) ->
    OpenFiles=get_open_files(Token),
    NewOpenFiles=gb_trees:delete_any(Inode,OpenFiles),
    set_open_files(Token,NewOpenFiles).

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
    append_attribute(Attr,FName,Stat),
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
    rehash_ext_from_db(Inode,Path),
    % Attribute dir handling
    remove_child_from_parent(inode:is_named(Inode),Attribute).


remove_child_from_parent(ChildName,ParentName) ->
    Inode=inode:get(ParentName),
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
    rehash_ext_from_db(Inode,Path),

    % removing file child from attribute folder entry
    FName=inode:is_named(Inode),
    lists:foreach(
        fun(AValue) -> 
            remove_child_from_parent(FName,{AName,AValue})
        end,
        Matches).

%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
dir(Stat) ->
    NewMode=(Stat#stat.st_mode band 8#777) bor ?S_IFDIR,
    ?DEBL("   transforming mode ~.8B into mode ~.8B",[Stat#stat.st_mode,NewMode]),
    Stat#stat{st_mode=NewMode}.

%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
generate_ext_info(Path) ->
    ?DEB2("   generating ext info for ~p",Path),
    ExtInfo0=dets:match(?ATTR_DB,{Path,'$1'}), 
    lists:flatten(ExtInfo0). % Going from a list of lists of tuples to a list of tuples.

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
    ?DEB2("    Adding zero to end of name ~p, and \"user.\" to the start",Name),
    Name0="user."++Name++"\0",
    ?DEB1("    Appending name to namelist"),
    NewString=String++Name0,
    ?DEB1("    Recursion"),
    ext_info_to_ext_io(InternalExtInfoTupleList,NewString).


%%--------------------------------------------------------------------------
%% Gets the children for the inode from the inode list, and runs direntrify
%% on it.
%%--------------------------------------------------------------------------
direntries(Inode) ->
    ?DEB1("    Creating direntries"),
    ?DEB1("     Getting child entries"),
    {value,Children}=lookup_children(Inode),
    ?DEBL("     Converting children ~p for ~p",[Children,Inode]),
    direntrify(Children).


%%--------------------------------------------------------------------------
%% direntrify takes a [{Name,Inode}] and returns a [fuserl:#{direntry}]
%%--------------------------------------------------------------------------
direntrify([]) -> 
    ?DEB1("    Done converting children"),
    [];

direntrify([{Name,Inode}|Children]) ->
    ?DEB2("    Getting inode for child ~p",{Name,Inode}),
    {value,Child}=tree_srv:lookup(Inode,inodes),
    ?DEB2("    Getting permissions for child ~p",{Name,Inode}),
    ChildStats=Child#inode_entry.stat,
    ?DEB2("    Creating direntry for child ~p",{Name,Inode}),
    Direntry= #direntry{name=Name ,stat=ChildStats },
    ?DEB2("    Calculatig size for direntry for child ~p",{Name,Inode}),
    Direntry1=Direntry#direntry{offset=fuserlsrv:dirent_size(Direntry)},
    ?DEB2("    Appending child ~p to list",{Name,Inode}),
    [Direntry1|direntrify(Children)].



%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
assert_started(Application) ->
    ?DEB2("   Checking that ~p is loaded",Application),
    case find(
              fun({A,_,_}) when A==Application -> true;
                 (_) -> false 
              end,application:loaded_applications())
       of
         true -> ?DEB1("   loaded"),true;
         false -> ?DEB1("   not loaded, starting"),application:start(Application)
    end.

%%--------------------------------------------------------------------------
%% find runs SearchFun for one element at a time in the provided list,
%% until SearchFun returns {true,Value} or true, whereupon it returns this.
%%--------------------------------------------------------------------------
find(_,[]) -> false;
find(SearchFun,[Item|Items]) ->
    case SearchFun(Item) of
        {true,Value} -> {true,Value};
        true -> true;
        _ -> find(SearchFun,Items)
    end.


%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
make_inode_list({Path,Name}) ->
    ?DEBL("   reading file info for ~p into ~p",[Path,Name]),
    case catch file:read_file_info(Path) of
        {ok, FileInfo} ->
            ?DEB1("   got file info"),
            {ok,Children,Type} = case FileInfo#file_info.type of
                directory ->
                    ?DEB1("    directory"),
                    {ok,ChildNames} = file:list_dir(Path),
                    ?DEB2("     directory entries:~p",ChildNames),
                    NameInodePairs=
                        lists:map(
                            fun(ChildName) -> {ChildName,inode:get(ChildName)} end, 
                            ChildNames),
                    {ok,NameInodePairs,#external_dir{external_file_info=FileInfo,path=Path}};
                regular ->
                    ?DEB1("    regular"),
                    {ok,[],#external_file{external_file_info=FileInfo,path=Path}};
                        _ ->
                    {error,not_supported} % for now
            end,
            ?DEB2("    Generating ext info for ~p",Path),
            {ExtInfo,ExtIo}=generate_ext_info_io(Path), 
            ?DEB2("     ext info: ~p", ExtInfo),
            % XXX: This will break if provided with a local date and time that does not
            % exist. Shouldn't be much of a problem.
            EpochAtime=lists:nth(1,calendar:local_time_to_universal_time_dst(FileInfo#file_info.atime)),
            EpochCtime=lists:nth(1,calendar:local_time_to_universal_time_dst(FileInfo#file_info.ctime)),
            EpochMtime=lists:nth(1,calendar:local_time_to_universal_time_dst(FileInfo#file_info.mtime)),
                
            ?DEBL("\tatime:~p~n\t\t\tctime:~p~n\t\t\tmtime:~p",[EpochAtime,EpochCtime,EpochMtime]),
            MyStat=statify_file_info(
                    FileInfo#file_info{
                        inode=inode:get(Name)
                       ,atime=EpochAtime
                       ,ctime=EpochCtime
                       ,mtime=EpochMtime
                           }),

            InodeEntry=#inode_entry{ 
                name=Name
                ,children=Children
                ,type=Type
                ,stat=MyStat
                ,ext_info=ExtInfo
                ,ext_io=ExtIo},
            tree_srv:enter(inode:get(Name),InodeEntry,inodes),
            lists:foreach(fun(Attr) -> append_attribute(Attr,Name,MyStat) end,ExtInfo),
            lists:foreach(fun({ChildName,_Inode})->make_inode_list({Path++"/"++ChildName,ChildName}) end,Children);
        E ->
            ?DEBL("   got ~p when trying to read ~p.",[E,Path]),
            ?DEB1("   are you sure your app file is correctly configured?"),
            ?DEB1(">>>exiting<<<"),
            exit(E)
    end.


%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
append_attribute({Key,Val},Name,Stat) ->
    ?DEBL("    appending ~p/~p",[{Key,Val},Name]),
    append_value_dir(Key,Val,Name,Stat),
    ?DEBL("    appending ~p/~p",[Key,Val]),
    append_key_dir(Key,Val,Stat),
    tree_srv:enter(Key,inode:get(Key),keys),
    AttributesFolderIno=inode:get("attribs"),
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
    ChildIno=inode:get({KeyDir,ValDir}),
    MyInode=inode:get(KeyDir),
    NewEntry=case tree_srv:lookup(MyInode,inodes) of
        % No entry found, creating new attribute entry.
        none ->
                #inode_entry{
                    type=#attribute_dir{atype=key},
                    name=KeyDir,
                    children=[{ValDir,ChildIno}],
                    stat=dir(Stat#stat{st_ino=MyInode}),
                    ext_info=[],
                    ext_io=ext_info_to_ext_io([])
                };
        {value,OldEntry} ->
        ?DEBL("   merging ~p into ~p",[{ValDir,ChildIno},OldEntry#inode_entry.children]),
            OldEntry#inode_entry{
                children=
                    keymergeunique({ValDir,ChildIno},OldEntry#inode_entry.children)
                }
    end,
    tree_srv:enter(MyInode,NewEntry,inodes).

%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
append_value_dir(Key,Value,ChildName,Stat) ->
    ChildIno=inode:get(ChildName),
    MyInode=inode:get({Key,Value}),
    NewEntry=case tree_srv:lookup(MyInode,inodes) of
        % No entry found, creating new attribute entry.
        none ->
                #inode_entry{
                    type=#attribute_dir{atype=value},
                    name={Key,Value},
                    children=[{ChildName,ChildIno}],
                    stat=dir(Stat#stat{st_ino=MyInode}),
                    ext_info=[],
                    ext_io=ext_info_to_ext_io([])
                };
        {value,OldEntry} ->
        ?DEBL("   merging ~p into ~p",[{ChildName,ChildIno},OldEntry#inode_entry.children]),
            OldEntry#inode_entry{
                children=
                    keymergeunique({ChildName,ChildIno},OldEntry#inode_entry.children)
                }
    end,
    ?DEB1("  entering new entry into server"),
    tree_srv:enter(MyInode,NewEntry,inodes).

%%--------------------------------------------------------------------------
%seems like lists:keymerge won't do what I ask it, so I build my own...
%%--------------------------------------------------------------------------
keymergeunique(Tuple,TupleList) ->
    keymerge(Tuple,TupleList,[]).

keymerge(Tuple,[],FilteredList) ->
    [Tuple|FilteredList];

keymerge(Tuple={Key,_Value},[{Key,_}|TupleList],FilteredList) ->
    keymerge(Tuple,TupleList,FilteredList);

keymerge(Tuple={_Key,_Value},[OtherTuple|TupleList],FilteredList) ->
    keymerge(Tuple,TupleList,[OtherTuple|FilteredList]).

%%--------------------------------------------------------------------------
%% (This one I stole from fuserlproc.)
%% this take_while runs the function F until it returns stop
%% Good for taking items from list where the number of list items taken
%% are dependent on the individual size of each item.
%%--------------------------------------------------------------------------
take_while (_, _, []) -> 
    [];
take_while (F, Acc, [ H | T ]) ->
    case F (H, Acc) of
        { continue, NewAcc } ->
            [ H | take_while (F, NewAcc, T) ];
        stop ->
            []
    end.

%%--------------------------------------------------------------------------
%% @spec (boolean(),A,B) -> A|B.
%% @doc Takes a boolean and redefines the meanings of true and false.
%% Example: transmogrify(is_foo(X),good,{error,no_foo}) will return either 
%% good or {error, no_foo}, depending on whether is_foo(X) returns true or 
%% false.
%%--------------------------------------------------------------------------
transmogrify(TrueOrFalse,NewTrue,NewFalse) ->
    if 
        TrueOrFalse -> 
            ?DEBL("   transmogrifying ~p into ~p",[TrueOrFalse,NewTrue]),
            NewTrue;
        true -> 
            ?DEBL("   transmogrifying ~p into ~p",[TrueOrFalse,NewFalse]),
            NewFalse
    end.

%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
test_access(Inode,Mask,Ctx) ->
    ?DEB1("   checking access..."),
    case tree_srv:lookup(Inode,inodes) of
        {value, Entry} ->
            % Can I use the mask like this?
            case Mask of
                ?F_OK ->
                    ?DEB1("   file existing"),
                    ok;
                _ -> 
                    transmogrify(has_rwx_access(Entry,Mask,Ctx),ok,eacces)
            end;
        none ->
            ?DEB1("   file does not exist!"),
            enoent
    end.

%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
has_rwx_access(Entry,Mask,Ctx) ->
    #stat{st_mode=Mode,st_uid=Uid,st_gid=Gid}=Entry#inode_entry.stat,
    has_other_perms(Mode,Mask)
        orelse Gid==Ctx#fuse_ctx.gid andalso has_group_perms(Mode,Mask)
        orelse Uid==Ctx#fuse_ctx.uid andalso has_user_perms(Mode,Mask).

%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
has_other_perms(Mode,Mask) ->
    Mode band ?S_IRWXO band Mask/=0.

%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
has_group_perms(Mode,Mask) ->
    Mode band ?S_IRWXG bsr 3 band Mask/=0.

%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
has_user_perms(Mode,Mask) ->
    Mode band ?S_IRWXU bsr 6 band Mask/=0.

%%--------------------------------------------------------------------------
%% removes string2 from the beginning of string1, if applicable. Returns what was left of string1
%%--------------------------------------------------------------------------
stringdelta(String1,[]) -> String1;

stringdelta([],_String2) -> []; %String2;

stringdelta([C1|String1],[C2|String2]) when C1 == C2 ->
    stringdelta(String1,String2);

stringdelta([C1|String1],[C2|_String2]) when C1 /= C2 ->
    String1.



%%%=========================================================================
%%% Debug functions
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
    inode:list_bound().

