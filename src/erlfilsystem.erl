-module(erlfilsystem).

-export([start_link/1,start_link/2,start_link/3,start_link/4]).
%%%--------------
%%% Fuserl functions
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

-export([handle_info/2,init/1]).
-export([code_change/3]). % TODO: Do something intelligent with this one. Returns "ok" now, totally ignoring its indata.

-behaviour(fuserl).

-include_lib("fuserl/include/fuserl.hrl").
-include("../include/erlfilsystem.hrl").


%% a small record to make it possible for me to choose which start options I want to give
%% Only used here, only used as a test, only used temporary.
-record(startopts,{
    linkedin=false,
    mountopts,%="",
    dir}).

start_link(Startopts=#startopts{}) ->
    start_link(Startopts#startopts.dir,Startopts#startopts.linkedin,Startopts#startopts.mountopts);

start_link(Dir) ->
    start_link(Dir,false).

start_link(Dir,LinkedIn) ->
    start_link(Dir,LinkedIn,"debug").

start_link(MountDir,LinkedIn,MountOpts) ->
    start_link(MountDir,LinkedIn,MountOpts,_StartOpts=_MirrorDir=MountDir). %TODO: make the last Dir mirror an arbitary directory, infusing its contents into the first Dir.

start_link(Dir,LinkedIn,MountOpts,Args) ->
    Options=[],
    fuserlsrv:start_link(?MODULE,LinkedIn,MountOpts,Dir,Args,Options).

init(Dir) ->
    io:format("<init dir=\"~p\"",[Dir]),
    State=make_initiate_state(Dir),
    io:format(" state=\"~p\">",[State]),
    {ok, State}.

%% Reads the contents of a dir, putting every file in the dir in my inode list.
%% Returns a state record.
make_initiate_state(Dir) ->
   {ok,CWD}=file:get_cwd(),
   {ok,Names}=file:list_dir(Dir),
   ok=file:set_cwd(Dir),
   FirstInode=1,
   {InodeEntryList,NewBiggest}=
       lists:mapfoldr
           % I use a fun here, because I might need the Dir from above, and must insert it directly into the function (unless erlang is more into currying than I think).
           (fun(Name,InodeNo) ->
               {ok,FileInfo} = file:read_file_info(Name),
               InodeEntry=#inode_entry{
                   children=[], % TODO: do something recursive here, if possible.
                   type=real_file, % For now, assume that this name points to a file.
                   real_file_info=FileInfo,
                   fake_file_info=FileInfo#file_info{inode=InodeNo},
                   ext_info=[]}, % XXX: This is where I will put in my file system specific magic.
               {{InodeNo,InodeEntry},InodeNo+1}
           end,
           FirstInode, % The inode number to assign the first name to.
           Names),
   InodeList=#inode_list{biggest=NewBiggest,inode_entries=gb_trees:from_orddict(InodeEntryList)},
   file:set_cwd(CWD),
   #state{inode_list=InodeList}.


                







     
access(_Ctx,_Inode,_Mask,_Continuation,State) ->
    io:format("~s I: ~p M: ~p\n",["access!",_Inode,_Mask]),
    % So, if I've gotten this right, I've got an inode to look up rights for, a context in Ctx which tells me who wanted to know their rights to the file, a mask, which does SOMETHING - maybe this is what is used on file systems who has no rights normally? - a Continuation, which is a magical item which somehow is used to make asyncronuous calls - more on this when I find a documentation for the record - and finally the state, which, afaik, I will NOT be changing by checking access to files.
    % First, lets try to just create an "access denied" version of this thing.
    {#fuse_reply_err{err=ebadr},State}. % like this?

create(_Ctx,_Parent,_Name,_Mode,_Fuse_File_Info,_Continuation, State) ->
    io:format("~s",["create!\n"]),
    {#fuse_reply_err{err=enotsup},State}.

flush(_Ctx,_Inode,_Fuse_File_Info,_Continuation,State) ->
    io:format("~s",["flush!\n"]),
    {#fuse_reply_err{err=enotsup},State}.

forget(_Ctx,_Inode,_Nlookup,_Continuation,State) ->
    io:format("~s",["forget!\n"]),
    {#fuse_reply_none{},State}.

fsync(_Ctx,_Inode,_IsDataSync,_Fuse_File_Info,_Continuation,State) ->
    io:format("~s",["fsync!\n"]),
    {#fuse_reply_err{err=enotsup},State}.

fsyncdir(_Ctx,_Inode,_IsDataSync,_Fuse_File_Info,_Continuation,State) ->
    io:format("~s",["fsyncdir!\n"]),
    {#fuse_reply_err{err=enotsup},State}.
    
getattr(#fuse_ctx{uid=Uid,gid=Gid,pid=Pid},Inode,_Continuation,State) ->
    io:format("~s",["<getattr>\n"]),
    io:format(" <pid=\"~p\"/>\n",[Pid]),
    {#fuse_reply_attr{
        attr=#stat {
            % Seems like I don't have to bother about these, I think.
%            st_dev = { 32, 453 }, %XXX: Ugly! How to get a good number?
%            st_rdev = { 32, 455 }, %XXX: See above.

            st_ino=Inode,
            st_nlink=2,
            st_uid=Uid,
            st_gid=Gid,
%            st_size=4096,
%            st_blksize=8,
%            st_blocks=4096,
%            st_atime=1234567890,
%            st_mtime=Pid,
%            st_ctime=012345678,
            st_mode=?S_IFDIR bor 8#0755
                
        },
        attr_timeout_ms=0
        }
    ,
    io:format("</getattr>\n"),
    State}.



%    {#fuse_reply_err{err=eacces},State}.

getlk(_Ctx,_Inode,_Fuse_File_Info,_Lock,_Continuation,State) ->
    io:format("~s",["getlk!\n"]),
    {#fuse_reply_err{err=enotsup},State}.

getxattr(_Ctx,_Inode,_Name,_Size,_Continuation,State) ->
    io:format("~s",["getxattr!\n"]),
    {#fuse_reply_err{err=enotsup},State}.
    
link(_Ctx,_Inode,_NewParent,_NewName,_Continuation,State) ->
    io:format("~s",["link!\n"]),
    {#fuse_reply_err{err=enotsup},State}.
    
listxattr(_Ctx,_Inode,_Size,_Continuation,State) ->
    io:format("~s",["listxattr!\n"]),
    {#fuse_reply_err{err=enotsup},State}.
    
lookup(_Ctx,_ParentInode,_Name,_Continuation,State) ->
    io:format("~s Parent: ~p Name: ~p\n",["lookup!",_ParentInode,_Name]),
    {#fuse_reply_err{err=enoent},State}. %empty file system, bitch!

mkdir(_Ctx,_ParentInode,_Name,_Mode,_Continuation,State) ->
    io:format("~s",["mkdir!\n"]),
    {#fuse_reply_err{err=enotsup},State}.

mknod(_Ctx,_ParentInode,_Name,_Mode,_Dev,_Continuation,State) ->
    io:format("~s",["mknod!\n"]),
    {#fuse_reply_err{err=enotsup},State}.

open(_Ctx,_Inode,_Fuse_File_Info,_Continuation,State) ->
    io:format("~s",["open!\n"]),
    {#fuse_reply_err{err=enotsup},State}.

opendir(_Ctx,_Inode,_FI=#fuse_file_info{flags=Flags,writepage=Writepage,direct_io=DirectIO,keep_cache=KeepCache,flush=_,fh=Fh,lock_owner=_},_Continuation,State) ->
    io:format("<opendir>\n <flags=\"~.8B\">\n <writepage=\"~p\"/>\n <direct_io=\"~p\"/>\n <keep_cache=\"~p\"/>\n <file_handle=\"~p\"/>\n",[Flags, Writepage, DirectIO, KeepCache, Fh]),
    {#fuse_reply_open{ fuse_file_info = _FI }, State}.
%    {#fuse_reply_err{err=enotsup},State}.

read(_Ctx,_Inode,_Size,_Offset,_Fuse_File_Info,_Continuation,State) ->
    io:format("~s",["read!\n"]),
    {#fuse_reply_err{err=enotsup},State}.

-define (DIRATTR (X), #stat{ st_ino = (X), 
                             st_mode = ?S_IFDIR bor 8#0555, 
                             st_nlink = 1 }).

%-define (DIRATTR (X), #stat{ st_ino = (X), 
%                             st_mode = ?S_IFDIR bor 8#0555, 
%                             st_nlink = 1 }).


-define(STAT (X,Y,Z), #stat{ st_ino = (X),
                             st_mode = (Y),
                             st_nlink = Z
                                 }.

%% blatantly stolen from fuserlprocserv.erl
readdir(_Ctx,Inode,Size,Offset,_Fuse_File_Info,_Continuation,State=#state{inode_list=InodeList}) ->
  io:format("~s ~p\n",["readdir! Offset:", Offset]),
  DirEntryList = 
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
       lists:nthtail(Offset,direntries(Inode,InodeList)) %TODO: create this in the opendir call instead!
      ),


  { #fuse_reply_direntrylist{ direntrylist = DirEntryList }, State }.

direntries(Inode,InodeList) ->
    InodeEntry=gb_trees:get(Inode,InodeList),
    Children=InodeEntry#inode_entry.children,
    direntrify(Children).

direntrify([]) -> [];

direntrify([{Name,Inode}|Children]) ->
    Child=gb_sets:get(Inode),
    ChildPerms=(Child#inode_entry.fake_file_info)#file_info.mode,
    Direntry=
        #direntry{name=Name
                  ,stat=#stat{ st_ino=Inode,
                               st_mode=ChildPerms,
                               st_nlink=1 % For now. TODO: Make this mirror how many directories the file is in? Maybe just count the number of attributes and add one?
                              }
                    },
    Direntry1=Direntry#direntry{offset=fuserlsrv:dirent_size(Direntry)},
    [Direntry1|direntrify(Children)].
               





take_while (_, _, []) -> 
  [];
take_while (F, Acc, [ H | T ]) ->
  case F (H, Acc) of
    { continue, NewAcc } ->
      [ H | take_while (F, NewAcc, T) ];
    stop ->
      []
  end.

    %{#fuse_reply_err{err=enotsup},State}.

readlink(_Ctx,_Inode,_Continuation,State) ->
    io:format("~s",["readlink!\n"]),
    {#fuse_reply_err{err=enotsup},State}.
    
release(_Ctx,_Inode,_Fuse_File_Info,_Continuation,State) ->
    io:format("~s",["release!\n"]),
    {#fuse_reply_err{err=enotsup},State}.

releasedir(_Ctx,_Inode,_Fuse_File_Info,_Continuation,State) ->
    io:format("~s",["releasedir!\n"]),
    {#fuse_reply_err{err=ok},State}.

removexattr(_Ctx,_Inode,_Name,_Continuation,State) ->
    io:format("~s",["removexattr!\n"]),
    {#fuse_reply_err{err=enotsup},State}.

rename(_Ctx,_Parent,_Name,_NewParent,_NewName,_Continuation,State) ->
    io:format("~s",["rename!\n"]),
    {#fuse_reply_err{err=enotsup},State}.

rmdir(_CTx,_Inode,_Name,_Continuation,State) ->
    io:format("~s",["rmdir!\n"]),
    {#fuse_reply_err{err=enotsup},State}.

setattr(_Ctx,_Inode,_Attr,_ToSet,_Fuse_File_Info,_Continuation,State) ->
    io:format("~s",["setattr!\n"]),
    {#fuse_reply_err{err=enotsup},State}.

setlk(_Ctx,_Inode,_Fuse_File_Info,_Lock,_Sleep,_Continuation,State) ->
    io:format("~s",["setlk!\n"]),
    {#fuse_reply_err{err=enotsup},State}.

setxattr(_Ctx,_Inode,_Name,_Value,_Flags,_Continuation,State) ->
    io:format("~s",["setxattr!\n"]),
    {#fuse_reply_err{err=enotsup},State}.

statfs(_Ctx,_Inode,_Continuation,State) ->
    io:format("~s",["statfs!\n"]),
    {#fuse_reply_err{err=enotsup},State}.

symlink(_Ctx,_Link,_Inode,_Name,_Continuation,State) ->
    io:format("~s",["symlink!\n"]),
    {#fuse_reply_err{err=enotsup},State}.

terminate(_Reason,_State) ->
    io:format("~s ~p\n",["terminate!",_Reason]),
    exit(3).

unlink(_Ctx,_Inode,_Name,_Cont,State) ->
    io:format("~s\n",["unlink!"]),
    {#fuse_reply_err{err=enotsup},State}.

write(_Ctx,_Inode,_Data,_Offset,_Fuse_File_Info,_Continuation,State) ->
    io:format("~s",["write!"]),
    {#fuse_reply_err{err=enotsup},State}.

    

handle_info(_Msg,State) ->
    io:format("~s",["handle_info!"]),
    {noreply,State}.

code_change(_,_,_) -> %XXX: Maybe do something more intelligent with this?
    io:format("~s",["code_change!"]),
    ok.


