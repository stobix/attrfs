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

%% Debug functions
-export([get_dir_hier/2]).

-export([test/2]).

-behaviour(fuserl).

-include_lib("fuserl/include/fuserl.hrl").
-include_lib("kernel/include/file.hrl").
-include("../include/erlfilsystem.hrl").
-include("../include/debug.hrl").


%% a small record to make it possible for me to choose which start options I want to give
%% Only used here, only used as a test, only used temporary.
-record(startopts,{
    linkedin=false,
    mountopts,%="",
    dir}).

start_link(Startopts=#startopts{}) ->
    start_link(Startopts#startopts.dir,Startopts#startopts.linkedin,Startopts#startopts.mountopts);


start_link({MountDir,MirrorDir}) ->
    start_link(MountDir,false,"",MirrorDir);

start_link(Dir) ->
    start_link(Dir,false). % A true value for linked in "jeopardizes the erlang emulator stability" according to fuserlproc

start_link(Dir,LinkedIn) ->
%    start_link(Dir,LinkedIn,"debug").
    start_link(Dir,LinkedIn,"").

start_link(MountDir,LinkedIn,MountOpts) ->
    start_link(MountDir,LinkedIn,MountOpts,MountDir). %TODO: make the last Dir mirror an arbitary directory, infusing its contents into the first Dir.

start_link(Dir,LinkedIn,MountOpts,MirrorDir) ->
    Options=[],
    
    ?DEBL("   opening attribute database file ~p as ~p~n", [?ATTR_DB_FILE, ?ATTR_DB]),
    {ok,_}=dets:open_file(?ATTR_DB,[{type,bag},{file,?ATTR_DB_FILE}]),
    ?DEB2("   mirroring dir ~p~n",MirrorDir),
    {InodeList,BiggestIno}=make_inode_list({MirrorDir,1},2),
    ?DEB1("   inode list made"),
    InodeTree=gb_trees:from_orddict(InodeList),
    ?DEB1("   inode tree made"),
    InodeListRec=#inode_list{inode_entries=InodeTree,biggest=BiggestIno},
    ?DEB1("   inode list made"),
    State=#state{inode_list=InodeListRec,open_files=gb_trees:empty()},
    assert_started(fuserl),
    ?DEB1("   fuserl started, starting fuserlsrv"),
    fuserlsrv:start_link(?MODULE,LinkedIn,MountOpts,Dir,State,Options).

assert_started(Application) ->
    case find(
              fun({A,_,_}) when A==Application -> {true,A};
                 (_) -> false 
              end,application:loaded_applications())
       of
         {true,_} -> true;
         false -> application:start(Application)
    end.


find(_,[]) -> false;
find(SearchFun,[Item|Items]) ->
    case SearchFun(Item) of
        {true,Value} -> {true,Value};
        _ -> find(SearchFun,Items)
    end.


init(State=#state{}) ->
    {ok, State}.

%% Reads the contents of a dir, putting every file in the dir in my inode list. Recursively.
%% Reads the contents of a dir, recursively, and produces a non-flat list of {Inode,#inode_entry{}}'s, which can be sent to gb_trees:from_orddict after it has been flattened.
make_inode_list({Entry,InitialIno},NextIno) ->
    ?DEB1("Making inode list"),
    {ok,FileInfo}=file:read_file_info(Entry),
    {ok,Children,Type,NewIno} = case FileInfo#file_info.type of
        directory ->
            ?DEB2(" directory ~p~n",Entry),
            {ok,Names} = file:list_dir(Entry),
            ?DEB2("  directory entries:~p~n",Names),
            {NameInodePairs,MyIno}=
                %make entries of type {{ExternalName,InternalName},Ino}
                lists:mapfoldl(
                    fun(Name,Ino) -> {{Name,Ino},Ino+1} end, 
                    %fun(Name,Ino) -> {Entry++"/"++Name,Ino},Ino+1} end, 
                    NextIno, Names),
            {ok,NameInodePairs,#external_dir{external_file_info=FileInfo,path=Entry},MyIno};
        regular ->
            ?DEB1(" regular"),
            % TODO: Get attributes for file and set them as children. I think.
            {ok,[],#external_file{external_file_info=FileInfo,path=Entry},NextIno};
                _ ->
            {error,not_supported} % for now
    end,
    ExtInfo=generate_ext_info(Entry),
    ?DEB2("  ext info: ~p~n", ExtInfo),
        ?DEB1("ext_info_to_ext_io"),
    ExtIo=ext_info_to_ext_io(ExtInfo),
    ?DEB1("Generating entry"),
    InodeEntry=#inode_entry{ 
             children=Children
            ,type=Type
            ,internal_file_info=FileInfo#file_info{inode=InitialIno}
            ,ext_info=ExtInfo
            ,ext_io=ExtIo},
    {ChildInodeEntries,FinalIno} = 
        lists:mapfoldl(
            fun({A,AA},B)->make_inode_list({Entry++"/"++A,AA},B) end
                ,NewIno,Children),
    % A list needs to be flat and ordered to be used by gb_trees:from_orddict/1
    {lists:keysort(1,lists:flatten([{InitialIno,InodeEntry},ChildInodeEntries])),FinalIno}.
        

get_dir_hier(InodeNo,InodeEntries) ->
    InodeEntry=gb_trees:get(InodeNo,InodeEntries),
    case InodeEntry#inode_entry.children of
        [] -> [];
        Children -> 
            lists:map(fun({Child,No}) -> {Child,get_dir_hier(No,InodeEntries)} end, Children)
    end.


get_inode_entries(State) ->
    (State#state.inode_list)#inode_list.inode_entries.

lookup_inode_entry(Inode,State) ->
    gb_trees:lookup(Inode,get_inode_entries(State)).

lookup_children(Inode,State) ->
    case lookup_inode_entry(Inode,State) of
        {value, Entry} -> {value,Entry#inode_entry.children};
        none -> none
    end.

get_open_files(State) ->
    State#state.open_files.

set_open_files(State,OpenFiles) ->
    State#state{open_files=OpenFiles}.

get_open_file(State,Inode) ->
    gb_trees:lookup(Inode,get_open_files(State)).

set_open_file(State,Inode,FileContents) ->
    OpenFiles=get_open_files(State),
    NewOpenFiles=gb_trees:enter(Inode,FileContents,OpenFiles),
    set_open_files(State,NewOpenFiles).

%get_biggest_inode_number(State) ->
%    (State#state.inode_list)#inode_list.biggest.

statify_internal_file_info(#inode_entry{internal_file_info=InternalFileInfo}) ->
    statify_file_info(InternalFileInfo).

%TODO: Find out what info it is I cannot provid here lest fuserl hangs.
statify_file_info(#file_info{size=_Size,type=_Type,atime=_Atime,ctime=_Ctime,mtime=_Mtime,access=_Access,mode=_Mode,links=Links,major_device=_MajorDevice,minor_device=_MinorDevice,inode=Inode,uid=UID,gid=GID}) ->
    #stat{
%        st_dev= {MajorDevice,MinorDevice}
         st_ino=Inode
         ,st_mode=?S_IFDIR bor 8#00755 %Mode
         ,st_nlink=Links
         ,st_uid=UID
         ,st_gid=GID
        %,st_rdev
%         ,st_size=Size
        %,st_blksize
        %,st_blocks
%         ,st_atime=Atime
%         ,st_mtime=Mtime
%         ,st_ctime=Ctime
             }.
           
     
access(_Ctx,_Inode,_Mask,_Continuation,State) ->
    ?DEBL("~s I: ~p M: ~p\n",["access!",_Inode,_Mask]),
    % So, if I've gotten this right, I've got an inode to look up rights for, a context in Ctx which tells me who wanted to know their rights to the file, a mask, which does SOMETHING - maybe this is what is used on file systems who has no rights normally? - a Continuation, which is a magical item which somehow is used to make asyncronuous calls - more on this when I find a documentation for the record - and finally the state, which, afaik, I will NOT be changing by checking access to files.
    % First, lets try to just create an "access denied" version of this thing.
    {#fuse_reply_err{err=ebadr},State}. % like this?

create(_Ctx,_Parent,_Name,_Mode,_Fuse_File_Info,_Continuation, State) ->
    ?DEBL("~s",["create!\n"]),
    {#fuse_reply_err{err=enotsup},State}.

flush(_Ctx,_Inode,_Fuse_File_Info,_Continuation,State) ->
    ?DEBL("~s",["flush!\n"]),
    {#fuse_reply_err{err=enotsup},State}.

forget(_Ctx,_Inode,_Nlookup,_Continuation,State) ->
    ?DEB2(">forget ~p~n",_Inode),
    {#fuse_reply_none{},State}.

fsync(_Ctx,_Inode,_IsDataSync,_Fuse_File_Info,_Continuation,State) ->
    ?DEBL("~s",["fsync!\n"]),
    {#fuse_reply_err{err=enotsup},State}.

fsyncdir(_Ctx,_Inode,_IsDataSync,_Fuse_File_Info,_Continuation,State) ->
    ?DEBL("~s",["fsyncdir!\n"]),
    {#fuse_reply_err{err=enotsup},State}.
    
getattr(#fuse_ctx{uid=_Uid,gid=_Gid,pid=_Pid},Inode,Continuation,State) ->
    ?DEB2(">getattr(~p)~n",Inode),
    spawn(fun() -> getattr_internal(Inode,Continuation,State) end),
    {noreply,State}.

%getattr_internal(_Ctx,Inode,Continuation,State) ->

getattr_internal(Inode,Continuation,State) ->
    ?DEB2("getattr_internal(~p)~n",Inode),

    case lookup_inode_entry(Inode,State) of
        none ->
            ?DEB1("non-existent file"),
            Reply=#fuse_reply_err{err=enoent};
        {value,Entry} ->
            ?DEB1("File exists, converting info"),
                Reply=#fuse_reply_attr{
                    attr=statify_internal_file_info(Entry),
                    attr_timeout_ms=5
                };
            _A -> 
                ?DEB2("This should not be happening: ~p~n",_A),
                Reply=#fuse_reply_err{err=enotsup}
    end,
    ?DEB1("Sending reply"),
    fuserlsrv:reply(Continuation,Reply).

test(Dir,Inode) ->
    {ok,State}=init(#initargs{dir=Dir}),
    getattr(#fuse_ctx{},Inode,continuation,State).


getlk(_Ctx,_Inode,_Fuse_File_Info,_Lock,_Continuation,State) ->
    ?DEBL("~s",["getlk!\n"]),
    {#fuse_reply_err{err=enotsup},State}.

%% TODO: Maybe this will be how one can view which attributes a file is sorted under?
%% Get the value of an extended attribute. If Size is zero, the size of the value should be sent with #fuse_reply_xattr{}. If Size is non-zero, and the value fits in the buffer, the value should be sent with #fuse_reply_buf{}. If Size is too small for the value, the erange error should be sent. If noreply is used, eventually fuserlsrv:reply/2  should be called with Cont as first argument and the second argument of type getxattr_async_reply ().
getxattr(_Ctx,_Inode,_Name,_Size,_Continuation,State) ->
    ?DEBL("~s",["getxattr!\n"]),
    {#fuse_reply_err{err=enotsup},State}.
    
link(_Ctx,_Inode,_NewParent,_NewName,_Continuation,State) ->
    ?DEBL("~s",["link!\n"]),
    {#fuse_reply_err{err=enotsup},State}.
    
%%List extended attribute names. If Size is zero, the total size in bytes of the attribute name list (including null terminators) should be sent via #fuse_reply_xattr{}. If the Size is non-zero, and the null character separated and terminated attribute list is Size or less, the list should be sent with #fuse_reply_buf{}. If Size is too small for the value, the erange error should be sent.
listxattr(_Ctx,Inode,Size,_Continuation,State) ->
    ?DEB2("listxattr(~p)~n",Inode),
    {value,Entry}=lookup_inode_entry(Inode,State),
    ?DEB1("   got inode number."),
    {ExtSize,ExtAttribs}=Entry#inode_entry.ext_io,
    ?DEB1("   got attribute and size"),
    Reply=case Size == 0 of 
        true -> ?DEB1("   they want to know our size."),#fuse_reply_xattr{count=ExtSize};
        false -> 
            case Size < ExtSize of
                true -> ?DEBL("   they are using a too small buffer; ~p < ~p ~n",[Size,ExtSize]),#fuse_reply_err{err=erange};
                false -> ?DEB1("   all is well, replying with attribs."),#fuse_reply_buf{buf=list_to_binary(ExtAttribs), size=ExtSize}
            end
    end,
    {Reply,State}.




    
lookup(_Ctx,ParentInode,BinaryChild,_Continuation,State) ->
    Child=binary_to_list(BinaryChild),
    ?DEBL(">~s Parent: ~p Name: ~p\n",["lookup!",ParentInode,Child]),
    case lookup_children(ParentInode,State) of
        {value,Children} ->
            ?DEB2("   Got children for ~p~n",ParentInode),
            case find_child(Child,Children) of
                {value,{_Name,Inode}} ->
                    ?DEB2("   Found child ~p~n",Child),
                    {value,Entry} = lookup_inode_entry(Inode,State),
                    ?DEB1("   Got child inode entry"),
                    Stat=statify_internal_file_info(Entry),
                    ?DEB1("   Made a stat of the inode entry, returning"),
                    Param=#fuse_entry_param{
                        ino=Inode,
                        generation=1, %for now.
                        attr=Stat,
                        attr_timeout_ms=1000,
                        entry_timeout_ms=1000
                            },
                    {#fuse_reply_entry{fuse_entry_param=Param},State};
                none ->{#fuse_reply_err{err=enoent},State} % child nonexistent.
            end;
        none -> {#fuse_reply_err{err=enoent},State} %no parent
    end.

find_child(_,[]) -> false;

find_child(Name,[Child={ChildName,_Inode}|Children]) ->
    case Name == ChildName of
        true -> {value,Child};
        false -> find_child(Name,Children)
    end.


mkdir(_Ctx,_ParentInode,_Name,_Mode,_Continuation,State) ->
    ?DEBL("~s",["mkdir!\n"]),
    {#fuse_reply_err{err=enotsup},State}.

mknod(_Ctx,_ParentInode,_Name,_Mode,_Dev,_Continuation,State) ->
    ?DEBL("~s",["mknod!\n"]),
    {#fuse_reply_err{err=enotsup},State}.

open(_Ctx,_Inode,_Fuse_File_Info,_Continuation,State) ->
    ?DEBL("~s",["open!\n"]),
    {#fuse_reply_err{err=enotsup},State}.

opendir(_Ctx,Inode,_FI=#fuse_file_info{flags=_Flags,writepage=_Writepage,direct_io=_DirectIO,keep_cache=_KeepCache,flush=_Flush,fh=_Fh,lock_owner=_LockOwner},_Continuation,State) ->
    ?DEB1(">opendir"),
    ?DEB2("   flags ~p~n",_Flags),
    ?DEB2("   writepage ~p~n",_Writepage),
    ?DEB2("   DirectIO ~p~n",_DirectIO),
    ?DEB2("   KeepCache ~p~n",_KeepCache),
    ?DEB2("   FileHandle ~p~n",_Fh),
    ?DEB2("  Getting inode entries for ~p ~n",Inode),
    Entries=get_inode_entries(State),
    ?DEB1("  Creating directory entries from inode entries"),
    % TODO: What to do if I get several opendir calls (from the same context?) while the dir is updating?
    NewState=set_open_file(State,Inode,direntries(Inode,Entries)),
    {#fuse_reply_open{ fuse_file_info = _FI }, NewState}.




read(_Ctx,_Inode,_Size,_Offset,_Fuse_File_Info,_Continuation,State) ->
    ?DEBL("~s",["read!\n"]),
    {#fuse_reply_err{err=enotsup},State}.

%these two I stole from fuserlproc. Maybe they'll come in handy.
-define (DIRATTR (X), #stat{ st_ino = (X), 
                             st_mode = ?S_IFDIR bor 8#0555, 
                             st_nlink = 1 }).


-define(STAT (X,Y,Z), #stat{ st_ino = (X),
                             st_mode = (Y),
                             st_nlink = Z
                                 }.

readdir(_Ctx,Inode,Size,Offset,_Fuse_File_Info,_Continuation,State) ->
  ?DEBL(">~s inode:~p offset:~p~n",["readdir",Inode,Offset]),
  case get_open_file(State,Inode) of 
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
          { #fuse_reply_direntrylist{ direntrylist = DirEntryList }, State };
    none ->
        {#fuse_reply_err{err=ebadr},State} % XXX: What should this REALLY return when the file is not open for this user?
  end.

%% TODO: rebuild this with lookup_children?
direntries(Inode,InodeList) ->
    ?DEB1("Creating direntries"),
    ?DEB1("Getting entries"),
    InodeEntry=gb_trees:get(Inode,InodeList),
    ?DEB1("Getting children"),
    Children=InodeEntry#inode_entry.children,
    ?DEB1("Converting children"),
    direntrify(Children,InodeList).

direntrify([],_) -> 
    ?DEB1("Done converting children"),
    [];

direntrify([{Name,Inode}|Children],InodeList) ->
    ?DEB2("Getting inode for child ~p~n",{Name,Inode}),
    Child=gb_trees:get(Inode,InodeList),
    ?DEB2("Getting permissions for child ~p~n",{Name,Inode}),
    ChildPerms=(Child#inode_entry.internal_file_info)#file_info.mode,
    ?DEB2("Creating direntry for child ~p~n",{Name,Inode}),
    Direntry=
        #direntry{name=Name
                  ,stat=#stat{ st_ino=Inode,
                               st_mode=ChildPerms,
                               st_nlink=1 % For now. TODO: Make this mirror how many directories the file is in? Maybe just count the number of attributes and add one?
                              }
                    },
    ?DEB2("Calculatig size for direntry for child ~p~n",{Name,Inode}),
    Direntry1=Direntry#direntry{offset=fuserlsrv:dirent_size(Direntry)},
    ?DEB2("Appending child ~p to list~n",{Name,Inode}),
    [Direntry1|direntrify(Children,InodeList)].
               





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
    ?DEBL("~s",["readlink!\n"]),
    {#fuse_reply_err{err=enotsup},State}.
    
release(_Ctx,_Inode,_Fuse_File_Info,_Continuation,State) ->
    ?DEBL("~s",["release!\n"]),
    {#fuse_reply_err{err=enotsup},State}.

releasedir(_Ctx,_Inode,_Fuse_File_Info,_Continuation,State) ->
    ?DEBL(">~s~n",["releasedir"]),
    % TODO: Release dir info from open files here. Make sure no other process tries to get to the same info etc.
    {#fuse_reply_err{err=ok},State}.

removexattr(_Ctx,_Inode,_Name,_Continuation,State) ->
    ?DEBL("~s",["removexattr!\n"]),
    {#fuse_reply_err{err=enotsup},State}.

rename(_Ctx,_Parent,_Name,_NewParent,_NewName,_Continuation,State) ->
    ?DEBL("~s",["rename!\n"]),
    {#fuse_reply_err{err=enotsup},State}.

rmdir(_CTx,_Inode,_Name,_Continuation,State) ->
    ?DEBL("~s",["rmdir!\n"]),
    {#fuse_reply_err{err=enotsup},State}.

setattr(_Ctx,_Inode,_Attr,_ToSet,_Fuse_File_Info,_Continuation,State) ->
    ?DEBL("~s",["setattr!\n"]),
    {#fuse_reply_err{err=enotsup},State}.

setlk(_Ctx,_Inode,_Fuse_File_Info,_Lock,_Sleep,_Continuation,State) ->
    ?DEBL("~s",["setlk!\n"]),
    {#fuse_reply_err{err=enotsup},State}.

%% Set file attributes. #fuse_reply_err{err = ok} indicates success. Flags is a bitmask consisting of ?XATTR_XXXXX macros portably defined in fuserl.hrl . If noreply is used, eventually fuserlsrv:reply/2  should be called with Cont as first argument and the second argument of type setxattr_async_reply ().

setxattr(_Ctx,Inode,BName,BValue,_Flags,_Continuation,State) ->
    ?DEBL("setxattr inode:~p name:~p value:~p flags: ~p~n",[Inode,BName,BValue,_Flags]),
    ?DEB1("   getting inode entry"),
    {value,Entry} = lookup_inode_entry(Inode,State),
    ?DEB1("   transforming input data"),
    Name=binary_to_list(BName),
    Value=binary_to_list(BValue),
    case Entry#inode_entry.type of
        #external_file{path=Path} ->
            ?DEBL("   adding attribute {~p,~p} for file ~p to database~n",[Name,Value,Path]),
            add_new_attribute(Path,{Name,Value}),
            ?DEB1("   generating ext io and info"),
            NewExtIo=generate_ext_io(Path),
            NewExtInfo=generate_ext_info(Path),
            ?DEB1("   creating new inode entry"),
            NewEntry=Entry#inode_entry{ext_io=NewExtIo,ext_info=NewExtInfo},
            Entries=get_inode_entries(State),
            NewEntries=gb_trees:enter(Inode,NewEntry,Entries),
            InodeList=State#state.inode_list,
            NewInodeList=InodeList#inode_list{inode_entries=NewEntries},
            NewState=State#state{inode_list=NewInodeList},
            {#fuse_reply_err{err=ok},NewState};
        _ ->
            ?DEB1("   entry not an external file, skipping..."),
            {#fuse_reply_err{err=enotsup},State}
    end.


%% Later on, this function will not only insert the attribute in the database, but add the file to the corresponding attribute folders as well.
add_new_attribute(Path,{Name,Value}) ->
    dets:insert(?ATTR_DB,{Path,{Name,Value}}).


generate_ext_info(Path) ->
    ExtInfo0=dets:match(?ATTR_DB,{Path,'$1'}), 
    lists:flatten(ExtInfo0). % Going from a list of lists of tuples to a list of tuples.

generate_ext_io(Path) ->
    ExtInfo=generate_ext_info(Path),
    ext_info_to_ext_io(ExtInfo).

ext_info_to_ext_io(InternalExtInfoTupleList) ->
    ?DEB1(">ext_info_to_ext_io"),
    ext_info_to_ext_io(InternalExtInfoTupleList,{0,[]}).


ext_info_to_ext_io([],{A,B}) -> {A+1,B++"\0"};
ext_info_to_ext_io([{Name,_}|InternalExtInfoTupleList],{N,String}) ->
    ?DEB1("   Adding zero to end of name"),
    Name0=Name++"\0",
    ?DEB1("   Appending name to namelist"),
    NewName=String++Name,
    ?DEB1("   Updating size"),
    NewN=N+length(Name0),
    ?DEB1("   Recursion"),
    ext_info_to_ext_io(InternalExtInfoTupleList,{NewN,NewName}).




statfs(_Ctx,_Inode,_Continuation,State) ->
    ?DEBL("~s",["statfs!\n"]),
    {#fuse_reply_err{err=enotsup},State}.

symlink(_Ctx,_Link,_Inode,_Name,_Continuation,State) ->
    ?DEBL("~s",["symlink!\n"]),
    {#fuse_reply_err{err=enotsup},State}.

terminate(_Reason,_State) ->
    ?DEBL("~s ~p\n",["terminate!",_Reason]),
    ?DEB2("Closing database ~p~n",?ATTR_DB),
    dets:close(?ATTR_DB),
    exit(3).

unlink(_Ctx,_Inode,_Name,_Cont,State) ->
    ?DEBL("~s\n",["unlink!"]),
    {#fuse_reply_err{err=enotsup},State}.

write(_Ctx,_Inode,_Data,_Offset,_Fuse_File_Info,_Continuation,State) ->
    ?DEBL("~s~n",["write!"]),
    {#fuse_reply_err{err=enotsup},State}.

    

handle_info(_Msg,State) ->
    ?DEBL(">handle_info(~p)",[_Msg]),
    {noreply,State}.

code_change(_,_,_) -> %XXX: Maybe do something more intelligent with this?
    ?DEBL("~s",["code_change!"]),
    ok.


