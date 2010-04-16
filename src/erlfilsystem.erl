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
    
    ?DEBL("   opening attribute database file ~p as ~p", [?ATTR_DB_FILE, ?ATTR_DB]),
    {ok,_}=dets:open_file(?ATTR_DB,[{type,bag},{file,?ATTR_DB_FILE}]),
    ?DEB2("   mirroring dir ~p",MirrorDir),
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
    ?DEB2("   reading file info for ~p:",Entry),
    {ok,FileInfo}=file:read_file_info(Entry),
    {ok,Children,Type,NewIno} = case FileInfo#file_info.type of
        directory ->
            ?DEB1("    directory"),
            {ok,Names} = file:list_dir(Entry),
            ?DEB2("     directory entries:~p",Names),
            {NameInodePairs,MyIno}=
                %make entries of type {{ExternalName,InternalName},Ino}
                lists:mapfoldl(
                    fun(Name,Ino) -> {{Name,Ino},Ino+1} end, 
                    %fun(Name,Ino) -> {Entry++"/"++Name,Ino},Ino+1} end, 
                    NextIno, Names),
            {ok,NameInodePairs,#external_dir{external_file_info=FileInfo,path=Entry},MyIno};
        regular ->
            ?DEB1("    regular"),
            {ok,[],#external_file{external_file_info=FileInfo,path=Entry},NextIno};
                _ ->
            {error,not_supported} % for now
    end,
    ExtInfo=generate_ext_info(Entry),
    ?DEB2("     ext info: ~p", ExtInfo),
    ExtIo=ext_info_to_ext_io(ExtInfo),
    ?DEB1("    Generating entry"),
    % XXX: This will break if provided with a date and time that does not
    % occur.
    EpochAtime=lists:nth(1,calendar:local_time_to_universal_time_dst(FileInfo#file_info.atime)),
    EpochCtime=lists:nth(1,calendar:local_time_to_universal_time_dst(FileInfo#file_info.ctime)),
    EpochMtime=lists:nth(1,calendar:local_time_to_universal_time_dst(FileInfo#file_info.mtime)),
        
    ?DEBL("\tatime:~p~n\t\t\tctime:~p~n\t\t\tmtime:~p",[EpochAtime,EpochCtime,EpochMtime]),
    InodeEntry=#inode_entry{ 
             children=Children
            ,type=Type
            ,internal_file_info=statify_file_info(
                FileInfo#file_info{
                    inode=InitialIno
                   ,atime=EpochAtime
                   ,ctime=EpochCtime
                   ,mtime=EpochMtime
                       })
            ,ext_info=ExtInfo
            ,ext_io=ExtIo},
    {ChildInodeEntries,FinalIno} = 
        lists:mapfoldl
            (
                fun({A,AA},B)->make_inode_list({Entry++"/"++A,AA},B) end
                ,NewIno
                ,Children
            ),
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


%TODO: Find out what info it is I cannot provid here lest fuserl hangs.
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

     
datetime_to_epoch(DateTime) ->
    calendar:datetime_to_gregorian_seconds(DateTime) - calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}).


access(_Ctx,_Inode,_Mask,_Continuation,State) ->
    ?DEBL("~s I: ~p M: ~p",["access!",_Inode,_Mask]),
    % First, lets try to just create an "access denied" version of this thing.
    {#fuse_reply_err{err=enotsup},State}. 

create(_Ctx,_Parent,_Name,_Mode,_Fuse_File_Info,_Continuation, State) ->
    ?DEBL("~s",["create!"]),
    {#fuse_reply_err{err=enotsup},State}.

flush(_Ctx,_Inode,_Fuse_File_Info,_Continuation,State) ->
    ?DEBL("~s",["flush!"]),
    {#fuse_reply_err{err=enotsup},State}.

forget(_Ctx,_Inode,_Nlookup,_Continuation,State) ->
    ?DEB2(">forget ~p",_Inode),
    {#fuse_reply_none{},State}.

fsync(_Ctx,_Inode,_IsDataSync,_Fuse_File_Info,_Continuation,State) ->
    ?DEBL("~s",["fsync!"]),
    {#fuse_reply_err{err=enotsup},State}.

fsyncdir(_Ctx,_Inode,_IsDataSync,_Fuse_File_Info,_Continuation,State) ->
    ?DEBL("~s",["fsyncdir!"]),
    {#fuse_reply_err{err=enotsup},State}.
    
getattr(#fuse_ctx{uid=_Uid,gid=_Gid,pid=_Pid},Inode,Continuation,State) ->
    ?DEBL(">getattr inode:~p",[Inode]),
    spawn(fun() -> getattr_internal(Inode,Continuation,State) end),
    {noreply,State}.

%getattr_internal(_Ctx,Inode,Continuation,State) ->

getattr_internal(Inode,Continuation,State) ->
    ?DEB2("getattr_internal(~p)",Inode),
    case lookup_inode_entry(Inode,State) of
        none ->
            ?DEB1("non-existent file"),
            Reply=#fuse_reply_err{err=enoent};
        {value,Entry} ->
            ?DEB1("File exists, returning info"),
                Reply=#fuse_reply_attr{
                    attr=Entry#inode_entry.internal_file_info,
                    attr_timeout_ms=5
                };
            _A -> 
                ?DEB2("This should not be happening: ~p",_A),
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

%% TODO: Handle the case where the OS wants some strange value which isn't implemented, say the value for system.posix_acl_access
%% Get the value of an extended attribute. If Size is zero, the size of the value should be sent with #fuse_reply_xattr{}. If Size is non-zero, and the value fits in the buffer, the value should be sent with #fuse_reply_buf{}. If Size is too small for the value, the erange error should be sent. If noreply is used, eventually fuserlsrv:reply/2  should be called with Cont as first argument and the second argument of type getxattr_async_reply ().
getxattr(_Ctx,Inode,Name,Size,_Continuation,State) ->
    ?DEBL(">getxattr, name:~p, size:~p, inode:~p)",[Name,Size,Inode]),
    {value,Entry}=lookup_inode_entry(Inode,State),
    ?DEB1("   Got inode entry"),
    ExtInfo=Entry#inode_entry.ext_info,
    ?DEB1("   Got extinfo"),
    Reply=case lookup_tuple(binary_to_list(Name),ExtInfo) of
        {value,ExtInfoValue} ->
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
        none ->
            ?DEB1("   Argument nonexistant, returning error"),
            #fuse_reply_err{err=enodata}
    end,
    {Reply,State}.
    
    
link(_Ctx,_Inode,_NewParent,_NewName,_Continuation,State) ->
    ?DEBL("~s",["link!"]),
    {#fuse_reply_err{err=enotsup},State}.
    
%%List extended attribute names. If Size is zero, the total size in bytes of the attribute name list (inCLUDING Null terminators) should be sent via #fuse_reply_xattr{}. If the Size is non-zero, and the null character separated and terminated attribute list is Size or less, the list should be sent with #fuse_reply_buf{}. If Size is too small for the value, the erange error should be sent.
listxattr(_Ctx,Inode,Size,_Continuation,State) ->
    ?DEB2("listxattr(~p)",Inode),
    {value,Entry}=lookup_inode_entry(Inode,State),
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




    
lookup(_Ctx,ParentInode,BinaryChild,_Continuation,State) ->
    Child=binary_to_list(BinaryChild),
    ?DEBL(">lookup Parent: ~p Name: ~p",[ParentInode,Child]),
    case lookup_children(ParentInode,State) of
        {value,Children} ->
            ?DEB2("   Got children for ~p",ParentInode),
            case lookup_tuple(Child,Children) of
                {value,Inode} ->
                    ?DEB2("   Found child ~p",Child),
                    {value,Entry} = lookup_inode_entry(Inode,State),
                    ?DEB1("   Got child inode entry"),
                    Stat=Entry#inode_entry.internal_file_info,
                    ?DEB1("   Got child inode entry stat file info, returning"),
                    Param=#fuse_entry_param{
                        ino=Inode,
                        generation=1, %for now.
                        attr=Stat,
                        attr_timeout_ms=1000,
                        entry_timeout_ms=1000
                            },
                    {#fuse_reply_entry{fuse_entry_param=Param},State};
                none ->
                    ?DEB1("   Child nonexistent!"),
                    {#fuse_reply_err{err=enoent},State} % child nonexistent.
            end;
        none -> 
            ?DEB1("   Parent nonexistent!"),
            {#fuse_reply_err{err=enoent},State} %no parent
    end.
%% lookup_tuple takes a key, and a [{Key,Value}] and returns {value,Value} or none
lookup_tuple(_,[]) -> none;

lookup_tuple(Name,[{ChildName,Inode}|Children]) ->
    case Name == ChildName of
        true -> {value,Inode};
        false -> lookup_tuple(Name,Children)
    end.


mkdir(_Ctx,_ParentInode,_Name,_Mode,_Continuation,State) ->
    ?DEBL("~s",["mkdir!"]),
    {#fuse_reply_err{err=enotsup},State}.

mknod(_Ctx,_ParentInode,_Name,_Mode,_Dev,_Continuation,State) ->
    ?DEBL("~s",["mknod!"]),
    {#fuse_reply_err{err=enotsup},State}.

open(_Ctx,_Inode,_Fuse_File_Info,_Continuation,State) ->
    ?DEBL("~s",["open!"]),
    {#fuse_reply_err{err=enotsup},State}.

opendir(_Ctx,Inode,_FI=#fuse_file_info{flags=_Flags,writepage=_Writepage,direct_io=_DirectIO,keep_cache=_KeepCache,flush=_Flush,fh=_Fh,lock_owner=_LockOwner},_Continuation,State) ->
    ?DEB1(">opendir"),
    ?DEB2("   flags ~p",_Flags),
    ?DEB2("   writepage ~p",_Writepage),
    ?DEB2("   DirectIO ~p",_DirectIO),
    ?DEB2("   KeepCache ~p",_KeepCache),
    ?DEB2("   FileHandle ~p",_Fh),
    ?DEB2("  Getting inode entries for ~p",Inode),
    Entries=get_inode_entries(State),
    ?DEB1("  Creating directory entries from inode entries"),
    % TODO: What to do if I get several opendir calls (from the same context?) while the dir is updating?
    NewState=set_open_file(State,Inode,direntries(Inode,Entries)),
    {#fuse_reply_open{ fuse_file_info = _FI }, NewState}.




read(_Ctx,_Inode,_Size,_Offset,_Fuse_File_Info,_Continuation,State) ->
    ?DEBL("~s",["read!"]),
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
  ?DEBL(">readdir inode:~p offset:~p",[Inode,Offset]),
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
    ?DEB1("    Creating direntries"),
    ?DEB1("     Getting entries"),
    InodeEntry=gb_trees:get(Inode,InodeList),
    ?DEB1("     Getting children"),
    Children=InodeEntry#inode_entry.children,
    ?DEB1("     Converting children"),
    direntrify(Children,InodeList).

direntrify([],_) -> 
    ?DEB1("    Done converting children"),
    [];

direntrify([{Name,Inode}|Children],InodeList) ->
    ?DEB2("    Getting inode for child ~p",{Name,Inode}),
    Child=gb_trees:get(Inode,InodeList),
    ?DEB2("    Getting permissions for child ~p",{Name,Inode}),
    ChildStats=Child#inode_entry.internal_file_info,
    ?DEB2("    Creating direntry for child ~p",{Name,Inode}),
    Direntry= #direntry{name=Name ,stat=ChildStats },
    ?DEB2("    Calculatig size for direntry for child ~p",{Name,Inode}),
    Direntry1=Direntry#direntry{offset=fuserlsrv:dirent_size(Direntry)},
    ?DEB2("    Appending child ~p to list",{Name,Inode}),
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
    ?DEBL("~s",["readlink!"]),
    {#fuse_reply_err{err=enotsup},State}.

release(_Ctx,_Inode,_Fuse_File_Info,_Continuation,State) ->
    ?DEBL("~s",["release!"]),
    {#fuse_reply_err{err=enotsup},State}.

%% TODO: Release dir info from open files in here. Make sure no other process tries to get to the same info etc.
releasedir(_Ctx,_Inode,_Fuse_File_Info,_Continuation,State) ->
    ?DEBL(">~s~n",["releasedir"]),
    {#fuse_reply_err{err=ok},State}.

%%Remove an extended attribute. #fuse_reply_err{err = ok} indicates success. If noreply is used, eventually fuserlsrv:reply/2  should be called with Cont as first argument and the second argument of type removexattr_async_reply ().
removexattr(_Ctx,Inode,Name,_Continuation,State) ->
    ?DEBL(">removexattr inode: ~p name: ~p",[Inode,Name]),
    {value,Entry} = lookup_inode_entry(Inode,State),
    case Entry#inode_entry.type of
        #external_file{ path=Path } ->
            case dets:match_delete(?ATTR_DB,{Path,{binary_to_list(Name),'_'}}) of
                ok -> 
                    % XXX: Getting here is not a guarantee that there was any deleted elements to start with.
                    ?DEB1("   Removed attribute, if any"),
                    {#fuse_reply_err{err=ok},State};
                {error, Error} ->
                    ?DEBL("   Error: ~p", [Error]),
                    {#fuse_reply_err{err=enoent},State}
            end;

        _ ->
            ?DEB1("   Non-existent inode"),
            {#fuse_reply_err{err=enoent},State}
    end.


rename(_Ctx,_Parent,_Name,_NewParent,_NewName,_Continuation,State) ->
    ?DEBL("~s",["rename!"]),
    {#fuse_reply_err{err=enotsup},State}.

rmdir(_CTx,_Inode,_Name,_Continuation,State) ->
    ?DEBL("~s",["rmdir!"]),
    {#fuse_reply_err{err=enotsup},State}.
%% Set file attributes. ToSet is a bitmask which defines which elements of Attr are defined and should be modified. Possible values are defined as ?FUSE_SET_ATTR_XXXX in fuserl.hrl . Fi will be set if setattr is invoked from ftruncate under Linux 2.6.15 or later. If noreply is used, eventually fuserlsrv:reply/2  should be called with Cont as first argument and the second argument of type setattr_async_reply ().
setattr(_Ctx,Inode,_Attr,_ToSet,_Fuse_File_Info,_Continuation,State) ->
    ?DEBL("setattr inode: ~p~n    attr: ~p~n    to_set: ~p~n    fuse_file_info: ~p",[Inode,_Attr,_ToSet,_Fuse_File_Info]),
%?FUSE_SET_ATTR_UID
%?FUSE_SET_ATTR_GID
    {value,Entry}=lookup_inode_entry(Inode,State),
    Stat=Entry#inode_entry.internal_file_info,
    NewStat=Stat#stat{
        st_atime=
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
    NewState=update_inode_entry(Inode,Entry#inode_entry{internal_file_info=NewStat},State),
    {#fuse_reply_attr{attr=NewStat,attr_timeout_ms=100000},NewState}.


update_inode_entry(Inode,Entry,State) ->
    ?DEB1("   updating state"),
    Entries=get_inode_entries(State),
    NewEntries=gb_trees:enter(Inode,Entry,Entries),
    InodeList=State#state.inode_list,
    NewInodeList=InodeList#inode_list{inode_entries=NewEntries},
    State#state{inode_list=NewInodeList}.


setlk(_Ctx,_Inode,_Fuse_File_Info,_Lock,_Sleep,_Continuation,State) ->
    ?DEBL("~s",["setlk!"]),
    {#fuse_reply_err{err=enotsup},State}.

%% Set file attributes. #fuse_reply_err{err = ok} indicates success. Flags is a bitmask consisting of ?XATTR_XXXXX macros portably defined in fuserl.hrl . If noreply is used, eventually fuserlsrv:reply/2  should be called with Cont as first argument and the second argument of type setxattr_async_reply ().

setxattr(_Ctx,Inode,BName,BValue,_Flags,_Continuation,State) ->
    ?DEBL("setxattr inode:~p name:~p value:~p flags: ~p",[Inode,BName,BValue,_Flags]),
    ?DEB1("   getting inode entry"),
    {value,Entry} = lookup_inode_entry(Inode,State),
    ?DEB1("   transforming input data"),
    Name=binary_to_list(BName),
    Value=binary_to_list(BValue),
    case Entry#inode_entry.type of
        #external_file{path=Path} ->
            ?DEBL("   adding attribute {~p,~p} for file ~p to database",[Name,Value,Path]),
            add_new_attribute(Path,{Name,Value}),
            ?DEB1("   generating ext io and info"),
            NewExtIo=generate_ext_io(Path),
            NewExtInfo=generate_ext_info(Path),
            ?DEB1("   creating new inode entry"),
            NewEntry=Entry#inode_entry{ext_io=NewExtIo,ext_info=NewExtInfo},
            NewState=update_inode_entry(Inode,NewEntry,State),
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
    ?DEB1("   Creating ext_io"),
    ext_info_to_ext_io(InternalExtInfoTupleList,[]).


ext_info_to_ext_io([],B) -> 
    B0=B++"\0",
    B0len=length(B0),
    ?DEB1("   Done creating ext_io"),
    ?DEBL("   Final string: \"~p\", size: ~p",[B0,B0len]),
    {B0len,B0};
ext_info_to_ext_io([{Name,_}|InternalExtInfoTupleList],String) ->
    ?DEB2("    Adding zero to end of name ~p",Name),
    Name0=Name++"\0",
    ?DEB1("    Appending name to namelist"),
    NewString=String++Name0,
    ?DEB1("    Recursion"),
    ext_info_to_ext_io(InternalExtInfoTupleList,NewString).




statfs(_Ctx,_Inode,_Continuation,State) ->
    ?DEBL("~s",["statfs!"]),
    {#fuse_reply_err{err=enotsup},State}.

symlink(_Ctx,_Link,_Inode,_Name,_Continuation,State) ->
    ?DEBL("~s",["symlink!"]),
    {#fuse_reply_err{err=enotsup},State}.

terminate(_Reason,_State) ->
    ?DEBL("~s ~p",["terminate!",_Reason]),
    ?DEB2("Closing database ~p",?ATTR_DB),
    dets:close(?ATTR_DB),
    exit(3).

unlink(_Ctx,_Inode,_Name,_Cont,State) ->
    ?DEBL("~s",["unlink!"]),
    {#fuse_reply_err{err=enotsup},State}.

write(_Ctx,_Inode,_Data,_Offset,_Fuse_File_Info,_Continuation,State) ->
    ?DEBL("~s",["write!"]),
    {#fuse_reply_err{err=enotsup},State}.

handle_info(_Msg,State) ->
    ?DEBL(">handle_info(~p)",[_Msg]),
    {noreply,State}.

code_change(_,_,_) -> %XXX: Maybe do something more intelligent with this?
    ?DEBL("~s",["code_change!"]),
    ok.


