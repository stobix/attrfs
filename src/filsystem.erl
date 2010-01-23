-module(filsystem).
-behaviour(gen_server).
-include("../include/filsystem.hrl").

%-export(start_link/0, start_link/1).
-export([start/0, start/1]).
-export([start_link/0, start_link/1]).
-export([init/1,handle_call/3,handle_cast/2]).
-export([handle_info/2,terminate/2,code_change/3]).
-export([file_info/1,touch/1]).
-export([touch/2]).
-export([remove/1]).
-export([dir_info/1]).
-export([stop/0, dump_fs/0]).
-export([test/0]).
-export([save_fs/1,load_fs/1]).

-export([recursive_add/2]).

-record(state,{fs,itable}).
%%% I use a decursive dict hierarchy to represent a file system in here. 
%%%   Each file/folder is in itself a dict.
%%% The heap "feature" of dicts is not used.
%%%   In fact, I try to make sure that each dict entry only has one value, 
%%%     which is a dict either detoning a branch or a leaf. If there is any 
%%%     point in doing so, I might use the heaps later if dicts are overkill 
%%%     for certain tasks (Maybe specific file info as heaps, and directories 
%%%     as dicts?)
%%% Update: Well, it seems logical also to have a table of inode numbers, so I include one in the state.

start() ->
    start(new_file(root)). %XXX: fixa en #init eller nåt för att fixa till detta snyggare?
%    start(#state{fs=new_file(root)}).
start(FS) ->
    gen_server:start({local,me},?MODULE,#state{fs=FS},[]).

start_link()->
    start_link(#state{fs=new_file(root)}).

start_link("")->
    start_link();

start_link(#state{fs=_} = State) ->
    gen_server:start_link({local,me},?MODULE,State,[]);

start_link(FS) ->
    gen_server:start_link({local,me},?MODULE,#state{fs=FS},[]).

stop() ->
    gen_server:cast(me,stop).

dump_fs() ->
    gen_server:call(me,dump_fs).

save_fs(Name) ->
    gen_server:cast(me,{save_fs,Name}).

load_fs(Name) ->
    gen_server:cast(me,{load_fs,Name}).

init(State) ->
    {ok, State}.

%% @spec file_info(Path) -> {ok, FileInfo} | Error
%%    Path = [File]
%%    File = string()
%%    FileInfo = dictionary()
%%    Error = {error, not_found} | {error, empty_path}
%% @doc Returns the info associated with the file/folder in path Path
%% @end

file_info(Path) ->
    case gen_server:call(me,{lookup,Path}) of
        {ok, Info} -> 
            dict:to_list(second(dict:find(nodeInfo,Info)));
        E -> E
            end.

second({_A,B}) -> 
    B.

%% @spec dir_info(Path) -> {ok, DirList}| Error
%%      
%%    Path = [File]
%%    File = string()
%%    DirList = [File]
%%    Error = {error, not found}

dir_info(Path) ->
    case gen_server:call(me,{lookup,Path}) of
        {ok, Info} ->
            [C||A=C <- dict:fetch_keys(Info), is_list(A)];
        E -> E
            end.

%% @spec touch (Path) -> ok.
%%  Path = [File]
%%  File = String
%% @doc Adds Path if unexistent. Otherwise, doesn't do anything.
%% @end

touch(Path) ->
    gen_server:cast(me,{create,Path}).

%% @spec touch (Path,{Key,Value}) -> ok.
%%  Path = [File]
%%  File = string()
%%  Key = atom()
%%  Value = string()
%% @doc Adds Path if unexistent. 
%%  Adds the attribute {Key,Value} to the list of attributes for the last item
%% in the path. This attribute list is retrieveable by file_info/1. 
%% Recplaces the attribute if existent.
%% @end


% TODO: implement!
touch(Path,Value={_Key,_Path}) ->
    gen_server:cast(me,{update_create,Path,Value}).


remove(Path) ->
    gen_server:cast(me,{destroy,Path}).


handle_call({lookup,Path},_Caller,#state{fs=FS}=State) ->
    ReturnVal=recursive_lookup(Path,FS),
    {reply, ReturnVal, State};

handle_call(dump_fs,_Caller,#state{fs=FS}=State) ->
    {reply, FS, State};

handle_call(_,_,State) ->
    {reply, {error, not_implemented, State}, State}.

recursive_lookup("",Dict) ->
    {ok, Dict};

recursive_lookup([Name|Names],Dict) ->
    case dict:find(Name,Dict) of
        {ok, NextDir} ->
            recursive_lookup(Names, NextDir);
        error -> {error, not_found}
            end.

handle_cast({create,Path},#state{fs=FS}=State) ->
    NewFS=recursive_add(Path,FS),
    {noreply,State#state{fs=NewFS}};

handle_cast({destroy,Path},#state{fs=FS}=State) ->
    NewFS=case recursive_remove_check(Path,FS) of
        {ok,NewFSFS} -> NewFSFS;
        {error,_} -> ?DEB1("Could not remove: file not found!"),
        FS
            end,
            {noreply,State#state{fs=NewFS}};

handle_cast({save_fs,File},#state{fs=FS}=State) ->
    case file:open(File,write) of 
        {ok, T} -> 
            io:format(T,"~w.",FS),
            ?DEB1("File written"),
            file:close(T);
        _E -> ?DEB2("File not written: ~w~n",[_E])
            end,
            {noreply,State};

handle_cast({load_fs,File},#state{fs=FS}=State) ->
    {noreply,State#state{fs=case file:consult(File) of
        {ok,[NewFS|_]} -> ?DEB2("File system loaded! ~W~n",[NewFS,5]),
        NewFS;
    _E -> ?DEB2("File could not be opened! ~w",[_E]),
    FS
        end}};


handle_cast(stop,State) ->
    {stop, normal, State};

handle_cast(_,State) ->
    {noreply,State}.


recursive_add("",FS) ->
    FS;

recursive_add([File|Files]=Path, FS) ->
    ?DEB2("Adding ~p to ~p ~n",[File, FS]),% to ~p~n", [File, root_dir(FS)]),
    dict:update(File, fun(Dict) -> recursive_add(Files, Dict) end,
                new_FS(Path), FS).

%root_dir([F|_FS]) ->
%    dict:find(name,second(dict:find(nodeInfo,F))).

test() ->
    %start(),
    dir_info(""),
    file_info(""),
    touch(["a","b","c"]),
    dir_info(""),
    file_info(""),
    remove(["a"]),
    dir_info(""),
    file_info(""),
    stop().


%TODO: Kolla så filen finns innan borttagning!

recursive_remove_check(Files,FS) ->
    case recursive_lookup(Files,FS) of
        {ok,_} -> {ok, recursive_remove(Files,FS)};
        E -> E
    end.

recursive_remove([File|[]],FS) ->
    dict:erase(File,FS);

recursive_remove([File|Files],FS) ->
    dict:update(File, fun(Dict) -> recursive_remove(Files, Dict) end, FS).

new_FS([Name|Names]) ->
    ?DEB2("<new_FS fs=~p>~n",[Name]),
    NF=new_file(Name),
    recursive_add(Names,NF).
%    io:format("</new_FS>~n",[Name]).
    

new_file(FileName) ->
    ?DEB2("<new_file file=~p>~n",[FileName]),
    FileDict = dict:new(),
    %TODO: Set correct file attributes and such...
    Attrs= [
        {name, FileName},
        {type, regular}, 
        {inode, 12345},
        % {type, {real_file, <real_path>}}
        {contents, ""},
        {attributes, [dir,7,5,5]}%,
        ],
    dict:update(nodeInfo,fun(A) -> A end, dict:from_list(Attrs),FileDict).

handle_info(_Info,FS) ->
    ?DEB2("Got myself a ~p~n", [_Info]),
    {noreply,FS}.

terminate(_Why, _FS) ->
    ?DEB2("Died because of ~p~n", [_Why]).


code_change(_,_,_) ->
    ?DEB1("NO CAN HAZ!"),
    gen_server:cast(me, stop).
