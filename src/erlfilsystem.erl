-module(erlfilsystem).

-export([start_link/1,start_link/2,start_link/3,start_link/4]).
-export([access/5]).
-export([code_change/3]). % TODO: Do something intelligent with this one. Returns "ok" now.
-behaviour(fuserl).
-include_lib("fuserl/include/fuserl.hrl").
-include("../include/erlfilsystem.hrl").


%% a small record to make it possible for me to choose which start options I want to give
%% Only used here, only used as a test, only used temporary.
-record(startopts,{
    linkedin,%=true,
    mountopts,%="",
    dir}).

start_link(Startopts=#startopts{}) ->
    start_link(dir=Startopts#startopts.dir,Startopts#startopts.linkedin,Startopts#startopts.mountopts);

start_link(Dir) ->
    start_link(Dir,true).

start_link(Dir,LinkedIn) ->
    start_link(Dir,LinkedIn,"").

start_link(Dir,LinkedIn,MountOpts) ->
    start_link(Dir,LinkedIn,MountOpts,"").

start_link(Dir,LinkedIn,MountOpts,Args) ->
    fuserlsrv:start_link(?MODULE,LinkedIn,MountOpts,Dir,Args,"").

     
access(_Ctx,_Inode,_Mask,_Continuation,_State) ->
    % So, if I've gotten this right, I've got an inode to look up rights for, a context in Ctx which tells me who wanted to know their rights to the file, a mask, which does SOMETHING - aybe this is what is used on file systems who has no rights normally? - a Continuation, which is a magical item which somehow is used to make asyncronuous calls - more on this when I find a documentation for the record - and finally the state, which, afaik, I will NOT be changing by checking access to files.
    % First, lets try to just create an "access denied" version of this thing.
    #fuse_reply_err{err=denied} % like this?
        .
code_change(_,_,_) -> %XXX: Maybe do something more intelligent with this?
    ok.


