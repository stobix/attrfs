-module(attrfs).

-behaviour(application).

-export([start/2,stop/1]).
-include("../include/debug.hrl").

start(_dont,_care) ->
    {ok,DirFrom}=application:get_env(?MODULE,from_dir),
    {ok,DirTo}=application:get_env(?MODULE,to_dir),
    ?DEBL("Starting ~p mirroring from ~p to ~p",[?MODULE,DirFrom,DirTo]),
    attrfs_sup:start_link(DirFrom,DirTo).

stop(_State) -> ok.

