-module(attrfs).

-behaviour(application).

-export([start/2,stop/1]).
-include("../include/debug.hrl").

start(_dont,_care) ->
    {ok,DirFrom}=application:get_env(?MODULE,from_dir),
    {ok,DirTo}=application:get_env(?MODULE,to_dir),
    {ok,DB}=application:get_env(?MODULE,attributes_db),
    ?DEBL("Starting ~p mirroring from ~p to ~p using database ~p",[?MODULE,DirFrom,DirTo,DB]),
    attrfs_sup:start_link(DirFrom,DirTo,DB).

stop(_State) -> ok.

