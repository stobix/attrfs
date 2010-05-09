-module(attrfs).

-behaviour(application).

-export([start/2,stop/1]).
-include("../include/debug.hrl").

start(_dont,_care) ->
    ?DEB2("Starting ~p",?MODULE),
    ?DEB1("  getting arguments"),
    {ok,DirFrom}=application:get_env(?MODULE,from_dir),
    ?DEB2("  dir_from: ~p",DirFrom),
    {ok,DirTo}=application:get_env(?MODULE,to_dir),
    ?DEB2("  dir_to: ~p",DirTo),
    {ok,DB}=application:get_env(?MODULE,attributes_db),
    ?DEB2("  database: ~p",DB),
    ?DEBL("Starting ~p mirroring from ~p to ~p using database ~p",[?MODULE,DirFrom,DirTo,DB]),
    attrfs_sup:start_link(DirFrom,DirTo,DB).

stop(_State) -> ok.

