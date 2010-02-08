-module(ports).
-behaviour(gen_server).

-export([start_link/1]).

start_link(ProgName) ->
    gen_server:start_link({local,?MODULE},echo,ProgName,[]).

init(ProgName) ->
    process_flag(trap_exit,true),

