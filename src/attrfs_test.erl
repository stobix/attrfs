-module(attrfs_test).

-export([start/0,
         process/0,
         stop/1]).

start() ->
    io:format("Starting test server~n"),
    register(test,spawn_link(attrfs_test,process,[])).

process() ->
    io:format("Test server process spawned~n"),
    application:start(attrfs),
    receive
        die ->
            io:format("Recieved shutdown request, terminating test server~n"),
            application:stop(attrfs),
            init:stop()
    after 50000 ->
        application:stop(attrfs),
        io:format("Timeout received, terminating test server~n"),
        init:stop()
    end.

stop([Node|_]) ->
    io:format("Shutting down test server ~p~n",[Node]),
    {test,list_to_atom(Node)}!die,
    init:stop().
