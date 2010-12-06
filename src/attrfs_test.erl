-module(attrfs_test).

-export([start/0,
         process/0,
         stop/1]).

start() ->
    io:format("Starting test server"),
    register(test,spawn(attrfs_test,process,[])).

process() ->
    application:start(attrfs),
    receive
        die ->
            application:stop(attrfs),
            init:stop()
    after 50000 ->
        application:stop(attrfs),
        io:format("Timeout received, terminating test server"),
        init:stop()
    end.

stop([Node|_]) ->
    io:format("Shutting down test server"),
    {test,list_to_atom(Node)}!die,
    init:stop().
