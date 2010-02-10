-module(xattr_app).

-behavior(application).

-export([start/2, 
         stop/1]).

start(_Type, _Args) ->
    PrivDir = code:priv_dir(?MODULE),
    {ok, ExtProg} = application:get_env(?MODULE, extprog),
    echo_sup:start_link(filename:join([PrivDir, ExtProg])).

stop(_State) ->
    ok.
        
