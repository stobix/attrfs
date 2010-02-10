-module(xattr).
-behaviour(gen_server).

-export([start_link/1,
    init/1,
    handle_call/3,
    handle_info/2,
    terminate/2
    ]).

-export([get/2]).
-export([set/3]).
-export([touch/2]).
-export([append/3]).
-export([remove/2]).
-export([list/1]).
-export([list_get/1]).

-export([exit/0]). %Why do I need to export this to use it in terminate??

-export([flatten/1]).
-export([consult/1]).
-export([parse/1]).



-record(state,{port}).

start_link(ProgName) ->
    gen_server:start_link({local,?MODULE},?MODULE,ProgName,[]).

init(ProgName) ->
%    register(porten,self()),
    process_flag(trap_exit,true),
    io:format("Opening port...\n"),
    Porten = open_port({spawn_executable,ProgName},[stream,use_stdio]),
    {ok,#state{port=Porten}}.

handle_call({command,List},_from,State=#state{port=Port}) ->
    port_commands(Port,List),
    case port_response(Port) of 
        {ok, Val} -> {reply,flatten(Val),State};
        {timeout, _T} -> {stop, port_timeout, State}
    end;

handle_call(Other,_,State) ->
    {reply,{error,not_implemented,Other},State}.

port_commands(Port,[]) ->
    port_command(Port,"\n");

port_commands(Port,[Cmd|Cmds]) ->
    port_command(Port,Cmd),
    port_command(Port," "),
    port_commands(Port,Cmds).

        
    

flatten([]) ->
    [];

flatten([A|As]) ->
    A++flatten(As).


port_response(Port) ->
    port_response(Port,[]).

-define(TIMEOUT,10000).

port_response(Port,Reply) ->
    receive
        {Port, {data, List}} ->
            case is_dot_terminated(List) of
                true ->
                    {ok, lists:reverse([List|Reply])};
                false ->
                    port_response(Port,[List|Reply])
            end
                    
        after ?TIMEOUT ->
            {timeout, ?TIMEOUT}
    end.

handle_info({'EXIT', Port, Reason}, #state{port = Port} = State) ->
    {stop, {port_terminated, Reason}, State}.

terminate({port_terminated, _Reason}, _State) ->
    ok;

terminate(_Reason, #state{port = Port} = _State) ->
    io:format("closing port"),
    exit(),
    port_close(Port).

                   
is_dot_terminated([]) ->
    false;

is_dot_terminated(".") ->
    true;

is_dot_terminated([_A|As]) ->
    is_dot_terminated(As).

get(File, Attribute) ->
    ?MODULE:parse(gen_server:call(?MODULE,{command,["g",File,Attribute]})).

set(File,Attribute,Value) ->
    ?MODULE:parse(gen_server:call(?MODULE,{command,["s",File,Attribute,Value]})).

touch(File,Attribute) ->
    ?MODULE:parse(gen_server:call(?MODULE,{command,["t",File,Attribute]})).

append(File,Attribute,AppendValue) ->
    ?MODULE:parse(gen_server:call(?MODULE,{command,["s",File,Attribute,AppendValue]})).

remove(File,Attribute) ->
    ?MODULE:parse(gen_server:call(?MODULE,{command,["r",File,Attribute]})).

exit() ->
    ?MODULE:parse(gen_server:call(?MODULE,{command,["e"]})).

list(File) ->
    ?MODULE:parse(gen_server:call(?MODULE, {command, ["l",File]})).

list_get(File) ->
    ?MODULE:parse(gen_server:call(?MODULE, {command, ["L",File]})).


consult(ArgString) ->
    parse(?MODULE:get(ArgString)).


parse(List) ->
    binary_to_term(list_to_binary(List)).

%% unchomp does the opposite of the chomp perl function; if the string is not newline terminated, chomp appends a \n to the end of the string.
unchomp(String) ->
    unchomp(String,[]).

unchomp([],Gnirts) ->
    lists:reverse([$\n|Gnirts]);

unchomp("\n",Gnirts) ->
    lists:reverse([$\n|Gnirts]);

unchomp([Char|String],Gnirts) ->
    unchomp(String,[Char|Gnirts]).

