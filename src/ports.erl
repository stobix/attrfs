-module(ports).
-behaviour(gen_server).

-export([start_link/1,
    init/1,
    handle_call/3,
    handle_info/2,
    terminate/2
    ]).

-export([get/1]).
-export([parse/1]).


-record(state,{port}).

start_link(ProgName) ->
    gen_server:start_link({local,?MODULE},?MODULE,ProgName,[]).

init(ProgName) ->
%    register(porten,self()),
    process_flag(trap_exit,true),
    io:format("Opening port...\n"),
    Porten = open_port({spawn_executable,ProgName},[{line,500},use_stdio]),
    {ok,#state{port=Porten}}.



handle_call({command,CmdString},_from, State=#state{port=Port}) -> % Latorz: {command, Cmd, Args} / {cmd, [Args]}
    port_command(Port,CmdString),
    case port_response(Port) of 
        {ok, Val} -> {reply,parse(Val),State};%{reply, Val, State};
        {timeout, _T} -> {stop, port_timeout, State}
    end.

        
parse([[]]) ->
    [];

parse([[]|B]) ->
    parse([B]);

parse([[A|As]|B]) ->
    A++parse([As|B]).


port_response(Port) ->
    port_response(Port,[],[]).

-define(TIMEOUT,10000).

port_response(Port,Repl,Line) ->
    receive
        {Port, {data, {eol, String}}} ->
            FinLine = lists:reverse([String|Line]),
            case is_dot_terminated(String) of
                true -> {ok, lists:reverse([FinLine|Repl])};
                false -> port_response(Port,[FinLine|Repl],[])
            end;
        {Port, {data, {noeol, String}}} ->
            port_response(Port,Repl,[String|Line])
        after ?TIMEOUT ->
            {timeout, ?TIMEOUT}
    end.

handle_info({'EXIT', Port, Reason}, #state{port = Port} = State) ->
    {stop, {port_terminated, Reason}, State}.

terminate({port_terminated, _Reason}, _State) ->
    ok;

terminate(_Reason, #state{port = Port} = _State) ->
    port_close(Port).

                   
is_dot_terminated([]) ->
    false;

is_dot_terminated(".") ->
    true;

is_dot_terminated([_A|As]) ->
    is_dot_terminated(As).

get(ArgString) ->
    gen_server:call(?MODULE, {command, unchomp(ArgString)}).


%% unchomp does the opposite of the chomp perl function; if the string is not newline terminated, chomp appends a \n to the end of the string.
unchomp(String) ->
    unchomp(String,[]).

unchomp([],Gnirts) ->
    lists:reverse([$\n|Gnirts]);

unchomp("\n",Gnirts) ->
    lists:reverse([$\n|Gnirts]);

unchomp([Char|String],Gnirts) ->
    unchomp(String,[Char|Gnirts]).

