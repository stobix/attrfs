-module(ports).
-behaviour(gen_server).

-export([start_link/1,
    init/1,
    handle_call/3,
    handle_info/2,
    terminate/2
    ]).

-export([get/1]).
-export([get_split/2]).
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

handle_call({split_command,{Cmd,Args}},_from,State=#state{port=Port}) ->
    port_command(Port,Cmd),
    port_command(Port," "),
    port_command(Port,Args),
    case port_response(Port) of 
        {ok, Val} -> {reply,flatten(Val),State};
        {timeout, _T} -> {stop, port_timeout, State}
    end;

handle_call({command,CmdString},_from, State=#state{port=Port}) -> 
    port_command(Port,CmdString),
    case port_response(Port) of 
        {ok, Val} -> {reply,flatten(Val),State};
        {timeout, _T} -> {stop, port_timeout, State}
    end;

handle_call({parse_command,CmdString},_from,State) ->
    {reply,parse(gen_server:call(?MODULE, {command, unchomp(CmdString)})),State}.
        
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
    port_close(Port).

                   
is_dot_terminated([]) ->
    false;

is_dot_terminated(".") ->
    true;

is_dot_terminated([_A|As]) ->
    is_dot_terminated(As).

get(ArgString) ->
    gen_server:call(?MODULE, {command, unchomp(ArgString)}).

get_split(Cmd,Args) ->
    gen_server:call(?MODULE, {split_command, {Cmd,unchomp(Args)}}).


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

