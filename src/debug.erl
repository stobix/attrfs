-module(debug).

-export([msg/4,msg/5,timestamp/4,timestamp/5]).

%-define(sp(X),?sc(case X of {_,err} -> 0; err -> 0; A -> A end)).
%-define(sc(X),string:copies(" ",case X of {_,Y} -> Y; Y -> Y end)).
-define(sp(X)," ").

msg(Level,Module,Line,Msg) ->
  debug_message("~-10s:~4..0b ~s~s~n",[Module,Line,?sp(Level),Msg],Level).

msg(Level,Module,Line,FormatString,Msg) ->
  debug_message("~-10s:~4..0b ~s"++FormatString++"~n",[Module,Line,?sp(Level)]++Msg,Level).

timestamp(Level,Module,Line,Msg) ->
  debug_message("[~2..0b:~2..0b:~2..0b] ~-10s:~4..0b ~s~s~n",tuple_to_list(time())++[Module,Line,?sp(Level),Msg],Level).

timestamp(Level,Module,Line,FormatString,Msg) ->
  debug_message("[~2..0b:~2..0b:~2..0b] ~-10s:~4..0b ~s"++FormatString++"~n",tuple_to_list(time())++[Module,Line,?sp(Level)]++Msg,Level).



-ifdef(modules).
output(FormatString,Message,Token) ->
case ?modules of
  all ->
    io:format(FormatString,Message);
  _ ->
    case lists:member(Token,?modules) of
      true ->
        io:format(FormatString,Message);
      false -> 
        Token
    end
  end.
-else.
output(_FormatString,_Message,_Token) ->
  no_modules.
-endif.

-ifndef(level).
-define(level,1).
-endif.

-define(checkl(L,Y),
  case L =< ?level of
    true -> Y;
    _ -> ok
  end).

debug_message(FormatString,Message,Level) ->
  case Level of
    % Show errors regardless of from which module they come.
    {Info,err} -> io:format(standard_error,FormatString,Message);
    % Errors without module info.
    err -> io:format(standard_error,FormatString,Message);
    % Filter normal messages by module.
    {Info,L} -> ?checkl(L,output(FormatString,Message,Info));
    % Messages filtered only by debug level.
    L -> ?checkl(L,io:format(FormatString,Message))
  end.
      
