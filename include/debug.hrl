-ifdef(debug).
-define(DEB1(X),io:format("~p:~p ~s~n",[?MODULE,?LINE,X])).
-define(DEB2(X,Y),io:format("~p:~p "++X++"~n",[?MODULE,?LINE,Y])).
-define(DEBL(X,Y),io:format("~p:~p "++X++"~n",[?MODULE,?LINE]++Y)).
-else.
-define(DEB1(X),void).
-define(DEB2(X,Y),void).
-define(DEBL(X,Y),void).
-endif.
