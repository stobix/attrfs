%%%=========================================================================
%%%                                 LICENSE
%%%=========================================================================
%%%
%%%  This program is free software; you can redistribute it and/or modify
%%%  it under the terms of the GNU General Public License as published by
%%%  the Free Software Foundation; either version 2 of the License, or
%%%  (at your option) any later version.
%%%
%%%  This program is distributed in the hope that it will be useful,
%%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%  GNU Library General Public License for more details.
%%%
%%%  You should have received a copy of the GNU General Public License
%%%  along with this program; if not, write to the Free Software
%%%  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%%
%%%=========================================================================
-ifndef(debug_hrl).
-define(debug_hrl,[]).
-ifdef(debug).
-ifdef(timestamp).
-define(DEB1(X),io:format("[~2..0p:~2..0p:~2..0p] ~w:~w ~s~n",tuple_to_list(time())++[?MODULE,?LINE,X])).
-define(DEB2(X,Y),io:format("[~2..0p:~2..0p:~2..0p] ~w:~w "++X++"~n",tuple_to_list(time())++[?MODULE,?LINE,Y])).
-define(DEBL(X,Y),io:format("[~2..0p:~2..0p:~2..0p] ~w:~w "++X++"~n",tuple_to_list(time())++[?MODULE,?LINE]++Y)).
-else.
-define(DEB1(X),io:format("~w:~w ~s~n",[?MODULE,?LINE,X])).
-define(DEB2(X,Y),io:format("~w:~w "++X++"~n",[?MODULE,?LINE,Y])).
-define(DEBL(X,Y),io:format("~w:~w "++X++"~n",[?MODULE,?LINE]++Y)).
-endif.
-else.
-define(DEB1(X),void).
-define(DEB2(X,Y),void).
-define(DEBL(X,Y),void).
-endif.
-endif.
