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
-define(F1,debug:timestamp).
-define(F2,debug:timestamp).
-else.
-define(F1,debug:msg).
-define(F2,debug:msg).
-endif.

-else.

-define(F1(M,L1,L2,X),void).
-define(F2(M,L1,L2,X,Y),void).

-endif.


-define(DEB1(L,X),?F1(L,?MODULE,?LINE,X)).
-define(DEB2(L,X,Y),?F2(L,?MODULE,?LINE,X,[Y])).
-define(DEBL(L,X,Y),?F2(L,?MODULE,?LINE,X,Y)).


-endif.
