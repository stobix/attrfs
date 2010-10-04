-module(filter).
-export([filter/3]).

-include("../include/debug.hrl").

filter(List1,"BUTNOT",List2) ->
  Filtered=filter_butnot(List1,List2),
  Filtered;

filter(List1,"AND",List2) ->
  filter_and(List1,List2);

filter(List1,"OR",List2) ->
  filter_or(List1,List2).
    
%----------------------------------------------------------------

filter_or([{K1,_}=E1|List1],[{K2,_}|List2]) when K1 == K2 ->
  [E1|filter_or(List1,List2)];


filter_or(List1,[E2|List2]) ->
  filter_or(List1,[E2],List2);

filter_or([],[]) ->
  [].

filter_or([E1|List1],List3,[]) ->
  [E1|filter_or(List1,List3)];

filter_or([],List3,List2) ->
  List3++List2;

filter_or([{K1,_}=E1|List1],List3,[{K2,_}|List2]) when K1 == K2 ->
  [E1|filter_or(List1,List3++List2)];

filter_or(List1,List3,[E2|List2]) ->
  filter_or(List1,[E2|List3],List2).

%----------------------------------------------------------------

filter_and([{K1,_}=E1|List1],[{K2,_}|List2]) when K1 == K2 ->
  [E1|filter_and(List1,List2)];

filter_and(List1,[E2|List2]) ->
  filter_and(List1,[E2],List2);

filter_and([],[]) ->
  [].

filter_and([_E1|List1],List3,[]) ->
  filter_and(List1,List3);

filter_and([],_,_) ->
  [];

filter_and([{K1,_}=E1|List1],List3,[{K2,_}|List2]) when K1 == K2 ->
  [E1|filter_and(List1,List3++List2)];

filter_and(List1,List3,[E2|List2]) ->
  filter_and(List1,[E2|List3],List2).

%----------------------------------------------------------------

filter_butnot(List1,List2) ->
  filter_butnot(List1,[],List2).

filter_butnot([E1|List1],List3,[]) ->
  [E1|filter_butnot(List1,List3)];

filter_butnot([],_,_) ->
  [];

filter_butnot([{K1,_}|List1],List3,[{K2,_}=E2|List2]) when K1 == K2 ->
  filter_butnot(List1,[],[E2|List3++List2]);

filter_butnot(List1,List3,[E2|List2]) ->
  filter_butnot(List1,[E2|List3],List2).
