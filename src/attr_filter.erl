-module(attr_filter).
-export([filter/3]).


-include("../include/attrfs.hrl").
-include("../include/debug.hrl").

filter(List1,LogicFoldr,List2) ->
  case LogicFoldr==?BUTNOT_FOLDR of
    true ->
      filter_butnot(List1,List2);
    false ->
      case LogicFoldr==?AND_FOLDR of
        true -> 
          filter_and(List1,List2);
        false ->
          case LogicFoldr==?OR_FOLDR of
            true -> 
              filter_or(List1,List2)
          end
      end
  end.

    
%----------------------------------------------------------------

-define(bad(Remove),(Remove =:= logic_dir) orelse (Remove =:= internal_file)).

filter_or([{_,_,Remove}|List1],List2) when ?bad(Remove) ->
  filter_or(List1,List2);

filter_or(List1,[{_,_,Remove}|List2]) when ?bad(Remove) ->
  filter_or(List1,List2);

filter_or([{_,_,attribute_dir}|List1],List2) ->
  filter_or(List1,List2);

filter_or(List1,[{_,_,attribute_dir}=E2|List2]) ->
  [E2|filter_or(List1,List2)];

filter_or([{K1,_,_}=E1|List1],[{K2,_,_}|List2]) when K1 == K2 ->
  [E1|filter_or(List1,List2)];


filter_or(List1,[E2|List2]) ->
  filter_or(List1,[E2],List2);

filter_or(List1,[]) -> 
  List1;

filter_or([],List2) -> 
  List2.

filter_or([E1|List1],List3,[]) ->
  [E1|filter_or(List1,List3)];

filter_or([],List3,List2) ->
  List3++List2;

filter_or([{K1,_,_}=E1|List1],List3,[{K2,_,_}|List2]) when K1 == K2 ->
  [E1|filter_or(List1,List3++List2)];

filter_or(List1,List3,[E2|List2]) ->
  filter_or(List1,[E2|List3],List2).

%----------------------------------------------------------------


% Filter away directories from the first directory...
filter_and([{_,_,attribute_dir}|List1],List2) ->
  filter_and(List1,List2);

% ... but not the second.
filter_and(List1,[{_,_,attribute_dir}=E2|List2]) ->
  [E2|filter_and(List1,List2)];

filter_and([{_,_,Remove}|List1],List2) when ?bad(Remove) ->
  filter_and(List1,List2);

filter_and(List1,[{_,_,Remove}|List2]) when ?bad(Remove) ->
  filter_and(List1,List2);

filter_and([{K1,_,_}=E1|List1],[{K2,_,_}|List2]) when K1 == K2 ->
  [E1|filter_and(List1,List2)];

% K1 ≠≠ K2. We now check if any element in List1 == K2

filter_and(List1,[E2|List2]) ->
  filter_and(List1,[E2],List2);

filter_and(_,[]) -> 
  [];

filter_and([],_) -> 
  [].

% If we've checked all elements in List2 against E1, it is safe to assume that E1 has no counterpart in List2, and is thus removed.

filter_and([_E1|List1],List3,[]) ->
  filter_and(List1,List3);

filter_and([],_,_) ->
  [];

% found an equal element. Removing from stack and recursing

filter_and([{K1,_,_}=E1|List1],List3,[{K2,_,_}|List2]) when K1 == K2 ->
  [E1|filter_and(List1,List3++List2)];

filter_and(List1,List3,[E2|List2]) ->
  filter_and(List1,[E2|List3],List2).

%----------------------------------------------------------------

filter_butnot([{_,_,attribute_dir}|List1],List2) ->
  filter_butnot(List1,List2);

filter_butnot(List1,[{_,_,attribute_dir}=E2|List2]) ->
  [E2|filter_butnot(List1,List2)];

filter_butnot([{_,_,Remove}|List1],List2) when ?bad(Remove) ->
  filter_butnot(List1,List2);

filter_butnot(List1,[{_,_,Remove}|List2]) when ?bad(Remove) ->
  filter_butnot(List1,List2);

filter_butnot(List1,List2) ->
  filter_butnot(List1,[],List2).

filter_butnot([E1|List1],List3,[]) ->
  [E1|filter_butnot(List1,List3)];

filter_butnot([],_,_) ->
  [];

filter_butnot([{K1,_,_}|List1],List3,[{K2,_,_}=E2|List2]) when K1 == K2 ->
  filter_butnot(List1,[],[E2|List3++List2]);

filter_butnot(List1,List3,[E2|List2]) ->
  filter_butnot(List1,[E2|List3],List2).
