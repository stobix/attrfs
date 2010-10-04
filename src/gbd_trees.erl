-module(gbd_trees).
%%% This module tries to implement gb_trees that uses two unique keys for each entry.
%%% It is implemented to be time efficient, not space efficient. The overhead is (hopefully) kept to a minimum.

-export([
  empty/0,
  enter/4,
  lookup/3,
  lookup1/2,
  lookup2/2,
  insert/4,
  delete_any/3,
  delete_any1/2,
  delete_any2/2
  ]).


empty() -> {gb_trees:empty(),gb_trees:empty()}.

enter(Key1,Key2,Value,{Tree1,Tree2}) ->
  case gb_trees:lookup(Key1,Tree1) of
    {value, {Key,_Val}} when Key == Key2  ->
      {gb_trees:enter(Key1,{Key2,Value},Tree1),
       gb_trees:enter(Key2,{Key1,Value},Tree2)};
    {value, {OtherKey, _}} ->
      FilteredTree2=gb_trees:delete(OtherKey,Tree2),
      {gb_trees:enter(Key1,{Key2,Value},Tree1),
       gb_trees:enter(Key2,{Key1,Value},FilteredTree2)};
    none ->
      case gb_trees:lookup(Key2,Tree2) of
        {value, {Key, _Val}} when Key == Key1 ->
          this_cannot_logically_happen;
        {value, {OtherKey, _}} ->
          FilteredTree1=gb_trees:delete(OtherKey,Tree1),
          {gb_trees:enter(Key1,{Key2,Value},FilteredTree1),
           gb_trees:enter(Key2,{Key1,Value},Tree2)};
        none ->
      {gb_trees:enter(Key1,{Key2,Value},Tree1),
       gb_trees:enter(Key2,{Key1,Value},Tree2)}
      end
  end.

lookup1(Key1,{Tree1,_Tree2}) ->
  case gb_trees:lookup(Key1,Tree1) of
    {value,{_,Value}} ->
      {value,Value};
    none -> none
  end.


lookup2(Key2,{_Tree1,Tree2}) ->
  case gb_trees:lookup(Key2,Tree2) of
    {value,{_,Value}} ->
      {value,Value};
    none -> none
  end.

lookup(Key1,Key2,{Tree1,Tree2}) ->
  case gb_trees:lookup(Key1,Tree1) of
    none -> none;
    _  -> 
      case gb_trees:lookup(Key2,Tree2) of
        {value,{_,Value}} ->
          {value,Value};
        none -> none
      end
  end.

insert(Key1,Key2,Value,{Tree1,Tree2}) ->
  case gb_trees:lookup(Key1,Tree1) of
    none ->
      NewTree2=gb_trees:insert(Key2,{Key1,Value},Tree2),
      {gb_trees:insert(Key1,{Key2,Value},Tree1),
       NewTree2};
    _ -> gb_trees:insert(Key1,{Key2,Value},Tree1) % generate an error the lazy way.
  end.

delete_any1(Key1,{Tree1,Tree2}) ->
  case gb_trees:lookup(Key1,Tree1) of
    {value,{Key2,_}} ->
      {gb_trees:delete_any(Key1,Tree1),
       gb_trees:delete_any(Key2,Tree2)};
    none -> 
      {Tree1,Tree2}
  end.

delete_any2(Key2,{Tree1,Tree2}) ->
  case gb_trees:lookup(Key2,Tree2) of
    {value,{Key1,_}} ->
      {gb_trees:delete_any(Key1,Tree1),
       gb_trees:delete_any(Key2,Tree2)};
    none -> 
      {Tree1,Tree2}
  end.

delete_any(Key1,Key2,{Tree1,Tree2}) ->
  case gb_trees:lookup(Key1,Tree1) of
    {value,{Key,_}} when Key == Key2 ->
        {gb_trees:delete_any(Key1,Tree1),
         gb_trees:delete_any(Key2,Tree2)};
      _ -> {Tree1,Tree2}
  end.
