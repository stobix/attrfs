-module(tests_meta).

-include("../include/attrfs.hrl").
-include("../include/debug.hrl").

-export([test/0,fs_prereq/0,switch/0]).

test() ->
  eunit:test({setup,fun fs_prereq/0,fun fs_cleanup/1,tests_meta}).
  

fs_prereq() ->
  application:stop(attrfs),
  application:unload(attrfs),
  switch(),
  ?assertCmd("touch test/from/foo"),
  ?assertCmd("touch test/attrs"),
  ?assertCmd("rm test/attrs"),
  application:start(attrfs).

fs_cleanup(_) ->
  ?debugMsg("letting the dust settle..."),
  switch(),
  application:stop(attrfs),
  ?assertCmd("touch test/attrs"),
  ?assertCmd("rm test/attrs").

%fs_test() ->
%  ?debugMsg("Dir listings:"),
%  ?debugFmt("to:~n ~s~n",[os:cmd("ls -l test/to")]),
%  ?debugFmt("to/a:~n ~s~n",[os:cmd("ls -l test/to/a")]),
%  ?debugFmt("to/r:~n ~s~n",[os:cmd("ls -l test/to/r")]),
%  ?debugFmt("to/r/from:~n ~s~n",[os:cmd("ls -l test/to/r/from")]),
%  ?debugFmt("to/r/all:~n ~s~n",[os:cmd("ls -l test/to/r/all")]).


switch() ->
  ?assertCmd("cp ebin/attrfs.app test/attrfs.app.orig"),
  ?assertCmd("cp test/attrfs.app ebin/attrfs.app"),
  ?assertCmd("cp test/attrfs.app.orig test/attrfs.app").

move_to(String) ->
  {"add",[
   ?_assertCmd("mv test/to/r/from/foo test/to/a/"++String),
   ?_assertCmdStatus(1,"mv test/to/r/from/foo test/to/a/"++String),
   ?_assertCmd("ls test/to/a/"++String++"|grep foo")]}.

remove_from(String) ->
  {"remove",[?_assertCmd("rm test/to/a/"++String++"foo"),
   ?_assertCmdStatus(1,"ls test/to/a/"++String++"|grep foo")]}.


ls_test() ->
  [?_assertCmdOutput("a\nr\n","ls test/to")].

create_test_() ->
  [?_assertCmdOutput("a\nr\n","ls test/to"),
   ?_assertCmd("mkdir -p test/to/a/a/b"),
   move_to("a/b/"),
   ?_assertCmdOutput("Attribute \"a\" had a 1 byte value for test/to/r/from/foo:\nb\n","attr -g a test/to/r/from/foo"),
   move_to("a/"),
   ?_assertCmdOutput("Attribute \"a\" had a 2 byte value for test/to/r/from/foo:\nb,\n","attr -g a test/to/r/from/foo"),
   remove_from("a/b/"),
   ?_assertCmdOutput("Attribute \"a\" had a 0 byte value for test/to/r/from/foo:\n\n","attr -g a test/to/r/from/foo"),
   remove_from("a/"),
   ?_assertCmdStatus(1,"attr -g a test/to/r/from/foo")
   ].


attribute_test_() ->
  [?_assertCmdOutput("Attribute \"sko/hus\" set to a 8 byte value for test/to/r/from/foo:\nget,skri\n","attr -s \"sko/hus\" -V \"get,skri\" test/to/r/from/foo"),
   ?_assertCmd("ls test/to/a/sko/hus/get/foo"),
   ?_assertCmd("ls test/to/a/sko/hus/skri/foo"),
   ?_assertCmd("attr -g \"sko/hus\" test/to/a/sko/hus/skri/foo"),
   ?_assertCmd("attr -r \"sko/hus\" test/to/r/from/foo"),
   ?_assertCmdStatus(1,"ls test/to/a/sko/hus/get|grep foo"),
   ?_assertCmdStatus(1,"ls test/to/a/sko/hus/skri|grep foo"),
   ?_assertCmdStatus(1,"attr -g \"sko/hus\" test/to/a/sko/hus/skri/foo")
   ].


multiple_create_test_() ->
  [?_assertCmd("attr -s \"a/b\" -V \"c,d\" test/to/r/from/foo"),
   ?_assertCmd("attr -s \"a/b\" -V \"c\" test/to/r/from/bar"),
   ?_assertCmd("attr -r \"a/b\" test/to/r/from/foo"),
   ?_assertCmd("attr -g \"a/b\" test/to/r/from/bar"),
   ?_assertCmd("ls test/to/a/a/b/c|grep bar"),
   ?_assertCmdStatus(1,"ls test/to/a/a/b/c|grep foo"),
   ?_assertCmd("attr -r \"a/b\" test/to/r/from/bar")].

