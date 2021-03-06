-module(tests_meta).

-include("../include/attrfs.hrl").
-include_lib("newdebug/include/newdebug19.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([test/0,fs_prereq/0,switch/0,timer/1,timer/2]).

test() ->
  eunit:test({setup,fun fs_prereq/0,fun fs_cleanup/1,tests_meta}).
  

fs_prereq() ->
  ?debugMsg("preparing tests"),
  ?assertCmd("mkdir -p test/from/also"),
  ?assertCmd("mkdir -p test/to"),
  MyAttrApp=
    {application,attrfs,
      [{description,"A file system used to sort files by attributes."},
       {vsn,0.9},
       {modules,[attrfs,attrfs_srv,attrfs_sup,inode,inode_sup,
                 tree_srv,tree_sup]},
       {applications,[kernel,stdlib,fuserl]},
       {registered,[attrfs]},
       {mod,{attrfs,[]}},
       {env,[{to_dir,"test/to"},
             {from_dir,"test/from"},
             {attributes_db,"test/attrs"},
             {linked_in,true},
             {real_name,"r"},
             {attr_name,"a"},
             {logic_name,"l"},
             {all_name,"all"},
             {dup_name,"dup"},
             {and_name,"&"},
             {or_name,"|"},
             {butnot_name,"&~"},
             {dup_prefix,"d"},
             {dup_suffix,"u"},
             {dup_ext,".dup"},
             {mount_opts,"allow_other,default_permissions"}]}]},
  file:write_file("test/attrfs.app",io_lib:write(MyAttrApp)++"."),
  application:stop(attrfs),
  application:unload(attrfs),
  ?assertCmd("touch test/from/also/etaoin"),
  ?assertCmd("touch test/from/etaoin"),
  ?assertCmd("touch test/from/foo"),
  ?assertCmd("touch test/from/bar"),
  switch(),
  utils:chain(attrfs).

fs_cleanup(_) ->
  ?debugMsg("cleaning up"),
  switch(),
  application:stop(attrfs),
  ?assertCmd("rm -r test").

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
  [?_assertCmdOutput("a\nl\nr\n","ls test/to"),
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
  Foo="test/to/r/from/foo",
  Bar="test/to/r/from/bar",
  [?_assertCmd("attr -s \"a/b\" -V \"c,d\" "++Foo),
   ?_assertCmd("attr -s \"a/b\" -V \"c\" "++Bar),
   ?_assertCmd("attr -r \"a/b\" "++Foo),
   ?_assertCmd("attr -g \"a/b\" "++Bar),
   ?_assertCmd("ls test/to/a/a/b/c|grep bar"),
   ?_assertCmdStatus(1,"ls test/to/a/a/b/c|grep foo"),
   ?_assertCmd("attr -r \"a/b\" "++Bar)
   ].

move_test() ->
  A="test/to/a/",
  F=A++"F",
  W=A++"W",
  [?_assertCmd("mkdir "++F),
   ?_assertCmd("mkdir "++W),
   ?_assertCmd("mv "++W++" "++F),
   ?_assertCmd("ls -l "++F++"|grep W")].

access_test_() ->
  Foo="test/to/r/from/foo",
  ABCFoo="test/to/a/b/c/foo",
  ABC="test/to/a/b/c",
  ABCANDAB="test/to/l/b/c/\\&/b/c",
  ABCANDABFoo="test/to/l/b/c/\\&/b/c/foo",
  [?_assertCmd("attr -s b -V c " ++ Foo),
   ?_assertCmdOutput("drwxr-xr-x\n","ls -ld " ++ ABC         ++ "|cut -f1 -d \" \""),
   ?_assertCmdOutput("-rw-r--r--\n","ls -l "  ++ ABCFoo      ++ "|cut -f1 -d \" \""),
   ?_assertCmdOutput("dr-xr-xr-x\n","ls -ld " ++ ABCANDAB    ++ "|cut -f1 -d \" \""),
   % This one I still need to fix, if it should be fixed.
   % ?_assertCmdOutput("-r--r--r--\n","ls -l "  ++ ABCANDABFoo ++ "|cut -f1 -d \" \""),
   ?_assertCmdStatus(1,"mv "  ++ ABCANDABFoo ++ " test/to/a/b/"),
   ?_assertCmd("attr -r a " ++ Foo)].


all_test_() ->
  Foo="test/to/r/from/foo",
  Bar="test/to/r/from/bar",
  AllFoo="test/to/r/all/foo",
  AllBar="test/to/r/all/bar",
  [?_assertCmd("attr -s \"a/b\" -V \"c,d\" "++Foo),
   ?_assertCmd("attr -s \"a/b\" -V \"c\" "++Bar),
   ?_assertCmd("attr -g \"a/b\" "++AllFoo),
   ?_assertCmd("attr -r \"a/b\" "++Foo),
   ?_assertCmd("attr -g \"a/b\" "++Bar),
   ?_assertCmd("attr -g \"a/b\" "++AllBar),
   ?_assertCmd("ls test/to/a/a/b/c|grep bar"),
   ?_assertCmdStatus(1,"ls test/to/a/a/b/c|grep foo"),
   ?_assertCmd("attr -r \"a/b\" "++Bar)
   ].

rmdir_test_() ->
  From="test/to/r/from/",
  Foo=From++"foo",
  Bar=From++"bar",
  [      ?_assertCmd("attr -s \"b/c\" -V d "++Foo),
         ?_assertCmd("attr -s \"b/c\" -V d "++Bar),
         ?_assertCmd("rm test/to/a/b/c/d/foo"),
         ?_assertCmd("attr -g \"b/c\" "++Bar),
 ?_assertCmdStatus(2,"ls test/to/a/b/c/d/foo"),
         ?_assertCmd("rm -r test/to/a/b/c/d"),
 ?_assertCmdStatus(2,"ls test/to/a/b/c/d/bar"),
 ?_assertCmdStatus(1,"attr -g \"b/c\" "++Bar),
         ?_assertCmd("attr -s ho -V hm "++Foo), %This generated a bug before.
         ?_assertCmd("attr -r ho "++Foo)
   ].

namespace_test_() ->
  Foo="test/to/r/all/foo",
  [?_assertCmdOutput("Attribute \"foo\" set to a 3 byte value for test/to/r/all/foo:\nfoo\n","attr -s foo -V foo test/to/r/all/foo"),
   ?_assertCmdOutput("Attribute \"foo\" had a 3 byte value for test/to/r/all/foo:\nfoo\n","attr -g foo "++Foo),
   ?_assertCmd("ls test/to/a/foo/foo/foo"),
   ?_assertCmd("attr -r foo "++ Foo),
   ?_assertCmd("mkdir test/to/a/from")].
  

dups_test_() -> 
  DupEtaoin="test/to/r/dup/etaoin.dup",
  AllEtaoin="test/to/r/all/etaoin",
  AllDupEtaoin="test/to/r/all/detaoinu",
  [?_assertCmd("ls test/to/r/dup"),
   ?_assertCmd("ls "++DupEtaoin),
   ?_assertCmd("ls "++AllEtaoin),
   ?_assertCmd("ls "++AllDupEtaoin),
   ?_assertCmdOutput("\"detaoinu\" \"test/from/also/etaoin\"\n","cat "++DupEtaoin++"|grep detaoinu" )
   
  ].
  
internal_files_test_() ->
  [?_assertCmd("touch test/to/a/yo"),
   ?_assertCmd("echo tjo > test/to/a/yo"),
   ?_assertCmdOutput("tjo\n","cat test/to/a/yo"),
   ?_assertCmd("rm test/to/a/yo"),
   ?_assertCmd("touch test/to/a/yo"),
   ?_assertCmd("echo tjo > test/to/a/yo"),
   ?_assertCmdOutput("tjo\n","cat test/to/a/yo"),
   ?_assertCmd("ls test/to/l/"),
   ?_assertCmd("rm test/to/a/yo")].
  


timer_prereq(Amount) ->
  ?debugMsg("preparing timer"),
  ?assertCmd("mkdir -p test/from/"),
  ?assertCmd("mkdir -p test/to"),
  MyAttrApp=
    {application,attrfs,
      [{description,"A file system used to sort files by attributes."},
       {vsn,0.9},
       {modules,[attrfs,attrfs_srv,attrfs_sup,inode,inode_sup,
                 tree_srv,tree_sup]},
       {applications,[kernel,stdlib,fuserl]},
       {registered,[attrfs]},
       {mod,{attrfs,[]}},
       {env,[{to_dir,"test/to"},
             {from_dir,"test/from"},
             {attributes_db,"test/attrs"},
             {linked_in,true},
             {real_name,"r"},
             {attr_name,"a"},
             {all_name,"all"},
             {dup_name,"dup"},
             {mount_opts,"allow_other,default_permissions"}]}]},
  file:write_file("test/attrfs.app",io_lib:write(MyAttrApp)++"."),
  application:stop(attrfs),
  application:unload(attrfs),
  lists:foreach(fun(X) ->
    ?assertCmd("touch test/from/"++integer_to_list(X))
    end,
    lists:seq(1,Amount)),
  switch(),
  utils:chain(attrfs).

timer_prereq(Amount,AmountDup) ->
  ?debugMsg("preparing timer"),
  ?assertCmd("mkdir -p test/from/dup"),
  ?assertCmd("mkdir -p test/to"),
  MyAttrApp=
    {application,attrfs,
      [{description,"A file system used to sort files by attributes."},
       {vsn,0.9},
       {modules,[attrfs,attrfs_srv,attrfs_sup,inode,inode_sup,
                 tree_srv,tree_sup]},
       {applications,[kernel,stdlib,fuserl]},
       {registered,[attrfs]},
       {mod,{attrfs,[]}},
       {env,[{to_dir,"test/to"},
             {from_dir,"test/from"},
             {attributes_db,"test/attrs"},
             {linked_in,true},
             {real_name,"r"},
             {attr_name,"a"},
             {all_name,"all"},
             {dup_name,"dup"},
             {dup_prefix,""},
             {dup_suffix,"d"},
             {mount_opts,"allow_other,default_permissions"}]}]},
  file:write_file("test/attrfs.app",io_lib:write(MyAttrApp)++"."),
  application:stop(attrfs),
  application:unload(attrfs),
  lists:foreach(fun(X) ->
    ?assertCmd("touch test/from/"++integer_to_list(X))
    end,
    lists:seq(1,Amount)),
  lists:foreach(fun(X) ->
    ?assertCmd("touch test/from/dup/"++integer_to_list(X))
    end,
    lists:seq(1,AmountDup)),
  switch(),
  utils:chain(attrfs).

timer_cleanup() ->
  fs_cleanup(foo).

timertest() ->
%  A=os:cmd("ls test/to/"),
%  B=os:cmd("ls test/to/r"),
  ?debugTime("ls all:",os:cmd("ls test/to/r/all")),
  ?debugTime("ls dup:",os:cmd("ls test/to/r/dup")).
%  ?debugVal(A),
%  ?debugVal(B),
%  ?debugVal(C).

timea(N) ->
  ?debugTime("init:", timer_prereq(N)),
  ?debugTime("ls:", timertest()),
  ?debugTime("cleanup:", timer_cleanup()).

timer(N) ->
  process_flag(trap_exit, true),
  timea(N),
  receive
    N ->
      ?debugMsg("cleaning up anyway"),
      timer_cleanup()
  after 1 ->
    ok
  end.

timea(N,M) ->
  ?debugTime("init:", timer_prereq(N,M)),
  ?debugTime("ls:", timertest()),
  ?debugTime("cleanup:", timer_cleanup()).

timer(N,M) ->
  process_flag(trap_exit, true),
  timea(N,M),
  receive
    N ->
      ?debugMsg("cleaning up anyway"),
      timer_cleanup()
  after 1 ->
    ok
  end.
 
