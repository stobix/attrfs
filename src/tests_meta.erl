-module(tests_meta).

-include("../include/attrfs.hrl").

%-export([fs_prereq/0]).


%fs_prereq() ->
%  ?assertCmd("mkdir -p tmp/attrfs"),
%  ?assertCmd("cp ebin/attrfs.app tmp/attrfs"),
%  ?assertCmd("echo \"to_dir\" > edcmd"),
%  ?assertCmd("echo s/"++[$\\]++"(to_dir,\\)[^}]*/\\1\\\"tmp\\/attrfs >> edcmd"),
%  ?assertCmd("echo l >> edcmd"),
%  ?assertCmd("echo Q >> edcmd"),
%  ?assertCmd("ed ebin/attrfs.app < edcmd").

%fs_test_() ->
