#!/bin/zsh

mkdir -p tmp/attrfs
ERL_LIBS="." erl -noshell -sname atest -run attrfs_test start&
sleep 2
echo "bla"

ERL_LIBS="." erl -noshell -run attrfs_test stop "atest@`hostname`" kaka&

