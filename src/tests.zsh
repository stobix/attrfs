#!/bin/zsh

mkdir -p tmp/attrfs
cp -v ebin/attrfs.app tmp/attrfs.app 
ed ebin/attrfs.app <<EOT
/to_dir
s/\(to_dir,\)\([^}]*\)/\1"tmp\/attrfs"/
l
wq ebin/attrfs.app
EOT

ERL_LIBS="." erl -noshell +A 3 -sname test -run attrfs_test start&
sleep 2
echo "Contents of tmp/attrfs:"
ls tmp/attrfs
echo "-------"
ERL_LIBS="." erl -noshell +A 3 -run attrfs_test stop "test@`hostname`" kaka&

mv tmp/attrfs.app ebin/attrfs.app
