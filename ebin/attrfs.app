{application, attrfs,
  [{description,"A file system used to sort files by attributes."},
   {vsn,"1.0"},
   {modules,[
attr_async,
attr_ext,
attr_filter,
attrfs,
attrfs_srv,
attrfs_sup,
attr_init,
attr_lookup,
attr_mkdir,
attr_opendir,
attr_open,
attr_remove,
attr_rename,
attr_reply,
attr_tools,
debug,
inode,
inode_sup,
tests_meta,
tree_srv,
tree_sup
           ]},
   {applications,[kernel,stdlib,fuserl,reporter]}, 
   {registered,[attrfs]},
   {mod,{attrfs,[]}},
   {env,
     [{to_dir,"/home/Volatile/attrfs"},
%      {from_dirs,[
%        "/home/Volatile/random images/",
%        "/home/Volatile/more random images/"]},
      {linked_in,true},
      {mount_opts, "allow_other,default_permissions"}
     ]
   } 
  ]
}.
