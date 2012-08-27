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
     [
      {linked_in,false},
      {mount_opts, "allow_other,default_permissions"}
     ]
   } 
  ]
}.
