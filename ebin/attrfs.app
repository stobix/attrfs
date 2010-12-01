{application, attrfs,
  [{description,"A file system used to sort files by attributes."},
   {vsn,0.9},
   {modules,[attrfs,attrfs_srv,attrfs_sup,inode,inode_sup,tree_srv,tree_sup]},
   {applications,[kernel,stdlib,fuserl]}, %,inode,tree_app]},
   {registered,[attrfs]},
   {mod,{attrfs,[]}},
   {env,
     [{to_dir,"/home/Volatile/attrfs"},
      {from_dirs,["/home/Volatile/mirrorfs","/home/Volatile/testfs"]},
      {attributes_db,"/home/Volatile/attributes"},
      {linked_in,true},
      {real_name,"real"},
      {attr_name,"attributes"},
      {all_name,"my_files"},
      {mount_opts, "allow_other,default_permissions"}
     ]
   } 
  ]
}.
