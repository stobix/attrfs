{application, attrfs,
  [{description,"A file system used to sort files by attributes."},
   {vsn,0.9},
   {modules,[attrfs,attrfs_srv,attrfs_sup,inode,inode_sup,tree_srv,tree_sup]},
   {applications,[kernel,stdlib,fuserl]}, 
   {registered,[attrfs]},
   {mod,{attrfs,[]}},
   {env,
     [{to_dir,"/home/Volatile/attrfs"},
      {from_dirs,["/mnt/bildix/kamer2","/mnt/bildix/kamer3"]},
      {attributes_db,"/home/Volatile/attributes"},
      {linked_in,true},
      {real_name,"real"},
      {attr_name,"attributes"},
      {and_name,".AND"},
      {or_name,".OR"},
      {butnot_name,".BUTNOT"},
      {all_name,"all"},
      {dup_name,"duplicates"},
      {mount_opts, "allow_other,default_permissions"}
     ]
   } 
  ]
}.
