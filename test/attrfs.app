{application, attrfs,
  [{description,"A file system used to sort files by attributes."},
   {vsn,0.9},
   {modules,[attrfs,attrfs_srv,attrfs_sup,inode,inode_sup,tree_srv,tree_sup]},
   {applications,[kernel,stdlib,fuserl]}, 
   {registered,[attrfs]},
   {mod,{attrfs,[]}},
   {env,
     [{to_dir,"test/to"},
      {from_dir,"test/from"},
      {attributes_db,"test/attrs"},
      {linked_in,true},
      {real_name,"r"},
      {attr_name,"a"},
      {all_name,"all"},
      {and_name,"&"},
      {or_name,"|"},
      {butnot_name,"&~"},
      {mount_opts, "allow_other,default_permissions"}
     ]
   } 
  ]
}.
