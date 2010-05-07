{application, attrfs,
    [{description,"A file system used to sort files by attributes."},
     {vsn,0.8},
     {modules,[attrfs,attrfs_srv,attrfs_sup]},
     {applications,[kernel,stdlib,fuserl]}, %,inode,tree_app]},
     {registered,[attrfs]},
     {mod,{attrfs,[]}},
     {env,[{to_dir,"/home/Volatile/testfs"},
         {from_dir,"/home/Volatile/mirrorfs"}]}
]}.
