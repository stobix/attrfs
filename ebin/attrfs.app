{application, attrfs,
    [{description,"A file system used to sort files by attributes."},
     {vsn,0.8},
     {modules,[attrfs,attrfs_srv,attrfs_sup]},
     {applications,[kernel,stdlib,fuserl,inode]},
     {registered,[attrfs]},
     {mod,{attrfs,[]}},
     {env,[{to_dir,"/home/Volatile/programmering/erlang/lib/attrfs/test/testfs"},
         {from_dir,"/home/Volatile/programmering/erlang/lib/attrfs/test/mirrorfs"}]}
]}.
