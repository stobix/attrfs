
%-type fs::[gb_trees()].
%-type inode_list::[gb_trees()].

%-include_lib("kernel/include/file.hrl"). %for file:read_file_info

-type inode_number()::integer().
-type name_list()::[{string(),inode_number()}].
-type attrib_list()::[{string(),string()}].

-record(inode_entry,
        {children::name_list()
        ,file_type::file_type().
        ,real_file_info
        ,fake_file_info
        ,perms
        ,ext_info::attrib_list()
        }).

-record(inode_list,
        {inode_entries::[inode_entry]
        ,biggest::integer()
        }).


-record(state,
        {
        inode_list
        }).

