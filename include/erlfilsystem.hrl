
%-type fs::[gb_trees()].
%-type inode_list::[gb_trees()].

-define(ATTR_DB, attributes).
-define(ATTR_DB_FILE, attributes).

-include_lib("kernel/include/file.hrl"). %for record file_info,type io_string()

-record(initargs,
        {dir::string()}).

-type inode_number()::pos_integer()|null.
-type name_tuple()::{string(),inode_number()}.
-type name_list()::[name_tuple()].
-type attrib_list()::[{name_tuple(), name_tuple()|null}].

%%% For now, I separate external files from external dirs. If there's no reason for this, I can always merge external_file/dir into external_entry.

-record(external_file,
        % the path is what path leads to the file in some external file system.
       {path::string()
        % external_file_info mirrors the file info for the real file in some real file system, if applicable.
        ,external_file_info
       }).

-record(external_dir,
        % the path is what path leads to the file in some external file system.
       {path::string()
        % external_file_info mirrors the file info for the real file in some real file system, if applicable.
        ,external_file_info
       }).

%% An internal file is a file without an external representation.
%% An external file or dir exists in an external file system somewhere.
%% An ext info dir is an internal directory representation of some attribute of some dir or file.
-type file_type()::#external_file{}|internal_file|#external_dir{}|ext_info_dir.
-type ext_io_tuple()::{non_neg_integer(),file:io_string()}.

-record(inode_entry,
        % children are the children of the file/dir
        {children::name_list()
        % file_type tells me what kind of file this is. This includes more types than #file_info.type
        ,type::file_type()
        % internal_file_info is the file info that the file has in my file system.
        ,internal_file_info
        % ext_info contains a list of attribute - value pairs in xattr style, used to put files into virtual folders.
        ,ext_info::attrib_list()
        ,ext_io::ext_io_tuple()
        }).

-record(inode_list,
        {inode_entries% ::gb_list of inode_entry
        ,biggest::integer()
        }).


-record(state,
        {
        inode_list,
        open_files
        }).

