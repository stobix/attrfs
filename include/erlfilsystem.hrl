
%%%-------------------------------------------------------
%%% @author Joel Ericson <kasettbok@gmail.com>
%%%
%%% @version Almost working, yo...
%%% @doc Header file for the attrfs modul.
%%% 
%%% @end
%%%-------------------------------------------------------

-define(ATTR_DB, attributes).
-define(ATTR_DB_FILE, attributes).

-include_lib("kernel/include/file.hrl"). %for record file_info,type io_string()
-include_lib("fuserl/include/fuserl.hrl"). % for #stat{}

-record(initargs,
        {dir::string()}).

-type inode_number()::pos_integer().
-type name_tuple()::{string(),inode_number()}.
-type name_list()::[name_tuple()].
-type attrib_list()::[{name_tuple(), name_tuple()}].

%%% For now, I separate external files from external dirs. If there's no reason for this, I can always merge external_file/dir into external_entry.

-record(external_file,
        % the path is what path leads to the file in some external file system.
       {path::string()
        % external_file_info mirrors the file info for the real file in some real file system, if applicable.
        ,external_file_info::#file_info{} % file:#file_info{}
       }).

-record(external_dir,
        % the path is what path leads to the file in some external file system.
       {path::string()
        % external_file_info mirrors the file info for the real file in some real file system, if applicable.
        ,external_file_info::#file_info{} % file:#file_info{}
       }).

%% An internal file is a file without an external representation.
%% An external file or dir exists in an external file system somewhere.
%% An ext info dir is an internal directory representation of some attribute of some dir or file.
-type file_type()::#external_file{}|internal_file|#external_dir{}|attribute_dir|internal_dir.
-type ext_io_tuple()::{non_neg_integer(),file:io_string()}.

-record(inode_entry,
        % This file system uses a one-to-one correspondence between names and inodes, so that move, link and copy can produce the same results.
        % TODO: Deal with the case that several files in external dirs have the same name.
        {name::string()
        % children are the children of the file/dir
        ,children::name_list()
        % file_type tells me what kind of file this is. This includes more types than #file_info.type
        ,type::file_type()
        % internal_file_info is the file info that the file has in my file system.
        ,internal_file_info::#stat{} % fuserl:#stat{}
        % ext_info contains a list of attribute - value pairs in xattr style, used to put files into virtual folders.
        ,ext_info::attrib_list()
        ,ext_io::ext_io_tuple()
        }).


-type attribute_entries()::[name_tuple()|{inode,inode_number()}].

-record(inode_list,
        {inode_entries% ::gb_trees{} of inode_entry
        }).

-record(state,
        {inode_entries% ::gb_trees{} of inode_entry with inode_number() keys
        ,open_files%%::#gb_trees{} of #direntry{} with inode_number() keys 
        ,attribute_list%:: indexed by {Attribute,Value}, containing attribute_entries()
        ,biggest_ino::integer() % actually the smallest available inode number.
        }).

-define (DIR (Stat), Stat#stat{ st_mode = (Stat#stat.st_mode band 8#777) bor ?S_IFDIR }).
%-define (DIR (Stat), Stat).

%these I stole from fuserlproc. Maybe they'll come in handy.
-define (DIRATTR (X), #stat{ st_ino = (X), 
                             st_mode = ?S_IFDIR bor 8#0555, 
                             st_nlink = 1 }).


-define(STAT (X,Y,Z), #stat{ st_ino = (X),
                             st_mode = (Y),
                             st_nlink = Z
                                 }.
