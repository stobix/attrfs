%% ----------------------------------------------------
%% data type: file data for an "abstract file"
%%   where:
%%      inode: the inode number of the file (integer)
%%      name: the name of the file (string)
%%      owner: the owner of the file (string)
%%      group: the group to which the file belongs (string)
%%      rights: the rights of the file. Set these with the stat macros of 
%%              fuserl.hrl or something similar.
%%      ctime: creation time. (integer, epoch-time)
%%      atime: access time. (integer, epoch-time)
%%      mtime: time last modified. (integer, epoch-time)
%% ----------------------------------------------------

-record(file_data,{inode,name,owner,group,rights,ctime,atime,mtime}).

%% ----------------------------------------------------
%% data type: meta info, extra info for abstract_files
%%  where:
%%      is_physical: whether the file is a file in some other file system
%%          or not. (here is assumed that all file systems except this are
%%          physical) (boolean)
%%      physical_name: the name of the file on the file system from which it
%%          is taken, if any. (string,default: "")
%%      type: type of (non-physical) file. The file might be a file 
%%          representation of some EXIF data, or a directory containing files
%%          filtered by some filter or other. The type attribute might help 
%%          distinguishing between different meta file types. 
%%          (string,default: "")
%%      type_info: A list of type specific info. 
%%          (list of strings, default: [])
%%
%% ----------------------------------------------------

-record(meta_info,{is_physical,physical_name="", type="",type_info=[]}).
%% ----------------------------------------------------
%% data type: abstract file, the basic unit of this file system.
%%   where:
%%      file_data: normal file data, common for most file systems 
%%          (record file_data)
%%      meta_info: extra info about the file, e.g. if the file represents a 
%%         physical file or is a "filization" of some other concept.
%% ----------------------------------------------------

-record(abstract_file,{file_data=#file_data{},meta_info=#meta_info{}}).

%% ----------------------------------------------------
%% data type: file system, a representation of a file system
%%  where:
%%      files_by_structure: a dict of dicts mirroring the hierarchial
%%          structure of the file system. The dict keys are strings with
%%          names of files ("directory entries"), and atoms with file prop-
%%          erties such as owner, name of the directory/file and so on.
%%          A thought with this is to support filesystems where files can 
%%          contain other files, i.e. one without a distinction between 
%%          directories and files. (dict of dicts (one per file))
%%      files_by_inode: a tree containing the inodes of the files.
%%          (gb_tree)
%%      
%% ----------------------------------------------------

-record(file_system,{files_by_structure,files_by_inode}).        
