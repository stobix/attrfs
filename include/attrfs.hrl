%%%=========================================================================
%%%                                 LICENSE
%%%=========================================================================
%%%
%%%  This program is free software; you can redistribute it and/or modify
%%%  it under the terms of the GNU General Public License as published by
%%%  the Free Software Foundation; either version 2 of the License, or
%%%  (at your option) any later version.
%%%
%%%  This program is distributed in the hope that it will be useful,
%%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%  GNU Library General Public License for more details.
%%%
%%%  You should have received a copy of the GNU General Public License
%%%  along with this program; if not, write to the Free Software
%%%  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%%
%%%=========================================================================

%%%-------------------------------------------------------
%%% @author Joel Ericson <kasettbok@gmail.com>
%%%
%%% @version Almost working, yo...
%%% @doc Header file for the attrfs modul.
%%% 
%%% @end
%%%-------------------------------------------------------


-ifdef(test).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(ATTR_DB, attributes).
-define(ATTR_DB_FILE, attrfs, attributes_db).

-define(ROOT_FOLDR, root).
-define(REAL_FOLDR, (attr_tools:get_or_default(real_name,"real"))).
-define(ATTR_FOLDR, []).
-define(ATTR_FOLDR_FS_NAME, (attr_tools:get_or_default(attr_name,"attribs"))).
-define(ALL_FOLDR,(attr_tools:get_or_default(all_name,"all_files"))).
-define(DUP_FOLDR,(attr_tools:get_or_default(dup_name,"duplicates"))).
-define(AND_FOLDR,(attr_tools:get_or_default(and_name,"AND"))).
-define(OR_FOLDR,(attr_tools:get_or_default(or_name,"OR"))).
-define(BUTNOT_FOLDR,(attr_tools:get_or_default(butnot_name,"BUTNOT"))).


-include_lib("kernel/include/file.hrl"). %for record file_info,type io_string()
-include_lib("fuserl/include/fuserl.hrl"). % for #stat{}

-define(UEXEC(X), (X#stat{st_mode=(X#stat.st_mode bor 8#100)})).

-define(STD_DIR_MODE, (?S_IFDIR bor 8#755)).

-record(initargs,
        {dir::string()}).

-type inode_number()::pos_integer().
-type attrib_list()::[{string(), string()}].

%The dupliacte_file record contains the file data for files containing duplicate info for the file whose name they bear.

-record(duplicate_file,
        {d_contents::[{string(),string()}]
        }).

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

-type ltype()::lnot| % this only resides in the top-level attribute folder, for now. Another solution would be to let not include the parent dir and the parent parent dir, so {not, "and", "Foo"} would mean the same that andnot mean here below.
                    % these are key-level conjunctions
                    land|
                    lor|
                    landnot|
                    lornot|
                    % these are value-level conjunctions
                    {p,land}|
                    {p,lor}|
                    {p,landnot}|
                    {p,lornot}.

-record(dir_link,
    {link::inode_number() % the inode linked to.
    }).

-define(KEY_SEP,"/").
-define(VAL_SEP,",").


-type parent()::name().
%% An internal file is a file without an external representation.
%% An external file or dir exists in an external file system somewhere.
%% An ext info dir is an internal directory representation of some attribute of some dir or file.
%% A logic dir is specified by its inode entry name, and is used to filter searches by dir browsing.
-type file_type()::#external_file{}|internal_file|#external_dir{}|attribute_dir|internal_dir|logic_dir|#dir_link{}|duplicate_file.
-type ext_io_tuple()::{non_neg_integer(),file:io_string()}.


-type name_tuple()::{string(),inode_number(),file_type()}.
-type name_list()::[name_tuple()].



%% The name slot in the inode_entry record is normally a string, but is a {parentname,childname} for value dirs, and a {grandparentname,parentname,childname} for logical dirs.
-type inode_entry_name()::string()|[string()]|atom().


-record(inode_entry,
        % This file system uses a one-to-one correspondence between names and inodes, so that move, link and copy can produce the same results.
        % TODO: Deal with the case that several files in external dirs have the same name.
        % This is the unique name that the inode server wants, that is
        % the name of the file, unless we have a value dir, in case it is
        % {Key,ValueName}, where ValueName is the name shown to the fuse 
        % file system server.
        {
        % The unique name the file uses internally. 
        % Name, for normal files (How do I fix duplicates?)
        % {Parent, Name} for Attribute value dirs
        % {{Grandparent,Parent},Name} for logical dirs.
        % maybe {Ggp,Gp,P,N} for parentheses foo/or/(/a/AND/B/)
        name::inode_entry_name() 
        % children are the children of the file/dir
        ,children::name_list()
        % file_type tells me what kind of file this is. This includes more types than #file_info.type
        ,type::file_type()
        % stat is the file info that the file has in my file system.
        ,stat::#stat{} % fuserl:#stat{}
        % ext_info contains a list of attribute - value pairs in xattr style, used to put files into virtual folders.
        ,ext_info::attrib_list()
        ,ext_io::ext_io_tuple()
        % this is a list of all links that point to this entry.
        ,links::[inode_number()]
        ,generated::boolean()
        }).


-record(open_external_file,
        {io_device::file:io_device(),
         path::string()
        }).


-type attribute_entries()::[name_tuple()|{inode,inode_number()}].

-record(state,
        {
        open_files%%::#gb_trees{} of #direntry{} with inode_number() keys 
        }).

-define (DIR (Stat), Stat#stat{ st_mode = (Stat#stat.st_mode band 8#777) bor ?S_IFDIR }).
%-define (DIR (Stat), Stat).

-define (ENTRY2PARAM (Entry,Inode), 
    #fuse_entry_param{ ino=Inode,
                       generation=0,
                       attr=Entry#inode_entry.stat,
                       attr_timeout_ms=1000,
                       entry_timeout_ms=1000}).


%these I stole from fuserlproc. Maybe they'll come in handy.
%-define (DIRATTR (X), #stat{ st_ino = (X), 
%                             st_mode = ?S_IFDIR bor 8#0555, 
%                             st_nlink = 1 }).
%
%
%-define(STAT (X,Y,Z), #stat{ st_ino = (X),
%                             st_mode = (Y),
%                             st_nlink = Z
%                                 }.
