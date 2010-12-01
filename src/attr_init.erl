-module(attr_init).

-export([init/1]).

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

%%%=========================================================================
%%%                                  META
%%%=========================================================================
%%% @author Joel Ericson <joel.mikael.ericson@gmail.com>
%%%
%%% @copyright Copylefted using some GNU license or other.
%%%
%%% @version 1.0

-include("../include/attrfs.hrl").
-include("../include/debug.hrl").

init({MirrorDirs,DB}) ->
  initiate_servers(DB),
  % getting inodes for base folders
  RootIno=inode:get(root,ino),
  AttribIno=inode:get(?ATTR_FOLDR,ino),
  RealIno=inode:get(?REAL_FOLDR,ino),
  AllIno=inode:get(?ALL_FOLDR,ino),

%  ?DEBL("   inodes;\troot:~p, real:~p, attribs:~p",[RootIno,RealIno,AttribIno]),
  ?DEB1("   creating root entry"),
  % creating base folders.

  AttributeEntry=
    #inode_entry{
      name=[],
      children=[],
      type=attribute_dir,
      stat=#stat{ 
          % For now I'll set all access here, and limit access on a per-user-basis.
          % Maybe even make this folder "magic", so that different users think that they own it?
          % More on this when I start using the Ctx structure everywhere.
          st_mode=8#777 bor ?S_IFDIR,
          st_ino=AttribIno
        },
      ext_info=[],
      ext_io=attr_ext:ext_info_to_ext_io([])
    },
  tree_srv:enter(AttribIno,AttributeEntry,inodes),
  ?DEB1("  Traversing all mirror dirs"),
  % This mirrors all files and folders, recursively, from each of the the external folders MirrorDirN to the internal folder ?REAL_FOLDR/MirrorDirN, adding attribute folders with appropriate files when a match between external file and internal database entry is found.
  {RealChildren,AllChildren}=lists:mapfoldl(
    fun(MirrorDir,Children) ->
      ?DEB2("   mirroring dir ~p",MirrorDir),
      {ChildName,ChildIno,ChildType,ChildChildren}=make_inode_list({MirrorDir,filename:basename(MirrorDir)}),
      Child={ChildName,ChildIno,ChildType},
      ?DEB2("   got ~p",Child),
      {Child,ChildChildren++Children}
    end,
    [],
    MirrorDirs),
  AllEntry=
    #inode_entry{
      name=?ALL_FOLDR,
      children=AllChildren,
      type=internal_dir,
      stat=
        #stat{
          st_mode=8#555 bor ?S_IFDIR,
          st_ino=AllIno
        },
      ext_info=[],
      ext_io=attr_ext:ext_info_to_ext_io([])
    },
  tree_srv:enter(AllIno,AllEntry,inodes),
  RealEntry=
    #inode_entry{
      name=?REAL_FOLDR,
      children=[{?ALL_FOLDR,AllIno,internal_dir}|RealChildren],
      type=internal_dir,
      stat=
        #stat{
          st_mode=8#555 bor ?S_IFDIR,
          st_ino=RealIno
        },
      ext_info=[],
      ext_io=attr_ext:ext_info_to_ext_io([])
    },
  tree_srv:enter(RealIno,RealEntry,inodes),
  RootEntry=
    #inode_entry{
      name=root,
      children=[{?REAL_FOLDR,RealIno,internal_dir},{?ATTR_FOLDR_FS_NAME,AttribIno,attribute_dir}],
      type=internal_dir, 
      stat=
        #stat{
          st_mode=8#755 bor ?S_IFDIR,
          st_ino=RootIno
        },
      ext_info=[],
      ext_io=attr_ext:ext_info_to_ext_io([])
    },
  ?DEB1("   updating root inode entry"),
  tree_srv:enter(RootIno,RootEntry,inodes).

initiate_servers(DB) ->
  % initiating databases, servers and so on.
  ?DEBL("   opening attribute database file ~p as ~p", [DB, ?ATTR_DB]),
  {ok,_}=dets:open_file(?ATTR_DB,[{type,bag},{file,DB}]),
  tree_srv:new(inodes), % contains inode entries
  attr_open:init(),
  tree_srv:new(filter), % gives info on how each Ctx has its attribute folder contents filtered by logical dirs
  ?DEB1("   created inode and key trees"),
  inode:initiate(ino), % the inode table
  inode:initiate(fino). % the open files "inode" table

type_and_children(Path,FileInfo) ->
        case FileInfo#file_info.type of
          directory ->
            ?DEB1("    directory"),
            {ok,ChildNames}=file:list_dir(Path),
            ?DEB2("     directory entries:~p",ChildNames),
            {NameInodePairs,MultitudeOfChildren}=
              lists:mapfoldl(
                fun(ChildName,OtherChildren) -> 
                  % I make the recursion here, so I easily will be able to both check for duplicates and return a correct file type to the child.
                  {InternalChildName,Ino,Type,AllChildren}=make_inode_list({Path++"/"++ChildName,ChildName}),
                  Me={InternalChildName,Ino,Type},
                  case Type of
                    #external_file{} ->
                      {Me,[Me|OtherChildren++AllChildren]};
                    _ ->
                      {Me,OtherChildren++AllChildren}
                  end
                end, 
                [],
                ChildNames
              ),
            {ok,NameInodePairs,#external_dir{external_file_info=FileInfo,path=Path},MultitudeOfChildren};
          regular ->
            ?DEB1("    regular"),
            {ok,[],#external_file{external_file_info=FileInfo,path=Path},[]};
          _ ->
            {error,not_supported} % for now
        end.


get_unique(Name0) ->
%    {Name0,inode:get(Name0,ino)}.

          case inode:number(Name0,ino) of
              {error,{is_numbered,{Name0,_Ino}}} ->
                  ?DEB2("~p is a duplicate!",Name0),
                  get_unique(string:concat("duplicate-",Name0));
              {ok,Ino} ->
                  ?DEB2("~p is not a duplicate!",Name0),
                  {Name0,Ino}
          end.

make_inode_list({Path,Name0}) ->
  ?DEBL("   reading file info for ~p into ~p",[Path,Name0]),
  case catch file:read_file_info(Path) of
    {ok,FileInfo} ->
      ?DEB1("   got file info"),
      {Name,Ino} =get_unique(Name0),
      ?DEB2("   got unique name ~p",Name),
      {ok,Children,Type,AllChildren}= type_and_children(Path,FileInfo),
      ?DEB2("    Generating ext info for ~p",Path),
      {ExtInfo,ExtIo}=attr_ext:generate_ext_info_io(Path), 
      ?DEB2("     ext info: ~p", ExtInfo),
      % XXX: This will break if provided with a local date and time that does not
      % exist. Shouldn't be much of a problem.
      EpochAtime=lists:nth(1,calendar:local_time_to_universal_time_dst(FileInfo#file_info.atime)),
      EpochCtime=lists:nth(1,calendar:local_time_to_universal_time_dst(FileInfo#file_info.ctime)),
      EpochMtime=lists:nth(1,calendar:local_time_to_universal_time_dst(FileInfo#file_info.mtime)),
      ?DEB2("    atime:~p~n",EpochAtime),
      ?DEB2("    ctime:~p~n",EpochCtime),
      ?DEB2("    mtime:~p~n",EpochMtime),
      MyStat=
        attr_tools:statify_file_info(
          FileInfo#file_info{
            inode=Ino,
            atime=EpochAtime,
            ctime=EpochCtime,
            mtime=EpochMtime
          }
        ),
      InodeEntry=
        #inode_entry{ 
          name=Name,
          children=Children,
          type=Type,
          stat=MyStat,
          ext_info=ExtInfo,
          ext_io=ExtIo
        },
      tree_srv:enter(Ino,InodeEntry,inodes),
      ?DEB2("    looking up ext folders for ~p",Name),
      % I need to go from [[String]] to [String] here...
      ExtFolders=attr_tools:flatten1(dets:match(?ATTR_DB,{Path,'$1'})),
      ?DEBL("    creating ext folders ~p for ~p",[ExtFolders,Name]),
      lists:foreach(
        fun(Attr) -> 
          attr_ext:append_attribute(Attr,Name,?UEXEC(MyStat)) 
        end,
        ExtFolders),
    {Name,Ino,Type,AllChildren};
    E ->
      ?DEBL("   got ~p when trying to read ~p.",[E,Path]),
      ?DEB1("   are you sure your app file is correctly configured?"),
      ?DEB1(">>>exiting<<<"),
      exit(E)
  end.
