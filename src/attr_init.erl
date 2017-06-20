

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

-module(attr_init).

-export([init/1]).

-include("../include/attrfs.hrl").
-include_lib("newdebug/include/debug.hrl").

init({MirrorDirs,DB}) ->
  ?DEB1({init,1},">init"),
%  check_dirs(MirrorDirs), TODO: This is smarter than checking for legit dirs when parsing them. Fix later.
  initiate_servers(DB),
  % getting inodes for base folders
  {ok,RootIno}=numberer:number(root,ino),
  {ok,AttribIno}=numberer:number(?ATTR_FOLDR,ino),
  {ok,RealIno}=numberer:number(?REAL_FOLDR,ino),
  {ok,AllIno}=numberer:number(?ALL_FOLDR,ino),
  {ok,DupIno}=numberer:number(?DUP_FOLDR,ino),
  {ok,UniIno}=numberer:number(?UNI_FOLDR,ino),
  {ok,LogicIno}=numberer:number(?LOGIC_FOLDR,ino),

  ?DEBL({init,5},"inodes;\troot:~w, real:~w, attribs:~w",[RootIno,RealIno,AttribIno]),
  ?DEB1({init,1},"creating root entry"),
  % creating base folders.

  LogicEntry=
    #inode_entry{
      name=?LOGIC_FOLDR,
      contents=[],
      type=logic_dir,
      stat=?DIR_STAT(8#555,LogicIno),
      ext_info=[],
      ext_io=attr_ext:ext_info_to_ext_io([])
    },
  tree_srv:enter(LogicIno,LogicEntry,inodes),
  tree_srv:enter(LogicIno,LogicEntry,specials),
  AttributeEntry=
    #inode_entry{
      name=?ATTR_FOLDR,
      contents=[],
      type=attribute_dir,
          % For now I'll set all access here, and limit access on a per-user-basis.
          % Maybe even make this folder "magic", so that different users think that they own it?
          % More on this when I start using the Ctx structure everywhere.
      stat=?DIR_STAT(8#777,AttribIno),
      ext_info=[],
      ext_io=attr_ext:ext_info_to_ext_io([])
    },
  tree_srv:enter(AttribIno,AttributeEntry,inodes),
  tree_srv:enter(AttribIno,AttributeEntry,specials),
  ?DEB1({init,1},"Traversing all mirror dirs"),
  % This mirrors all files and folders, recursively, from each of the the external folders MirrorDirN to the internal folder ?REAL_FOLDR/MirrorDirN, adding attribute folders with appropriate files when a match between external file and internal database entry is found.
  {RealChildren,AllChildren}=lists:mapfoldl(
    fun(MirrorDir,Children) ->
      ?DEB2({init,1},"mirroring dir ~p",MirrorDir),
      {ChildName,ChildIno,ChildType,ChildChildren}=make_inode_list(MirrorDir,filename:basename(MirrorDir)),
      Child={ChildName,ChildIno,ChildType},
      ?DEB2({init,1},"got ~p",ChildName),
      {Child,ChildChildren++Children}
    end,
    [],
    MirrorDirs),
  AllEntry=
    #inode_entry{
      name=?ALL_FOLDR,
      contents=AllChildren,
      type=internal_dir,
      stat=?DIR_STAT(8#555,AllIno),
      ext_info=[],
      ext_io=attr_ext:ext_info_to_ext_io([])
    },
  tree_srv:enter(AllIno,AllEntry,inodes),
  {UniChildren,DupChildren}=make_duplicate_children(),
  DupEntry=
    #inode_entry{
      name=?DUP_FOLDR,
      contents=DupChildren,
      type=internal_dir,
      stat=?DIR_STAT(8#555,DupIno),
      ext_info=[],
      ext_io=attr_ext:ext_info_to_ext_io([])
    },
  tree_srv:enter(DupIno,DupEntry,inodes),
  ?DEB1({init,1},"duplicate folder done"),
  UniEntry=
    #inode_entry{
        name=?UNI_FOLDR,
        contents=UniChildren,
        type=internal_dir,
        stat=?DIR_STAT(8#555,UniIno),
        ext_info=[],
        ext_io=attr_ext:ext_info_to_ext_io([])
    },
  tree_srv:enter(UniIno,UniEntry,inodes),
  ?DEB1({init,1},"unique folder done"),
  RealEntry=
    #inode_entry{
      name=?REAL_FOLDR,
      contents=[
        {?ALL_FOLDR,AllIno,internal_dir},
        {?DUP_FOLDR,DupIno,internal_dir},
        {?UNI_FOLDR,UniIno,internal_dir}|
          RealChildren],
      type=internal_dir,
      stat=?DIR_STAT(8#555,RealIno),
      ext_info=[],
      ext_io=attr_ext:ext_info_to_ext_io([])
    },
  tree_srv:enter(RealIno,RealEntry,inodes),
  RootEntry=
    #inode_entry{
      name=?ROOT_FOLDR,
      contents=[
        {?REAL_FOLDR,RealIno,internal_dir},
        {?LOGIC_FOLDR,LogicIno,logic_dir},
        {?ATTR_FOLDR_FS_NAME,AttribIno,attribute_dir}],
      type=internal_dir, 
      stat=?DIR_STAT(8#755,RootIno),
      ext_info=[],
      ext_io=attr_ext:ext_info_to_ext_io([])
    },
  ?DEB1({init,5},"updating root inode entry"),
  tree_srv:enter(RootIno,RootEntry,inodes),
  tree_srv:enter(RootIno,RootEntry,specials).


initiate_servers(DB) ->
  % initiating databases, servers and so on.
  ?DEBL({init,6},"opening attribute database file ~p as ~p", [DB, ?ATTR_DB]),
  {ok,_}=dets:open_file(?ATTR_DB,[{type,bag},{file,DB}]),
  tree_srv:new(inodes), % contains inode entries
  tree_srv:new(duplicates), % to store {Name,[{Given_name,Path}]} of all duplicate entries.
  tree_srv:new(specials), % to provide fast access to the root dir and other critical dirs
  attr_open:init(),
  ?DEB1({init,8},"created inode and key trees"),
  numberer:initiate(ino), % the inode table
  numberer:initiate(pino), % the spawned processes "inode" table
  numberer:initiate(fino). % the open files "inode" table


% XXX: Do NOT call this on children with a parent without an inode entry ready!  
type_and_children(Path0,FileInfo) ->
  Path=unicode:characters_to_binary(Path0),
        case FileInfo#file_info.type of
          directory ->
            ?DEB1({init,7},"directory"),
            {ok,ChildNames}=file:list_dir_all(Path),
            ?DEB2({init,7},"directory entries:~p",ChildNames),
            {NameInodePairs,AllRecursiveChildren}=
              lists:mapfoldl(
                fun(ChildName,ChildAcc) -> 
                  % I make the recursion here, so I easily will be able to both check for duplicates and return a correct file type to the child.
                  SubPath= << <<P/binary>> || P <- [Path,<<"/">>,unicode:characters_to_binary(ChildName)] >>,
                  {InternalChildName,Ino,Type,ChildChildren}=make_inode_list(SubPath,ChildName),
                  Me={InternalChildName,Ino,Type},
                  case Type of
                    #external_file{} ->
                      {Me,[Me|ChildAcc++ChildChildren]};
                    _ ->
                      {Me,ChildAcc++ChildChildren}
                  end
                end, 
                [],
                ChildNames
              ),
            {ok,NameInodePairs,#external_dir{external_file_info=FileInfo,path=Path},AllRecursiveChildren};
          regular ->
            ?DEB1({init,7},"regular"),
            {ok,[],#external_file{external_file_info=FileInfo,path=Path},[]};
          _ ->
            {error,not_supported} % for now
        end.



%-define(dupname(X),string:concat(string:concat(?DUP_PREFIX,Name0),?DUP_SUFFIX)).
-define(dupname(X), << <<Y/binary>> || Y <- [
        unicode:characters_to_binary(?DUP_PREFIX),
        X,
        unicode:characters_to_binary(?DUP_SUFFIX)] >>).

get_unique(Name0) ->
  case numberer:number(Name0,ino) of
    {error,{is_numbered,{Name0,Ino0}}} ->
      ?DEB2({init,7},"~ts is a duplicate!",Name0),
      case tree_srv:lookup(Ino0,inodes) of
          {value,Entry0} ->
              case Entry0#inode_entry.type of
                #external_file{path=Path0} ->
                  ?DEB2({init,9},"~ts is a file!",Name0),
                  {Name,PossiblePaths,Ino}=get_unique(?dupname(Name0)),
                  ?DEBL({init,9},"adding ~p to ~p",[{Name0,Path0},PossiblePaths]),
                  {Name,[{Name0,Path0}|PossiblePaths],Ino};
        %        #external_dir{path=Path0} ->
        %          ?DEB2({init,9},"~p is a dir!",Name0),
        %          {Name,PossiblePaths,Ino}=get_unique(?dupname(Name0)),
        %          ?DEBL({init,9},"adding ~p to ~p",[{Name0,Path0},PossiblePaths]),
        %          {Name,[{Name0,Path0}|PossiblePaths],Ino};
                _ ->
                  %We don't care if external dirs have duplicate names
                  get_unique(?dupname(Name0))
              end;
        _ ->
            %We don't care if external dirs have duplicate names
            get_unique(?dupname(Name0))
    end;
    {ok,Ino} ->
      ?DEB2({init,7},"~p is not a duplicate!",Name0),
      {Name0,[],Ino};
    Other ->
      ?DEB2({init,err},"Got ~p when expecting an ino or error.",Other),
      throw({"Got",Other})
  end.

make_inode_list(Path,Name) when is_list(Path) ->
  make_inode_list(unicode:characters_to_binary(Path),Name);

make_inode_list(Path,Name) when is_list(Name)->
  make_inode_list(Path,unicode:characters_to_binary(Name));

make_inode_list(Path,Name0) when is_binary(Path), is_binary(Name0)->
  ?DEBL({init,6},"reading file info for ~ts into ~ts",[Path,Name0]),
  case catch file:read_file_info(Path) of
    {ok,FileInfo} ->
       
      
      % TODO Kolla filtyp här, så jag kan generera entry för kataloger innan jag rekurserar ner i filträdet.

      ?DEB1({init,8},"got file info"),
      % Skall helst inte rekursera!
      {Name,OlderEntries,Ino} =get_unique(Name0),
      ?DEB2({init,8},"got unique name ~p",Name),
      ?DEB1({init,8},"updating duplicate list"),
      tree_srv:enter(Name0,[{Name,Path}|OlderEntries],duplicates),
      {ok,Children,Type,AllChildren}= type_and_children(Path,FileInfo),
      ?DEB2({init,6},"Generating ext info for ~p",Path),
      {ExtInfo,ExtIo,ExtAmount}=attr_ext:generate_ext_info_io(Path), 
      ?DEB2({init,8},"ext info: ~p", ExtInfo),
      % XXX: This will break if provided with a local date and time that does not
      % exist. Shouldn't be much of a problem.
      EpochAtime=lists:nth(1,calendar:local_time_to_universal_time_dst(FileInfo#file_info.atime)),
      EpochCtime=lists:nth(1,calendar:local_time_to_universal_time_dst(FileInfo#file_info.ctime)),
      EpochMtime=lists:nth(1,calendar:local_time_to_universal_time_dst(FileInfo#file_info.mtime)),
      ?DEB2({init,8},"atime:~w~n",EpochAtime),
      ?DEB2({init,8},"ctime:~w~n",EpochCtime),
      ?DEB2({init,8},"mtime:~w~n",EpochMtime),
      MyStat0=
        attr_tools:statify_file_info(
          FileInfo#file_info{
            inode=Ino,
            atime=EpochAtime,
            ctime=EpochCtime,
            mtime=EpochMtime
          }
        ),
      MyStat=MyStat0#stat{st_nlink=ExtAmount+1},
      InodeEntry=
        #inode_entry{ 
          name=Name,
          contents=Children,
          type=Type,
          stat=MyStat,
          ext_info=ExtInfo,
          ext_io=ExtIo
        },
      tree_srv:enter(Ino,InodeEntry,inodes),
      ?DEB2({init,8},"looking up ext folders for ~p",Name),
      % I need to go from [[String]] to [String] here...
      ExtFolders=attr_tools:flatten1(dets:match(?ATTR_DB,{Path,'$1'})),
      ?DEBL({init,8},"creating ext folders ~p for ~p",[ExtFolders,Name]),
      lists:foreach(
        fun(Attr) -> 
          attr_ext:append_attribute(Attr,Name,?UEXEC(MyStat)) 
        end,
        ExtFolders),
      {Name,Ino,Type,AllChildren};
    E ->
      ?DEBL(err,"got ~w when trying to read ~p.",[E,lists:flatten(Path)]),
      ?DEB1(err,"are you sure your app file is correctly configured?"),
      ?DEB1(err,">>>exiting<<<"),
      exit(E)
  end.


merge_binary(A,B) when is_list(B) ->
  merge_binary(A,unicode:characters_to_binary(B));

merge_binary(A,B) ->
  << <<X/binary>> || X <-[A,B] >>.

make_duplicate_children() ->
  ?DEB1({init,3},"Generating duplicates dir"),
  DupExt=?DUP_EXT, % One options lookup is enough.
  lists:foldl(
    fun({Name,List0},{Unis,Dups}) ->
      ?DEBL({init,8},"Making inode number for {duplicate,~p}",[Name]),
      {ok,Ino}=numberer:number({duplicate,Name},ino),
      Type= duplicate_file,
      IOList = 
        case length(List0) of
          1 -> element(2,lists:nth(1,List0)); % skip the local name if we only have one entry to show
          _ -> lists:foldl(
                fun({DupName,Path},Acc) ->
                  [io_lib:format("~ts :\n~ts\n",[DupName,Path])|Acc]
                end
                ,[]
                ,List0)
        end,
      Data=unicode:characters_to_binary(IOList),
      Entry=#inode_entry{
        type=Type,
        contents=Data,
        ext_info=[],
        ext_io=attr_ext:ext_info_to_ext_io([]),
        stat=?FILE_STAT(8#755,Ino,size(Data))
        },
      ?DEB1({init,8},"entering dup entry into inode list"),
      tree_srv:enter(Ino,Entry,inodes),
      ?DEB1({init,8},"done"),
      DupEntry={merge_binary(Name,DupExt),Ino,#duplicate_file{}},
      case length(List0) of
        1 -> {[DupEntry|Unis],Dups};
        _ -> {Unis,[DupEntry|Dups]}
      end
    end,
    {[],[]},
    tree_srv:to_list(duplicates)
      ).

