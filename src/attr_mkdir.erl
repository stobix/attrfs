-module(attr_mkdir).

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
-include_lib("newdebug/include/debug.hrl").

-export([make_dir/6]).
-export([insert_entry/2]).

make_dir(Ctx,ParentInode,ParentName,attribute_dir,Name,Mode) ->
  make_attr_child_dir(Ctx,ParentInode,ParentName,Name,Mode);

make_dir(_Ctx,_ParentInode,_ParentName,_DirType,_Name,_Mode) ->
  ?DEB2(3,"~p , not supported",_DirType),
  #fuse_reply_err{err=enotsup}.


%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------

make_attr_child_dir(Ctx,ParentInode,ParentName,Name,Mode) ->
  ?DEB1(6,"Creating an attribute dir"),
  MyName=[Name|ParentName],
  case make_general_dir(Ctx,ParentInode,MyName,Mode,attribute_dir) of
    {ok,Stat} ->
      Param=make_param(Stat),
      #fuse_reply_entry{
        fuse_entry_param=Param
      };
    _ ->
      #fuse_reply_err{err=eexist}
  end.

make_param(Stat) ->
  #fuse_entry_param{
    ino=Stat#stat.st_ino,
    generation=1,
    attr=Stat,
    attr_timeout_ms=?TIMEOUT_MS,
    entry_timeout_ms=?TIMEOUT_MS
  }.

%%--------------------------------------------------------------------------
%% Creates a dir inode entry of type DirType and inserts it into the inode table as a child of ParentInode; Creates and returns dirstats with actual time info, and the UID and GID provided in CTX and the mode provided in Mode.
%%--------------------------------------------------------------------------
make_general_dir(Ctx,ParentInode,Name,Mode,DirType) ->
  ?DEB1(6,"make_general_dir"),
  case inode:is_numbered(Name,ino) of
    false ->
      {ok,MyInode}=inode:number(Name,ino),
      ?DEBL(8,"creating new directory entry called ~p",[Name]),
      {MegaNow,NormalNow,_}=now(),
      Now=MegaNow*1000000+NormalNow,
      ?DEBL(8,"atime etc: ~p",[Now]),
      #fuse_ctx{uid=Uid,gid=Gid}=Ctx,
      DirStat=
        #stat{
          st_mode=Mode,
          st_ino=MyInode,
          st_nlink=1,
          st_uid=Uid,
          st_gid=Gid,
          st_atime=Now,
          st_ctime=Now,
          st_mtime=Now
        },
      DirEntry=#inode_entry{
        name=Name,
        stat=DirStat,
        ext_info=[],
        ext_io=attr_ext:ext_info_to_ext_io([]),
        type=DirType,
        contents=[]
      },
      insert_entry(ParentInode,DirEntry),
      {ok,DirStat};
    _ ->
      ?DEB2(6,"~p already has an inode! Not creating duplicate folder...",Name),
      error
  end.

%%--------------------------------------------------------------------------
%insert entry Entry with into the file system tree under ParentInode. Returns new inode.
%%--------------------------------------------------------------------------
insert_entry(ParentInode,ChildEntry) ->
  ?DEBL(2,"inserting new entry as child for ~p",[ParentInode]),
  {value,ParentEntry}=tree_srv:lookup(ParentInode,inodes),

  InoName=ChildEntry#inode_entry.name,
  {ok,ChildInode}=inode:n2i(InoName,ino),
  ChildType=ChildEntry#inode_entry.type, 
  ChildName=
    case ChildType of
      attribute_dir ->
        [Name|_Parents] = InoName,
        Name;
      _ -> InoName
    end,
  NewChildren=[{ChildName,ChildInode,ChildType}|ParentEntry#inode_entry.contents],
  tree_srv:enter(ParentInode,ParentEntry#inode_entry{contents=NewChildren},inodes),
  tree_srv:enter(ChildInode,ChildEntry,inodes),
  ChildInode.


