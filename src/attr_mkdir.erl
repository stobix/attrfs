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
-include("../include/debug.hrl").

-export([make_dir/6]).

make_dir(_Ctx,_ParentInode,_ParentName,#external_dir{},_Name,_Mode) ->
  ?DEB1("   external dir, not supported"),
  #fuse_reply_err{err=enotsup};


make_dir(Ctx,ParentInode,ParentName,DirType=#attribute_dir{},Name,Mode) ->
  AttrDirType=DirType#attribute_dir.atype,
  make_attr_child_dir(Ctx,ParentInode,AttrDirType,ParentName,Name,Mode);


make_dir(Ctx,ParentInode,ParentName,internal_dir,Name,Mode) ->
  make_internal_child_dir(Ctx,ParentInode,ParentName,Name,Mode).

%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
make_attr_child_dir(Ctx,ParentInode,value,ValueParentName,Name,Mode) ->
  ?DEB1("   NOT creating a subvalue dir"),
  MyName={ValueParentName,Name},
  MyInode=inode:get(MyName,ino),
  Stat=make_general_dir(Ctx,ParentInode,MyInode,MyName,Mode,#attribute_dir{atype=value}),
  #fuse_reply_entry{
    fuse_entry_param=
      #fuse_entry_param{
        ino=MyInode,
        generation=1,
        attr=Stat,
        attr_timeout_ms=1000,
        entry_timeout_ms=1000
      }
  }.
  %this is where I create a subvalue dir, when these are supported.
  % subvalue dir name {{Key,Val},Name}, I guess.
  % Inode=make_general_dir(Ctx,ParentInode,Name,Mode),

make_attr_child_dir(Ctx,ParentInode,key,Key,Name,Mode) ->
  ?DEB1("   creating an attribute value dir"),
  % create value directory here.
  MyInode=inode:get({Key,Name},ino),
  Stat=make_general_dir(Ctx,ParentInode,MyInode,{Key,Name},Mode,#attribute_dir{atype=value}),
  ?DEB1("   returning new dir"),
  #fuse_reply_entry{
    fuse_entry_param=
      #fuse_entry_param{
        ino=MyInode,
        generation=1,
        attr=Stat,
        attr_timeout_ms=1000,
        entry_timeout_ms=1000
      }
  }.

make_internal_child_dir(_Ctx,_ParentInode,root,_Name,_Mode) ->
  ?DEB1("   creating of new directory type not supported"),
  #fuse_reply_err{err=enotsup};

make_internal_child_dir(_Ctx,_ParentInode,?REAL_FOLDR,_Name,_Mode) ->
  ?DEB1("   creating of external dirs not supported"),
  #fuse_reply_err{err=enotsup};

make_internal_child_dir(Ctx,ParentInode,?ATTR_FOLDR,Name,Mode) ->
  % This is where I add an attribute key folder.
  ?DEB1("   creating an attribute key dir"),
  MyInode=inode:get(Name,ino),
  Stat=make_general_dir(Ctx,ParentInode,MyInode,Name,Mode,#attribute_dir{atype=key}),
  ?DEB1("   returning new dir"),
  tree_srv:enter(Name,MyInode,keys),
  #fuse_reply_entry{
    fuse_entry_param=
      #fuse_entry_param{
        ino=MyInode,
        generation=0,
        attr=Stat,
        attr_timeout_ms=1000,
        entry_timeout_ms=1000
      }
  };

make_internal_child_dir(_Ctx,_ParentInode,_,_Name,_Mode) ->
  #fuse_reply_err{err=enotsup}.

%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
make_general_dir(Ctx,ParentInode,MyInode,Name,Mode,DirType) ->
  ?DEBL("   creating new directory entry called ~p",[Name]),
  {MegaNow,NormalNow,_} = now(),
  Now=MegaNow*1000000+NormalNow,
  ?DEBL("   atime etc: ~p",[Now]),
  #fuse_ctx{uid=Uid,gid=Gid}=Ctx,
  DirStat=
    #stat{
      st_mode=Mode,
      st_ino=MyInode,
%        st_nlink=1, % FIXME: Make this accurate.
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
    children=[]
  },
  insert_entry(ParentInode,DirEntry),
  DirStat.

%%--------------------------------------------------------------------------
%insert entry Entry with into the file system tree under ParentInode. Returns new inode.
%%--------------------------------------------------------------------------
insert_entry(ParentInode,ChildEntry) ->
  ?DEBL("    inserting new entry as child for ~p",[ParentInode]),
  {value,ParentEntry}=tree_srv:lookup(ParentInode,inodes),

  InoName=ChildEntry#inode_entry.name,
  ChildInode=inode:get(InoName,ino),
  ChildName=
    case ChildEntry#inode_entry.type of
      #attribute_dir{atype=value} ->
        {_,Name} = InoName,
        Name;
      _ -> InoName
    end,
  NewChildren=[{ChildName,ChildInode}|ParentEntry#inode_entry.children],
  tree_srv:enter(ParentInode,ParentEntry#inode_entry{children=NewChildren},inodes),
  tree_srv:enter(ChildInode,ChildEntry,inodes),
  ChildInode.


