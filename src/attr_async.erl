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
%%% @doc This module contains all the functions that are spawned processes from when 
%%%      making async calls from attrfs_srv.
%%%
%%% @end

-module(attr_async).

-include("../include/attrfs.hrl").
-include_lib("newdebug/include/newdebug19.hrl").

-export([getattr/2,
         getxattr/4,
         lookup/3,
         access/4
         ]).

getattr(1,Token) ->
    {value,Entry} = tree_srv:lookup(1,specials),
    Reply=?ENTRY2REPLY_ATTR(Entry),
    attr_reply:reply(Token,Reply);

getattr(Inode,Token) ->
  ?DEB2({async,3},">getattr_internal inode:~w",Inode),
  case tree_srv:lookup(Inode,inodes) of
    none ->
      ?DEB1({async,5},"Non-existent file"),
      Reply=#fuse_reply_err{err=enoent};
    {value,Entry} ->
      ?DEB1({async,5},"File exists, returning info"),
      Reply=?ENTRY2REPLY_ATTR(Entry);
    _A -> 
      ?DEB2({async,5},"This should not be happening: ~p",_A),
      Reply=#fuse_reply_err{err=enotsup}
  end,
  ?DEB1({async,5},"Sending reply"),
  attr_reply:reply(Token,Reply).



getxattr(Inode,RawName,Size,Token) ->
  ?DEBL({async,3},">getxattr ~p ~p ~p ~p",[Inode,RawName,Size,Token]),
  Name=RawName,
  % FIXME do something about the "system" getxattr calls?
  % Mainly seems to be system.posix_acl_access and system.posix_acl_default
  {value,Entry}=tree_srv:lookup(Inode,inodes),
  ?DEB1({async,4},"Got inode entry"),
  ExtInfo=Entry#inode_entry.ext_info,
  ?DEB1({async,4},"Got extinfo"),
  Reply=case lists:keyfind(Name,1,ExtInfo) of
    {Name,ExtInfoValue} ->
      ?DEB1({async,4},"Got attribute value"),
      ExtAttrib=ExtInfoValue, %Seems I shouldn't 0-terminate the strings here.
      ExtSize=size(ExtAttrib),
      ?DEBL({async,4},"Converted attribute and got (~w,~w)",[ExtAttrib,ExtSize]),
      case Size == 0 of 
        true -> ?DEB1({async,4},"They want to know our size."),#fuse_reply_xattr{count=ExtSize};
        false -> 
          case Size < ExtSize of
            true -> ?DEBL({async,4},"They are using too small a buffer; ~w < ~w ",[Size,ExtSize]),#fuse_reply_err{err=erange};
            false -> ?DEB1({async,4},"All is well, replying with attrib value."),#fuse_reply_buf{buf=ExtAttrib, size=ExtSize}
          end
      end;
    false ->
      ?DEB1({async,4},"Argument nonexistent, returning error"),
      #fuse_reply_err{err=enodata}
  end,
  attr_reply:reply(Token,Reply).

lookup(ParentInode,Child,Token) ->
  Reply=
    case attr_lookup:children(ParentInode) of
      {value,Children} ->
        ?DEBL({async,4},"Got children for ~p",[ParentInode]),
        case lists:keysearch(Child,1,Children) of
          {value,{_,Inode,_}} ->
            ?DEB2({async,4},"Found child ~p",Child),
            {value,Entry} = tree_srv:lookup(Inode,inodes),
            ?DEB1({async,4},"Got child inode entry, returning..."),
            #fuse_reply_entry{fuse_entry_param=?ENTRY2PARAM(Entry,Inode)};
          false ->
            ?DEBL({async,4},"Child ~p not present in ~p!",[Child,ParentInode]),
            #fuse_reply_err{err=enoent} % child nonexistent.
        end;
      none -> 
        ?DEB1({async,4},"Parent nonexistent!"),
        #fuse_reply_err{err=enoent} %no parent
    end,
  attr_reply:reply(Token,Reply).


access(Ctx,Inode,Mask,Token) ->
  Reply=attr_tools:test_access(Inode,Mask,Ctx),
  attr_reply:reply(Token,#fuse_reply_err{err=Reply}).
  %fuserlsrv:reply(Continuation,#fuse_reply_err{err=Reply}).
