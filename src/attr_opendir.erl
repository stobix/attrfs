-module(attr_opendir).

-include("../include/attrfs.hrl").
-include("../include/debug.hrl").

-export([direntries/1]).

%%--------------------------------------------------------------------------
%% Gets the children for the inode from the inode list, and runs direntrify
%% on it.
%%--------------------------------------------------------------------------
direntries(Inode) ->
  ?DEB1("    Creating direntries"),
  ?DEB1("     Getting child entries"),
  {value,Children}=attr_lookup:children(Inode),
  ?DEBL("     Converting children ~p for ~p",[Children,Inode]),
  direntrify(Children).


%%--------------------------------------------------------------------------
%% direntrify takes a [{Name,Inode,Type}] and returns a [fuserl:#{direntry}]
%%--------------------------------------------------------------------------
direntrify([]) -> 
  ?DEB1("    Done converting children"),
  [];

direntrify([{Name,Inode,Type}|Children]) ->
  ?DEB2("    Getting entry for child ~p",{Name,Inode,Type}),
  {value,Child}=tree_srv:lookup(Inode,inodes),
  ?DEB2("    Getting permissions for child ~p",{Name,Inode,Type}),
  ChildStats=Child#inode_entry.stat,
  ?DEB2("    Creating direntry for child ~p",{Name,Inode,Type}),
  Direntry= #direntry{name=Name ,stat=ChildStats },
  ?DEB2("    Calculatig size for direntry for child ~p",{Name,Inode,Type}),
  Direntry1=Direntry#direntry{offset=fuserlsrv:dirent_size(Direntry)},
  ?DEB2("    Appending child ~p to list",{Name,Inode,Type}),
  [Direntry1|direntrify(Children)].



