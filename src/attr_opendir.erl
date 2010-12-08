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
  ?DEBL("     Converting children ~w for ~w",[Children,Inode]),
  direntrify(Children).


%%--------------------------------------------------------------------------
%% direntrify takes a [{Name,Inode,Type}] and returns a [fuserl:#{direntry}]
%%--------------------------------------------------------------------------

direntrify(List) ->
  direntrify(List,1).

direntrify([],N) -> 
  ?DEB2("    Done converting ~w children",N),
  [];

direntrify([{Name,Inode,_Type}|Children],N) ->
%  ?DEB2("    Getting entry for child ~w",{Name,Inode,_Type}),
  {value,Child}=tree_srv:lookup(Inode,inodes),
%  ?DEB2("    Getting permissions for child ~w",{Name,Inode,_Type}),
  ChildStats=Child#inode_entry.stat,
%  ?DEB2("    Creating direntry for child ~w",{Name,Inode,_Type}),
  Direntry= #direntry{name=Name ,stat=ChildStats },
%  ?DEB2("    Calculatig size for direntry for child ~w",{Name,Inode,_Type}),
  Direntry1=Direntry#direntry{offset=N},
  ?DEB2("    Appending child ~w to list",{Name,Inode,_Type}),
  [Direntry1|direntrify(Children,N+1)].

