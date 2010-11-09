-module(attr_lookup).

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

-export([direntries/1,
        children/1]).

-export([link_ino/1]).

-define(CONNS(X),((X=="AND") or (X=="OR") or (X=="BUTNOT"))).

%%--------------------------------------------------------------------------
%% Gets the children for the inode from the inode list, and runs direntrify
%% on it.
%%--------------------------------------------------------------------------
direntries(Inode) ->
  ?DEB1("    Creating direntries"),
  ?DEB1("     Getting child entries"),
  {value,Children}=attr_tools:children(Inode),
  ?DEBL("     Converting children ~p for ~p",[Children,Inode]),
  direntrify(Children).


%%--------------------------------------------------------------------------
%% direntrify takes a [{Name,Inode}] and returns a [fuserl:#{direntry}]
%%--------------------------------------------------------------------------
direntrify([]) -> 
  ?DEB1("    Done converting children"),
  [];

direntrify([{Name,Inode}|Children]) ->
  ?DEB2("    Getting entry for child ~p",{Name,Inode}),
  {value,Child}=tree_srv:lookup(Inode,inodes),
  ?DEB2("    Getting permissions for child ~p",{Name,Inode}),
  ChildStats=Child#inode_entry.stat,
  ?DEB2("    Creating direntry for child ~p",{Name,Inode}),
  Direntry= #direntry{name=Name ,stat=ChildStats },
  ?DEB2("    Calculatig size for direntry for child ~p",{Name,Inode}),
  Direntry1=Direntry#direntry{offset=fuserlsrv:dirent_size(Direntry)},
  ?DEB2("    Appending child ~p to list",{Name,Inode}),
  [Direntry1|direntrify(Children)].




find_conn([]) -> [];
find_conn([A|_As]=Conn) when ?CONNS(A) ->
  Conn;
find_conn([_A|As]) ->
  find_conn(As).
 

%%--------------------------------------------------------------------------
%% children returns the children for the inode, if the inode is 
%% present. Returns like gb_trees:lookup
%%--------------------------------------------------------------------------


%% For sake of efficiency, I let this function be called with either an inode or directly with an #inode_entry
children(#inode_entry{}=Entry) ->
  Name=Entry#inode_entry.name,
  ?DEBL("    got an entry: ~p",[Name]),
  Children=
    case Entry#inode_entry.type of
      logic_dir ->
        % logical dirs needs to be treated separately; they have no children of their own, but steal the children of the dirs they're associated with.
        ?DEBL("    ~p: logical dir: ~p",[Inode,Name]),
        generate_logic_attribute_dir_children(Name,?ATTR_FOLDR);

      attribute_dir ->
        ?DEB1("    attribute dir; generating lodic dirs"),
        MyLogicDirs = generate_logic_dirs(Name),
        ?DEB1("    filtering children"),
        MyChildren = filter_children(find_conn(Name),Entry#inode_entry.children),
        ?DEB1("    combining children"),
        % Lets push the logic dirs to the childrens list, removing old logic dirs.
        MyFinalChildren=lists:foldl(fun(Item,Acc) -> attr_tools:keymergeunique(Item,Acc) end, MyChildren,MyLogicDirs),
        ?DEB2("    children: ~p",MyFinalChildren),
        MyFinalChildren;

      #dir_link{link=Ino} ->
        ?DEB1("     dir link"),
        MyChildren=generate_dir_link_children(Ino,Name),
        ?DEB2("     generated children: ~p",MyChildren),
        MyChildren;

      _DirType ->
        ?DEB2("    other dir: ~p",_DirType),
        Entry#inode_entry.children

    end,
  {value,Children};

children(Inode) ->
  ?DEB2("  >children ~p",Inode),
  case tree_srv:lookup(Inode,inodes) of
   {value, Entry} ->
       children(Entry);
   none -> none
 end.

generate_dir_link_children(Ino,Name) ->
  ?DEB1("generate_dir_link_children"),
  {value,Entry}=tree_srv:lookup(Ino,inodes),
  EntryType=Entry#inode_entry.type,
  LinkChildren0=filter_children_entry(Entry,Name),
  LinkChildren = case EntryType of
    attribute_dir ->
      ?DEB1("Filtering out bogus logical connectives"),
      filter:filter(LinkChildren0,"BUTNOT",[{"AND",any},{"OR",any},{"BUTNOT",any}]);
    _ ->
      LinkChildren0
  end,
  ?DEB2("    converting children: ~p",LinkChildren),
  %XXX: When looking for ONE entry, generating ALL seems stupid.
  ConvertedChildren=lists:map(
    fun({MyName,Inode}) ->
      MyInode=inode:get({Name,MyName},ino),
      case tree_srv:lookup(MyInode,inodes) of
        {value,_MyEntry} ->
          % For now, I return the children of the linked to entry, if already generated.
          {MyName,MyInode};
        none ->
          % No entry found, generating children from the dir linked with, if a dir,
          % and returning linked to file entry if file.
          {value,MyLinkEntry}=tree_srv:lookup(Inode,inodes),
          case MyLinkEntry#inode_entry.type of
            attribute_dir ->
              MyEntry=
                MyLinkEntry#inode_entry{
                  name={Name,MyName},
                  children=[],
                  type=#dir_link{link=Inode},
                  links=[]
                },
                tree_srv:enter(MyInode,MyEntry,inodes),
                {MyName,MyInode};
              _ ->
                {MyName,Inode}
          end
      end
    end,LinkChildren),
  case EntryType of
    attribute_dir ->
      LogicDirs=generate_logic_dirs(Name),
      ConvertedChildren++LogicDirs;
    _ ->
      ConvertedChildren
  end.



%% {{{{{{{{{a,b},c},"AND"},d},e},"OR"},f},"butnot"},g} ->
%% {{a,b},c}
%% "AND"
%% {d,e}
%% "OR"
%% f
%% "butnot"
%% g
%%
%% ==>
%% [{{a,b},c},{d,e},f,g]
%% ["AND","OR","butnot"]
%% ==>
%% 




%filter_children(Ino,{{{_KeyVal,_Connective}=Logic,_Parent},_Me}) ->
%  {value,Children}=children(Ino),
%  filter_children(Logic,Children);

%% Entry is an entry whose children are known. Name is a directory structure containing connectives to be used to filter

filter_children_entry(InoOrEntry,Name) ->
  {value,Children}=children(InoOrEntry),
  %We've gotten the parent's children. now, using the connecive we've got, we combine these with the children of the dirs before the connective
  filter_children(find_conn(Name),Children).

%% The first argument is a recursive list of ordered pairs, with the first element containing directory parents and the second containing the last element, a logic connective.
%% LastChildrenUnfiltered is the children after the final logical connective in the list.
%% XXX: This thing tails in the wrong way! This will consume much memory if we have very complicated connections.

%filter_children({KeyValPair,Connective},LastChildrenUnfiltered) when ?CONNS(Connective) ->
%  case inode:is_numbered(KeyValPair,ino) of
%    false -> false;
%    Ino -> 
%      % Since directories are filtered when created, I need not filter the children of the parent anew.
%      {value,PrevChildrenUnfiltered} = children(Ino),
%      ?DEBL("Filtering ~p and ~p using connective ~p",[PrevChildrenUnfiltered,LastChildrenUnfiltered,Connective]),
%      LastChildren = filter:filter(PrevChildrenUnfiltered,Connective,LastChildrenUnfiltered),
%      LastChildren
%  end;

filter_children([Connective|Parents],Children) when ?CONNS(Connective) ->
  case inode:is_numbered(Parents,ino) of
    false -> false;
    Ino ->
      % Since directories are filtered when created, I need not filter the children of the parent anew.
      {value,PrevChildren} = children(Ino),
      ?DEBL("Filtering ~p and ~p using connective ~p",[PrevChildren,Children,Connective]),
      filter:filter(PrevChildren,Connective,Children)
  end;



%% When we have no logical connectiv as final dir, we return

filter_children(_,LastChildrenUnfiltered) -> 
  LastChildrenUnfiltered.



parse({A,B}) when ?CONNS(B) ->
  [{null,B}|parse(A)];


parse({A,B}) ->
  [{B,null}|parse(A)];
   

parse(A) ->
  {A,null}.


combine(A,{B,C}) ->
  {combine(A,B),C};

combine(A,C) ->
  {A,C}.

link_ino({_,Entry}) ->
  (Entry#inode_entry.type)#dir_link.link.

generate_logic_attribute_dir_children(LogicName,MirrorDir) ->
  % get entry, change inodes and names, return.
  ?DEB1("     getting attributes entry "),
  MIno=inode:get(MirrorDir,ino),
  {value,MEntry} = tree_srv:lookup(MIno,inodes),
  ?DEB1("     transforming children"),
  % generate links to the children of ?ATTR_FOLDR to be sent to the logic dir children list.
  lists:map(
    fun({Name,Inode}) ->
      {value,Entry}=tree_srv:lookup(Inode,inodes),
      case Entry#inode_entry.type of
        attribute_dir ->
          LinkEntry=Entry#inode_entry{
            name=[Name|LogicName],
            links=[],
            generated=false,
            type=#dir_link{link=Inode}},
          LinkIno=inode:get(LinkEntry#inode_entry.name,ino),
          tree_srv:enter(LinkIno,LinkEntry,inodes),
          {Name,LinkIno};
        #external_file{} ->
          {Name,Inode}
      end
    end,
    MEntry#inode_entry.children
  ).

-define(LD(X) , generate_logic_dir(Predecessor,X)).

%% TODO: This function will later add different folders for different directories, 
%% when I add support for dir types such as attribs/BOTH/.../AND/..., .../AND/EITHER/.../OR/...,
%% filtering away "nonsense" dir combinations such as "EITHER/.../AND/..." and "BOTH/.../OR/..."
%% also, having an "AND" at the root attrib folder makes no sense...

generate_logic_dirs(Predecessor) ->
  [
    ?LD("AND"),
    ?LD("OR"),
    ?LD("BUTNOT")
  ].


generate_logic_dir(Parent,X) ->
  ?DEB2("     generating lodic dir \"~p\"",X),
  ?DEB1("      getting entry"),
  {value,PEntry}=tree_srv:lookup(inode:get(Parent,ino),inodes),
  ?DEB1("      generating new entry"),
  Name=[X|Parent],
  Ino=inode:get(Name,ino),
  Entry=PEntry#inode_entry{name=Name,type=logic_dir,children=[]},
  ?DEB1("      saving new entry"),
  tree_srv:enter(Ino,Entry,inodes),
  {X,Ino}.

