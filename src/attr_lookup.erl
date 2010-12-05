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

-export([find_conn/1]).

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
        generate_logic_attribute_dir_children(Name,[]);
        %generate_logic_attribute_dir_children(Name,?ATTR_FOLDR);

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
  case EntryType of
    attribute_dir ->
      LinkChildren=filter_children_entry(Entry,Name),
      ?DEB1("Filtering out bogus logical connectives"),
%     TODO: Replace the filter call with a built in feature of filter to always filter away logical dirs.
      ?DEB2("    converting children: ~p",LinkChildren),
      ConvertedChildren=lists:map(
        fun({_MyName,_Inode,logic_dir}=E) ->
          E;
           ({MyName,Inode,Type}) ->
          MyInode=inode:get([MyName|Name],ino),
          case tree_srv:lookup(MyInode,inodes) of
            {value,_MyEntry} ->
              % For now, I return the children of the linked to entry, if already generated.
              {MyName,MyInode,Type};
            none ->
              % No entry found, generating children from the dir linked with, if a dir,
              % and returning linked to file entry if file.
              {value,MyLinkEntry}=tree_srv:lookup(Inode,inodes),
              case Type of
                attribute_dir ->
                  MyType=#dir_link{link=Inode},
                  MyEntry=
                    MyLinkEntry#inode_entry{
                      name=[MyName|Name],
                      children=[],
                      type=MyType,
                      links=[]
                    },
                  tree_srv:enter(MyInode,MyEntry,inodes),
                  {MyName,MyInode,MyType};
                Other ->
                  {MyName,Inode,Other}
              end
          end
        end,
        LinkChildren),
      LogicDirs=generate_logic_dirs(Name),
      ConvertedChildren++LogicDirs;
    _ ->
    #inode_entry.children
  end.


%% Entry is an entry whose children are known. Name is a directory structure containing connectives to be used to filter

filter_children_entry(InoOrEntry,Name) ->
  {value,Children}=children(InoOrEntry),
  %We've gotten the parent's children. now, using the connecive we've got, we combine these with the children of the dirs before the connective
  filter_children(find_conn(Name),Children).

%% The first argument is a recursive list of ordered pairs, with the first element containing directory parents and the second containing the last element, a logic connective.
%% LastChildrenUnfiltered is the children after the final logical connective in the list.
%% XXX: This thing tails in the wrong way! This will consume much memory if we have very complicated connections.

filter_children([Connective|Parents],Children) when ?CONNS(Connective) ->
  case inode:is_numbered(Parents,ino) of
    false -> false;
    Ino ->
      % Since directories are filtered when created, I need not filter the children of the parent anew.
      {value,PrevChildren} = children(Ino),
      ?DEBL("Filtering ~p and ~p using connective ~p",[PrevChildren,Children,Connective]),
      attr_filter:filter(PrevChildren,Connective,Children)
  end;



%% When we have no logical connectiv as final dir, we return

filter_children(_,LastChildrenUnfiltered) -> 
  LastChildrenUnfiltered.


link_ino({_,Entry}) ->
  (Entry#inode_entry.type)#dir_link.link.

generate_logic_attribute_dir_children(LogicName,MirrorDir) ->
  % get entry, change inodes and names, return.

  {ok,MIno}=inode:n2i(MirrorDir,ino),
  ?DEB2("     attributes ino: ~p",MIno),
  ?DEB1("     getting attributes entry "),
  {value,MEntry} = tree_srv:lookup(MIno,inodes),
  ?DEB1("     transforming children"),
  % generate links to the children of ?ATTR_FOLDR to be sent to the logic dir children list.
  lists:map(
    fun({Name,Inode,Type}) ->
      case Type of
        attribute_dir ->
          {value,Entry}=tree_srv:lookup(Inode,inodes),
          LinkName=[Name|LogicName],
          LinkType=#dir_link{link=Inode},
          LinkEntry=Entry#inode_entry{
            name=LinkName,
            links=[],
            generated=false,
            type=LinkType},
          ?DEB2("    getting or setting inode number of ~p",[Name|LogicName]),
          LinkIno=inode:get(LinkName,ino),
          tree_srv:enter(LinkIno,LinkEntry,inodes),
          {Name,LinkIno,LinkType};
        #external_file{}=E ->
          {Name,Inode,E}
      end
    end,
    MEntry#inode_entry.children
  ).

-define(LD(X) , generate_logic_dir(Predecessor,X)).

%% TODO: This function will later add different folders for different directories, 
%% when I add support for dir types such as attribs/BOTH/.../AND/..., .../AND/EITHER/.../OR/...,
%% filtering away "nonsense" dir combinations such as "EITHER/.../AND/..." and "BOTH/.../OR/..."
%% also, having an "AND" at the root attrib folder makes no sense...

generate_logic_dirs([]) ->
%% Will place a BOTH and an EITHER directory in the attribute root folder when I support those.
  [
  ];

generate_logic_dirs(Predecessor) ->
  [
    ?LD("AND"),
    ?LD("OR"),
    ?LD("BUTNOT")
  ].


generate_logic_dir(Parent,X) ->
  ?DEB2("     generating lodic dir \"~p\"",X),
  ?DEB1("      getting entry"),
  {ok,PIno}=inode:n2i(Parent,ino),
  {value,PEntry}=tree_srv:lookup(PIno,inodes),
  ?DEB1("      generating new entry"),
  Name=[X|Parent],
  Ino=inode:get(Name,ino),
  Entry=PEntry#inode_entry{name=Name,type=logic_dir,children=[]},
  ?DEB1("      saving new entry"),
  tree_srv:enter(Ino,Entry,inodes),
  {X,Ino,logic_dir}.

