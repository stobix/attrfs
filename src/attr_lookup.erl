
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
%%%
%%% @end
%%%=========================================================================

-module(attr_lookup).

-include("../include/attrfs.hrl").
-include_lib("newdebug/include/newdebug19.hrl").

-export([direntries/1,
        children/1]).

-export([find_conn/1]).

% Debug exports
-export([generate_logic_dir_children/2]).

-define(IS_CONN_FOLDR(X),((X==?AND_FOLDR) or (X==?OR_FOLDR) or (X==?BUTNOT_FOLDR))).

%%--------------------------------------------------------------------------
%% Gets the children for the inode from the inode list, and runs direntrify
%% on it.
%%--------------------------------------------------------------------------
direntries(Inode) ->
  ?DEB1({lookup,5},">direntries"),
  ?DEB1({lookup,7},"Getting child entries"),
  case tree_srv:lookup(Inode,folder_cache) of
    none ->
      {value,Children}=children(Inode),
      ?DEBL({lookup,7},"Converting children ~p for ~p",[Children,Inode]),
      Direntries=direntrify(Children),
      tree_srv:insert(Inode,Direntries,folder_cache),
      Direntries;
    error ->
      {value,Children}=children(Inode),
      ?DEBL({lookup,err},"Converting children ~p for ~p, but there is no folder cache!",[Children,Inode]),
      Direntries=direntrify(Children),
      Direntries;
    {value,Direntries} -> 
      Direntries
  end.


%%--------------------------------------------------------------------------
%% direntrify takes a [{Name,Inode,Type}] and returns a [fuserl:#direntry{}]
%%--------------------------------------------------------------------------
direntrify(Things) -> direntrify(Things,0,[]).

direntrify([],Offset,Converted) -> 
  ?DEBL({lookup,8},"Done converting children, final offset: ~p",[Offset]),
  {Offset,Converted};

direntrify([{Name,Inode,_Type}|Children],Offset,Converted) ->
  ?DEB2({lookup,6},"Getting entry for child ~p",{Name,Inode}),
  {value,Child}=tree_srv:lookup(Inode,inodes),
  ?DEB2({lookup,8},"Getting permissions for child ~p",{Name,Inode}),
  ChildStats=Child#inode_entry.stat,
  ?DEB2({lookup,8},"Creating direntry for child ~p",{Name,Inode}),
  Direntry= #direntry{name=Name ,stat=ChildStats },
  ?DEB2({lookup,8},"Calculatig size for direntry for child ~p",{Name,Inode}),
  NewOffset=Offset+fuserlsrv:dirent_size(Direntry),
  Direntry1=Direntry#direntry{offset=NewOffset},
  ?DEB2({lookup,8},"Appending child ~p to list",{Name,Inode}),
  direntrify(Children,NewOffset,[Direntry1|Converted]).




%%--------------------------------------------------------------------------
%% Finds the previous logical connective in a directory tree string list, 
%%  if any, and returns the "tail" of the directory tree upwards from the 
%%  logical connective.
%%--------------------------------------------------------------------------
find_conn([]) -> [];
find_conn(A) when is_binary(A) -> []; %XXX This is _probably_ a correct fix for the list to binary conversion
find_conn([A|As]=Conn) ->
  case ?IS_CONN_FOLDR(A) of
    true -> Conn;
    false -> find_conn(As)
  end.

 

%%--------------------------------------------------------------------------
%% children returns the children for the inode, if the inode is 
%%  present. Returns like gb_trees:lookup
%%--------------------------------------------------------------------------


%% For sake of efficiency, I let this function be called with either an inode or directly with an #inode_entry
children(#inode_entry{}=Entry) ->
  ?DEB1({lookup,3},">children"),
  Name=Entry#inode_entry.name,
  ?DEBL({lookup,5},"got an entry: ~p",[Name]),
  Children=
    case Entry#inode_entry.type of
      logic_dir ->
        % logical dirs needs to be treated separately; they have no children of their own, but steal the children of the dirs they're associated with.
        generate_logic_dir_children(Name,?ATTR_FOLDR);

      attribute_dir ->
        Entry#inode_entry.contents;

      #dir_link{link=Ino} ->
      % For now, I assume that all dir links are children of logical dirs, 
        ?DEB1({lookup,5},"logical dir link"),
        MyChildren=generate_logic_link_children(Ino,Name),
        ?DEB2({lookup,5},"generated children: ~p",MyChildren),
        MyChildren;

      _DirType ->
        ?DEB2({lookup,5},"other dir: ~p",_DirType),
        Entry#inode_entry.contents

    end,
  {value,Children};

children(Inode) ->
  ?DEB2({lookup,3},">children ~p",Inode),
  case tree_srv:lookup(Inode,inodes) of
   {value, Entry} ->
       children(Entry);
   none -> none
 end.


% TODO: Filter away &lt;&lt;system.*>> from the attribute children.
generate_logic_link_children(Ino,Name) ->
  ?DEBL({lookup,3},">generate_logic_link_children for ~p ~tp",[Ino,Name]),
  {value,Entry}=tree_srv:lookup(Ino,inodes),
  EntryType=Entry#inode_entry.type,
  case EntryType of
    attribute_dir ->
      LinkChildren=filter_children_entry(Entry,Name),
      ?DEB1({lookup,5},"Filtering out bogus logical connectives"),
%     TODO: Replace the filter call with a built in feature of filter to always filter away logical dirs.
      ?DEB2({lookup,5},"converting children: ~p",LinkChildren),
      ConvertedChildren=lists:map(
        fun({_MyName,_Inode,logic_dir}=E) ->
          E;
           ({MyName,Inode,Type}) ->
          MyInode=numberer:get([MyName|Name],ino),
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
                  MyLinkStat=MyLinkEntry#inode_entry.stat,
                  MyLinkMode=MyLinkStat#stat.st_mode,
                  MyStat=MyLinkStat#stat{st_mode=MyLinkMode band (bnot 8#222)},
                  MyEntry=
                    MyLinkEntry#inode_entry{
                      name=[MyName|Name],
                      contents=[],
                      stat=MyStat,
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
      case length(Name)<?MAX_LOGIC_RECURS of
        true ->
          LogicDirs=generate_logic_dirs(Name),
          ConvertedChildren++LogicDirs;
        false ->
          ?DEB1({lookup,5},"not generating logic dirs, due to depth"),
          ConvertedChildren
      end;
          
    _ ->
    #inode_entry.contents
  end.


%% Entry is an entry whose children are known. Name is a directory structure containing connectives to be used to filter

filter_children_entry(InoOrEntry,Name) ->
  {value,Children}=children(InoOrEntry),
  %We've gotten the parent's children. now, using the connecive we've got, we combine these with the children of the dirs before the connective
  filter_children(find_conn(Name),Children).

%% The first argument is a list of parents, starting with a logical connective,unless the list is empty.
%% Children is the children after the final logical connective in the list.
%% XXX: This thing tails in the wrong way! This will consume much memory if we have very complicated connections.

filter_children([Connective|Parents],Children) ->
  ?DEB1({lookup,6},">filter_children"),
  case ?IS_CONN_FOLDR(Connective) of
    true ->
      case numberer:is_numbered(Parents,ino) of
        false -> false;
        Ino ->
          % Since directories are filtered when created, I need not filter the children of the parent anew.
          {value,PrevChildren}=children(Ino),
          ?DEBL({lookup,8},"Filtering ~p and ~p using connective ~p",[PrevChildren,Children,Connective]),
          attr_filter:filter(PrevChildren,Connective,Children)
      end;
    false ->
    % No more connectives, returning children.
      Children
  end;

%% Since I always call find_conn before filter_children, I end up here if I have no connective to begin with.
filter_children([],LastChildrenUnfiltered) ->
      LastChildrenUnfiltered.

%%--------------------------------------------------------------------------
%% generate_logic_dir_children takes the name of a logic folder 
%%  and a dir whose children will be linked to the logic dir.
%% Since directories for some reason cannot share inode numbers, I need to
%%  have a different inode number for each directory link.
%%--------------------------------------------------------------------------
generate_logic_dir_children(LogicName,MirrorDir) ->
  % get entry, change inodes and names, return.
  ?DEBL({lookup,6},">generate_logic_dir_children ~p ~p",[LogicName,MirrorDir]),
  {ok,MIno}=numberer:n2i(MirrorDir,ino),
  ?DEB2({lookup,8},"attributes ino: ~p",MIno),
  ?DEB1({lookup,8},"getting attributes entry "),
  {value,MEntry} = tree_srv:lookup(MIno,inodes),
  ?DEB1({lookup,8},"transforming children"),
  % generate links to the children of ?ATTR_FOLDR to be sent to the logic dir children list.
  % Since I want to get rid of elements not belonging in a logic dir, I filter out everything that is not an attridute_bir or an #external_file{}.
  lists:foldl(
    fun({Name,Inode,Type},Acc) ->
      case Type of
        attribute_dir ->
          {value,Entry}=tree_srv:lookup(Inode,inodes),
          %LinkName=[Name|LogicName],
          LinkName=if 
            is_binary(LogicName) -> [Name|[LogicName|[]]];
            true -> [Name|LogicName]
          end,
          LinkType=#dir_link{link=Inode},
          Stat=Entry#inode_entry.stat,
          Mode=Stat#stat.st_mode,
          LinkStat=?ST_MODE(Stat,(bnot 8#222) band Mode), % Remove write permissions from child.
          LinkEntry=Entry#inode_entry{
            name=LinkName,
            links=[],
            stat=LinkStat,
            generated=false,
            type=LinkType},
          ?DEB2({lookup,8},"getting or setting inode number of ~p",[Name|LogicName]),
          LinkIno=numberer:get(LinkName,ino),
          tree_srv:enter(LinkIno,LinkEntry,inodes),
          [{Name,LinkIno,LinkType}|Acc];
        #external_file{}=E ->
          [{Name,Inode,E}|Acc];
        _ ->
          Acc
      end
    end,
    [],
    MEntry#inode_entry.contents
  ).

-define(LD(X) , generate_logic_dir(Predecessor,X)).

%% TODO: This function will later add different folders for different directories, 
%% when I add support for dir types such as attribs/BOTH/.../AND/..., .../AND/EITHER/.../OR/...,
%% filtering away "nonsense" dir combinations such as "EITHER/.../AND/..." and "BOTH/.../OR/..."
%% also, having an ?AND_FOLDR at the root attrib folder makes no sense...

generate_logic_dirs([]) ->
%% Will place a BOTH and an EITHER directory in the attribute root folder when I support those.
  [
  ];

generate_logic_dirs(Predecessor) ->
  [
    ?LD(?AND_FOLDR),
    ?LD(?OR_FOLDR),
    ?LD(?BUTNOT_FOLDR)
  ].

generate_logic_dir(Parent,X) ->
  ?DEBL({lookup,6},"generate_lodic_dir ~p->~p",[Parent,X]),
  ?DEB1({lookup,8},"getting entry"),
  {ok,PIno}=numberer:n2i(Parent,ino),
  {value,PEntry}=tree_srv:lookup(PIno,inodes),
  ?DEB1({lookup,8},"generating new entry"),
  Name=[X|Parent],
%  Name=if 
%    is_binary(Parent) -> [X|[Parent|[]]];
%    true -> [X|Parent]
%  end,
  Ino=numberer:get(Name,ino),
  ?DEBL({lookup,6},"new entry ~p has ino ~p",[Name,Ino]),
  PStat=PEntry#inode_entry.stat,
  % Logical dirs will not be writeable. Therefore, I do not directly copy the mode of the parent
  Stat=?ST_MODE(PStat,?M_DIR(8#555)), 
  Entry=PEntry#inode_entry{name=Name,type=logic_dir,contents=[],stat=Stat},
  ?DEB1({lookup,8},"saving new entry"),
  tree_srv:enter(Ino,Entry,inodes),
  {X,Ino,logic_dir}.

