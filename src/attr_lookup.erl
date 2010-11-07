-module(attr_lookup).

-include("../include/attrfs.hrl").
-include("../include/debug.hrl").

-export([direntries/1,
        children/1]).

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



%%--------------------------------------------------------------------------
%% children returns the children for the inode, if the inode is 
%% present. Returns like gb_trees:lookup
%%--------------------------------------------------------------------------
children(Inode) ->
  ?DEB2("  >children ~p",Inode),
  case tree_srv:lookup(Inode,inodes) of
    {value, Entry} -> 
      Name=Entry#inode_entry.name,
      ?DEBL("    got an entry: ~p",[Name]),
      Children=
        case Entry#inode_entry.type of
          logic_dir ->
            % logical dirs needs to be treated separately; they have no children of their own, but steal the children of the dirs they're associated with.
            ?DEBL("    ~p: logical dir: ~p",[Inode,Name]),
            generate_logic_attribute_dir_children(Name);
          #attribute_dir{atype=value} ->
            ?DEB1("    value dir"),
            {GParent,_Parent}=Name,
            ?DEB1("    generating lodic dirs"),
            MyLogicDirs = generate_logic_dirs(Name),
            ?DEB1("    filtering children"),
            MyChildren = filter_children(GParent,Entry#inode_entry.children),
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
    none -> none
  end.

generate_dir_link_children(Ino,Name) ->
  ?DEB1("generate_dir_link_children"),
  {value,Entry}=tree_srv:lookup(Ino,inodes),
  EntryType=Entry#inode_entry.type,
  LinkChildren0=filter_children(Ino,Name),
  LinkChildren = case EntryType of
    #attribute_dir{atype=value} ->
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
            #attribute_dir{} ->
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
    #attribute_dir{atype=value} ->
      LogicDirs=generate_logic_dirs(Name),
      ConvertedChildren++LogicDirs;
    _ ->
      ConvertedChildren
  end.

filter_children(Ino,{{{_KeyVal,_Connective}=Logic,_Parent},_Me}) ->
  {value,Children}=children(Ino),
  filter_children(Logic,Children);


filter_children({{Key,_Val}=KeyValPair,Connective},LastChildrenUnfiltered) when (Connective == "AND") or (Connective == "OR") or (Connective == "BUTNOT") ->
  case inode:is_numbered(KeyValPair,ino) of
    false -> false;
    Ino -> 
      {value,PrevChildrenUnfiltered} = children(Ino),
      PrevChildren = filter_children(Key,PrevChildrenUnfiltered),
      ?DEBL("Filtering ~p and ~p using connective ~p",[PrevChildren,LastChildrenUnfiltered,Connective]),
      LastChildren = filter:filter(PrevChildren,Connective,LastChildrenUnfiltered),
      LastChildren
  end;

filter_children(_Grandparent,LastChildrenUnfiltered) -> 
  LastChildrenUnfiltered.

generate_logic_attribute_dir_children(LogicName) ->
  % get entry, change inodes and names, return.
  ?DEB1("     getting attributes entry "),
  PIno=inode:get(?ATTR_FOLDR,ino),
  {value,PEntry} = tree_srv:lookup(PIno,inodes),
  ?DEB1("     transforming children"),
  lists:map(
    fun({Name,Inode}) ->
      {value,Entry}=tree_srv:lookup(Inode,inodes),
      case Entry#inode_entry.type of
        #attribute_dir{} ->
          LinkEntry=Entry#inode_entry{
            dir_name=Name,
            name={LogicName,Name},
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
    PEntry#inode_entry.children
  ).

-define(LD(X) , generate_logic_dir(Predecessor,X)).

generate_logic_dirs({_Grandparent,_Parent}=Predecessor) ->
  [
    ?LD("AND"),
    ?LD("OR"),
    ?LD("BUTNOT")
  ].

-define(LDIR(X) , {Predecessor,X}).
-define(LINO(X) , inode:get(?LDIR(X),ino)).
-define(LCLD(X) , {X,?LINO(X)}).

generate_logic_dir({GrandParent,_Parent}=Predecessor,X) ->
  ?DEB2("     generating lodic dir \"~p\"",X),
  ?DEB1("      getting entry"),
  {value,GPEntry}=tree_srv:lookup(inode:get(GrandParent,ino),inodes),
  ?DEB1("      generating new entry"),
  Entry=GPEntry#inode_entry{dir_name=X,name=?LDIR(X),type=logic_dir,children=[]},
  ?DEB1("      saving new entry"),
  tree_srv:enter(?LINO(X),Entry,inodes),
  ?LCLD(X).
