-module(attr_ext).

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

-export([add_new_attribute/4,
         rehash_ext_from_db/2,
         append_attribute/3,
         generate_ext_info_io/1,
         ext_info_to_ext_io/1]).


-include("../include/attrfs.hrl").
-include("../include/debug.hrl").


%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
rehash_ext_from_db(Inode,Path) ->
  {ExtInfo,ExtIo,ExtAmount}=generate_ext_info_io(Path),
  {value,Entry}=tree_srv:lookup(Inode,inodes),
  Stat=Entry#inode_entry.stat,
  NewStat=Stat#stat{st_nlink=ExtAmount+1},
  NewEntry=Entry#inode_entry{stat=NewStat,ext_info=ExtInfo,ext_io=ExtIo},
  tree_srv:enter(Inode,NewEntry,inodes).

%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
add_new_attribute(Path,FIno,FEntry,Attr) ->
  ?DEB1(3,">add_new_attribute"),
  % Add the new attribute, if non-existent.
  ?DEBL(5,"inserting (~p) ~p into database, if nonexistent",[Path,Attr]),
  length(dets:match(?ATTR_DB,{Path,Attr}))==0 andalso
    (ok=dets:insert(?ATTR_DB,{Path,Attr})),
  ?DEBL(5,"database entry: ~p",[dets:match(?ATTR_DB,{Path,Attr})]),
  #inode_entry{stat=Stat,name=FName}=FEntry,
  ?DEBL(5,"appending ~p for {~p,~p} to the file system",[Attr,FName,FIno]),
  append_attribute(Attr,FName,?UEXEC(Stat)),
  ?DEB1(5,"creating new ext info"),
  rehash_ext_from_db(FIno,Path).




%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
append_attribute(Attr,FName,Stat) ->
  ?DEBL(3,"appending ~p/~p",[Attr,FName]),
  append(Attr,FName,FName,Stat).


%% this function creates directory parents recursively if unexistent, updating
%% the lists of children along the way.

append(Parent,ChildInoName,ChildName,Stat) ->
  ?DEBL(6,"append ~p ~p ~p",[Parent,ChildInoName,ChildName]),
  {ok,ChildIno}=inode:n2i(ChildInoName,ino),
  ParentIno=inode:get(Parent,ino),
  {value,ChildEntry}=tree_srv:lookup(ChildIno,inodes),
  ?DEB1(8,"got child entry"),
  ChildType=ChildEntry#inode_entry.type,
  ChildTriplet={ChildName,ChildIno,ChildType},
  case tree_srv:lookup(ParentIno,inodes) of
    % No entry found, creating new attribute entry.
    none ->
      ?DEBL(8,"adding new attribute folder ~p with the child ~p",[Parent,ChildName]),
      PEntry=
        #inode_entry{
          type=attribute_dir,
          name=Parent,
          contents=[ChildTriplet],
          stat=attr_tools:dir(Stat#stat{st_ino=ParentIno}),
          ext_info=[],
          ext_io=ext_info_to_ext_io([])
        },
      ?DEB1(8,"entering new entry into server"),
      tree_srv:enter(ParentIno,PEntry,inodes),
      [PName|GrandParent]=Parent, %Since attribute folder names are defined recursively.
      ?DEBL(8,"checking parent of parent (~p)",[GrandParent]),
      append(GrandParent,Parent,PName,Stat);
    {value,PEntry} ->
      ?DEBL(8,"merging ~w into ~w",[ChildTriplet,PEntry#inode_entry.contents]),
      attr_tools:append_child(ChildTriplet,PEntry)
  end.

%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
generate_ext_info(Path) ->
  ?DEB2(3,"generating ext info for ~s",Path),
  ExtInfo0=dets:match(?ATTR_DB,{Path,'$1'}), 
  ExtAmount=lists:foldr(fun(_,N) -> N+1 end,0,ExtInfo0),
  ExtInfo1=convert(ExtInfo0),
  {attr_tools:merge_duplicates(ExtInfo1),ExtAmount}. % Going from a list of lists of tuples to a list of tuples.

convert(LList) ->
    convert1(attr_tools:flatten1(LList)).


convert1([]) -> [];
convert1([A|As]) ->
    [keyvalue(A)|convert1(As)].

% A key AND value less attribute will make this function crash, and it should.

% A valueless key will have an empty string as value
keyvalue([A|[]]) ->
    {A,""};

% All other attributes are converted from [value,words,key,of,list,long] to {"long/list/of/key/words",value}
keyvalue(A) ->
    lists:foldl(
        fun(X,{[],[]}) ->
            ?DEB2(5,"init: ~p",X),
            {[],X};
           (X,{[],Z}) ->
            {X,Z};
           (X,{Y,Z}) ->
            ?DEBL(5,"recurs: ~p,~p,~p",[X,Y,Z]),
            {X++"/"++Y,Z}
        end,
        {[],[]},
        A).


%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
generate_ext_info_io(Path) ->
  ?DEB2(3,"generating ext info for ~p",Path),
  {ExtInfo,ExtAmount}=generate_ext_info(Path),
  ?DEB2(5,"generating ext io for ~p",ExtInfo),
  ExtIo=ext_info_to_ext_io(ExtInfo),
  {ExtInfo,ExtIo,ExtAmount}.

%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
ext_info_to_ext_io(InternalExtInfoTupleList) ->
  ?DEB1(6,"Creating ext_io"),
  ext_info_to_ext_io(InternalExtInfoTupleList,[]).

%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
ext_info_to_ext_io([],B) -> 
  B0=B++"\0",
  B0len=length(B0),
  ?DEB1(8,"Done creating ext_io"),
  ?DEBL(8,"Final string: \"~p\", size: ~p",[B0,B0len]),
  {B0len,B0};

%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
ext_info_to_ext_io([{Name,_}|InternalExtInfoTupleList],String) ->
  Name0=
    case string:str(Name,"system")==1 of
      false ->
        ?DEB2(8,"Adding zero to end of name ~p, and \"user.\" to the start",Name),
        "user."++Name++"\0";
      true -> 
        ?DEB2(8,"Adding zero to end of name ~p",Name),
        Name++"\0"
    end,
  ?DEB1(8,"Appending name to namelist"),
  NewString=String++Name0,
  ?DEB1(8,"Recursion"),
  ext_info_to_ext_io(InternalExtInfoTupleList,NewString).


