-module(options).
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
%%% @doc A server that handles lookups of config values that are either in the file name given to start_link orin application:get_env.
%%%      options:get/2 returns like application:get_env.
%%%      values in the config file have precedence over the values in application:get_env.

-behaviour(gen_server).

-include_lib("newdebug/include/debug.hrl").
-export([init/1,start_link/1,terminate/2]).
% Debugging
-export([get_all/0
        ,rehash/0]).
% Official        
-export([get/2,mget/2]).
-export([handle_call/3,handle_cast/2]).

-ifndef(CONFIG_NAME).
-define(CONFIG_NAME,".attrfsrc").
-endif.

%TODO: Include filename as an optional start argument
%   that is, starting with erl -config_file foo
%    and getting it with case init:get_argument(config_file) of {ok,[[File]]} -> ...;error->... end

start_link(ConfigName) -> 
  gen_server:start_link({local,?MODULE},?MODULE,ConfigName,[]).

init(ConfigName) ->
    CN=fix_home(ConfigName),
    Configs=rehash_(CN),
    {ok,{CN,Configs}}.

fix_home([$~,$/|Name]) ->
    [os:getenv("HOME"),"/",Name];

fix_home(A) -> A.


rehash_(ConfigName) ->
    Configs = case file:open(ConfigName,[read,raw]) of
        {ok,ConfigFile} ->
            % Read options from config file
            Options=readfile(ConfigFile),
            file:close(ConfigFile),
            ?DEBL({options,9},"Options: ~n ~p",[Options]),
            ParsedOpts=lists:map(fun(X)->parse_line(X) end,Options),
            ?DEBL({options,9},"Parsed: ~n ~p",[ParsedOpts]),
            ParsedTree=make_tree(ParsedOpts),
            ParsedTree;

        Error ->
            ?DEB1({err,1},"Options file could not be read! Options only taken from environment file"),
            gb_trees:empty()
    end,
    Configs.

make_tree(A) -> make_tree(A,gb_trees:empty()).

make_tree([],Acc) -> Acc;
make_tree([[]|As],Acc) -> make_tree(As,Acc); % Comment or empty line.
make_tree([{AKey,AVal}|As],Acc) ->
    case gb_trees:lookup(AKey,Acc) of
        {value,{string,AVal0}} -> make_tree(As,gb_trees:enter(AKey,{list,[AVal,AVal0]},Acc));
        {value,{list,AList}} -> make_tree(As,gb_trees:enter(AKey,{list,[AVal|AList]},Acc));
        none -> make_tree(As,gb_trees:insert(AKey,{string,AVal},Acc))
    end.

parse_line([]) -> [];
parse_line([$%|_]) -> [];
parse_line(Line) ->
    ?DEBL({options,1},"Parsing ~p",[Line]),
    [Head|Tail]=string:tokens(Line,":\n"),
    A={list_to_atom(replace(Head,$ ,$_)),fix_home(string:strip(string:join(Tail,":")))},
    ?DEBL({options,3},"Returning ~p",[A]),
    A.

replace([],B,C) -> [];
replace([A|Str],B,C) when A == B ->
     [C|replace(Str,B,C)];
replace([A|Str],B,C) -> [A|replace(Str,B,C)].


rehash() ->
    gen_server:cast(?MODULE,rehash).

readfile(File) ->
    readfile(File,[]).

readfile(File,Acc) ->
    case file:read_line(File) of
        {ok,Line} -> readfile(File,[Line|Acc]);
        eof -> Acc;
        Error -> Error
    end.


get_all() ->
    gen_server:call(?MODULE,get).

% Returns 
%   * the first value of option X in application Y, if multiple such values exist
%   * the value of option X in application Y, if it exists
%   * undefined otherwise
%  The values are fetched primarily from the config file specified when starting the options server
%  using the app file of Y as a fallback.
get(Y,X) ->
    gen_server:call(?MODULE,{get,Y,X}).

% Like mget, but without any filtering of return types.
mget(Y,X) ->
    gen_server:call(?MODULE,{mget,Y,X}).

handle_call(get,_From,State) ->
    {reply,State,State};

handle_call({get,Y,X},_From,State={N,Names}) ->
    ?DEBL({options,9},"Searching for ~p",[X]),
    Value=case gb_trees:lookup(X,Names) of
        {value,{string,V}} -> 
            ?DEBL({options,9},"Found name: ~p",[V]),
            {ok,V};
        {value,{list,V}} ->
            ?DEBL({options,9},"Found list: ~p, returning first entry",[V]),
            {ok,hd(V)};
        _ -> ?DEBL({options,9},"No name in ~p, returning application:get_env(~p,~p)",[N,Y,X]),
            application:get_env(Y,X)
    end,
    {reply,Value,State};


handle_call({mget,Y,X},_From,State={N,Names}) ->
    ?DEBL({options,9},"Searching for ~p",[X]),
    Value=case gb_trees:lookup(X,Names) of
        {value,V} -> 
            ?DEBL({options,9},"Found ~p",[V]),
            V;
        _ -> ?DEBL({options,9},"No name in ~p, returning application:get_env(~p,~p)",[N,Y,X]),
            application:get_env(Y,X)
    end,
    {reply,Value,State}.

handle_cast(rehash,{N,_}) ->
    {noreply,rehash_(N)}.


terminate(_,_) ->
    ok.
    
