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
%%% 
%%% 

-module(attr_reply).


-include("../include/attrfs.hrl").
-include_lib("newdebug/include/debug.hrl").

-behaviour(gen_server).


-export([init/1,
        start_link/0,
        handle_cast/2,
        handle_info/2,
        terminate/2]).

-export([watch/3,
        reply/2]).


start_link() ->
  gen_server:start_link({local,?MODULE},?MODULE,[],[]).

init(State) ->
  process_flag(trap_exit, true),
  {ok,State}.

handle_info({'EXIT', PID, Reason}, State) ->
  ?DEBL(3,"~p died because of ~p",[PID,Reason]),
  case lists:keytake(PID,1,State) of
    {value,{_,_,Continuation,Reply},NewState} ->
      fuserlsrv:reply(Continuation,Reply),
      {noreply,NewState};
    false ->
      {noreply, State}
  end.

watch(Fun,Continuation,DefaultAnswer) ->
  gen_server:cast(?MODULE,{add,Fun,Continuation,DefaultAnswer}).

reply(Token,Reply) ->
  gen_server:cast(?MODULE,{reply,Reply,Token}).

terminate(A,B) -> 
  ?DEBL(1,"Reply server killed! (~p,~p)",[A,B]),
  ok.

handle_cast({add,Fun,Continuation,DefaultAnswer},State) ->
  Token=numberer:get(pino),
  PID=spawn_link(fun() -> Fun(Token) end),
  ?DEBL(3,"Started a fun with pid ~p and token ~p",[PID,Token]),
  {noreply,[{PID,Token,Continuation,DefaultAnswer}|State]};

handle_cast({reply,Reply,Token},State) ->
  case lists:keytake(Token,2,State) of 
    {value,{_,_,Continuation,_},NewState} ->
      fuserlsrv:reply(Continuation,Reply),
      {noreply,NewState};
    false ->
      {noreply,State}
  end.

