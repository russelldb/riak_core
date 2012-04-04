%% -------------------------------------------------------------------
%% Copyright (c) 2007-2012 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%% @doc process/state wrapper around a metric

-module(riak_core_metric_proc).

-behaviour(gen_server).

%% API
-export([start_link/3, update/3, value/2, value/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%-record(state, {name}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(_App, Name, Args) ->
    gen_server:start_link({local, Name}, ?MODULE, [{name, Name}|Args], []).

update(_App, Name, Args) ->
    gen_server:cast(Name, {update, Args}).

value(App, Name) ->
    value(App, Name, []).

value(_App, Name, Presentation) ->
    {ok, Val} = gen_server:call(Name, {value, Presentation}),
    Val.

init(Args) ->
    Name = proplists:get_value(name, Args),
    {type, ShortName} = proplists:lookup(type, Args), %% Does mod need init args?
    Mod = mod_from_shortname(ShortName),
    Description = proplists:get_value(description, Args),
    DisplaySpec =  proplists:get_value(presentation, Args),
    ModState = Mod:new(),
    ets:insert(stats, {Name,  Mod,  ModState, Description, DisplaySpec}),
    {ok, Name}.

handle_call({value, undefined}, _From, Name) ->
    [{Name, Mod, ModState, _, _}] = ets:lookup(stats, Name),
    Stat = Mod:value(Name, ModState),
    {reply, {ok, Stat}, Name};
handle_call({value, Presentation}, _From, Name) ->
    [{Name, Mod, ModState, _, DisplaySpecs}] = ets:lookup(stats, Name),
    Stat = case DisplaySpecs of 
               undefined ->
                   Mod:value(Name, ModState);
               Spec ->
                   case proplists:get_value(Presentation, Spec) of
                       undefined ->
                           Mod:value(Name, ModState);
                       DisplaySpec ->
                           Mod:value(DisplaySpec, Name, ModState)
                   end
           end,
    {reply, {ok, Stat}, Name}.

handle_cast({update, Args}, Name) ->
    [{Name, Mod, ModState0, _, _}] = ets:lookup(stats, Name),
    ModState = Mod:update(Args, ModState0),
    ets:update_element(stats, Name, {3, ModState}),
    {noreply, Name}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
mod_from_shortname(ShortName) ->
    list_to_atom("riak_core_metric_" ++ atom_to_list(ShortName)).
