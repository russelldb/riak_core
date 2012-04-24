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
-export([start_link/2, update/2, value/1, value/2, value/3, options/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {name, mod, mod_state, description}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Name, Args) ->
    gen_server:start_link({local, Name}, ?MODULE, [{name, Name}|Args], []).

update(Name, Args) ->
    gen_server:cast(Name, {update, Args}).

value(Name) ->
    value(Name, 0).

value(Name, Level) when is_integer(Level) ->
    {ok, Val} = gen_server:call(Name, {level, Level}),
    Val.

value(Level, Name, Spec) ->
    {ok, Val} = gen_server:call(Name, {spec, Level, Spec}),
    Val.

options(Name) ->
    {ok, Options} = gen_serer:call(Name, options),
    Options.
                              
init(Args) ->
    Name = proplists:get_value(name, Args),
    {type, Type} = proplists:lookup(type, Args), %% Does mod need init args?
    Mod = mod_from_type(Type),
    Description = proplists:get_value(description, Args),
    ModState = Mod:new(),
    DoTicks = do_ticks(Mod),
    if DoTicks == true ->
            timer:send_interval(5000, tick); %% configureable tick interval?
       true ->
            ok
    end,
    {ok, #state{name=Name, mod=Mod, mod_state=ModState,
                description=Description}}.

handle_call({level, Level}, _From, #state{mod=Mod, mod_state=ModState, name=Name}=State) ->
    Stat = Mod:value(Level, Name, ModState),
    {reply, {ok, Stat}, State};
handle_call({spec, Level, Spec}, _From, #state{mod=Mod, mod_state=ModState, name=Name}=State) ->
    Stat = Mod:value(Level, Spec, Name, ModState),
    {reply, {ok, Stat}, State};
handle_call(options, _From, #state{mod=Mod, mod_state=ModState, name=Name}=State) ->
    Options = Mod:options(ModState),
    {reply, {ok, {Name, Options}}, State}.


handle_cast({update, Args}, #state{mod=Mod, mod_state=ModState0}=State) ->
    ModState = Mod:update(Args, ModState0),
    {noreply, State#state{mod_state=ModState}}.

handle_info(tick,  #state{mod=Mod, mod_state=ModState}=State) ->
    Mod:tick(ModState),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
mod_from_type({mod, Mod}) ->
    Mod;
mod_from_type(ShortName) ->
    list_to_atom("riak_core_metric_" ++ atom_to_list(ShortName)).

do_ticks(Mod) ->
    case proplists:get_value(tick, Mod:module_info(exports)) of
        1 ->
            true;
        _ ->
            false
    end.
