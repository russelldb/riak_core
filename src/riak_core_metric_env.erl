%%
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

%% TODO provide access to app envs by app/name
-module(riak_core_metric_env).

-behaviour(riak_core_metric).

-export([new/0, value/3, value/4, update/2, options/1]).

new() ->
    env.

options(env) ->
    [{App, [K || {K, _V} <- app_helper:get_env(App)]} || {App, _Mod} <- [{riak_core, vnode}|riak_core:vnode_modules()]].
        
update(_Arg, env) ->
    env.

value(Level, _Name, env) when Level < 3 ->
    [];
value(_Level, _Name, env) ->
    [{riak_core_metric:join_as_atom([App, '_', env]), 
      [E || E <- app_helper:get_env(App)]} || {App, _Mod} <- [{riak_core, vnode}|riak_core:vnode_modules()]].

value(_Level, App, _Name, env) when is_atom(App)->
    app_helper:get_env(App);
value(_Level, [App, Var], _Name, env) ->
    {riak_core_metric:join_as_atom([App, '_', Var]), app_helper:get_env(App, Var)}.



