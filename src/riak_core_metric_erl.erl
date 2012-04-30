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

%% @doc Read only stat for cpu
-module(riak_core_metric_erl).

-behaviour(riak_core_metric).

-export([new/0, value/3, value/4,  update/2, options/1]).

new() ->
    erl.

options(erl) ->
    [{memory, [K || {K, _} <- erlang:memory()]},
     {app, [A || {A,_, _} <- application:which_applications()]},
     {system, system(1)}].
        
update(_Arg, erl) ->
    erl.

value(Level, _Name, erl) ->
    lists:flatten([value(Field, Level) || Field <- [memory, app, system]]).

value(Level, Field, _Name, erl) when is_atom(Field) ->
    value(Field, Level);
value(_Level, [app, Elem], _Name, erl) ->
    Stat = value(app, 5),
    ElemName = riak_core_metric:join_as_atom([Elem, '_', version]),
    proplists:lookup(ElemName, Stat);
value(_Level, [system, Elem], _Name, erl) ->
   system_info(Elem);
value(_Level, [Field, Elem], _Name, erl) ->
    Stat = value(Field, 5),
    ElemName = riak_core_metric:join_as_atom([Field, '_', Elem]),
    proplists:lookup(ElemName, Stat).

value(memory, 0) ->
    [{list_to_atom("memory_" ++ atom_to_list(K)), V} || {K,V} <- erlang:memory(),
    lists:member(K, memory(0))];
value(memory, _Level) ->
    [{list_to_atom("memory_" ++ atom_to_list(K)), V} || {K,V} <- erlang:memory()];
value(app, Level) when Level < 2 ->
    [];
value(app, Level) when Level > 1 ->
    [{list_to_atom(atom_to_list(A) ++ "_version"), list_to_binary(V)}
     || {A,_,V} <- application:which_applications()];
value(system, Level) ->
    [system_info(Field) || Field <- system(Level)].

system_info(nodename) ->
    {nodename, node()};
system_info(connected_nodes) ->
    {connected_nodes, nodes()};
system_info(system_version) ->
    {sys_system_version, list_to_binary(string:strip(erlang:system_info(system_version), right, $\n))};
system_info(threads_enabled) ->
    {sys_threads_enabled, erlang:system_info(threads)};
system_info(Field) ->
    Key = riak_core_metric:join_as_atom([sys, '_', Field]),
    Val = maybe_to_binary(erlang:system_info(Field)),
    {Key, Val}.

maybe_to_binary(SysInfo) when is_list(SysInfo) ->
    list_to_binary(SysInfo);
maybe_to_binary(SysInfo) ->
    SysInfo.

memory(0) ->
    [total, processes].

system(0) ->
    [nodename, connected_nodes];
system(_Level) ->
    system(0) ++ system_info().

system_info() ->
    [driver_version,
     global_heaps_size, 
     heap_type, 
     logical_processors, 
     otp_release, 
     process_count,
     smp_support,
     system_version,
     system_architecture,
     threads_enabled,
     thread_pool_size,
     wordsize].
