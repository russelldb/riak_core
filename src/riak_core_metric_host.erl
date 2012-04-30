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
-module(riak_core_metric_host).

-behaviour(riak_core_metric).

-export([new/0, value/3, value/4,  update/2, options/1]).

new() ->
    host.

options(host) ->
    [{cpu, [nprocs, avg1, avg5, avg15]},
     {mem, [total, allocated]},
     {disk, [Id || {Id, _, _} <- disksup:get_disk_data()]}].
        
update(_Arg, host) ->
    host.

value(_Level, _Name, host) ->
    lists:flatten([value(Field) || Field <- [cpu, mem, disk]]).

value(_Level, Field, _Name, host) when is_atom(Field) ->
    value(Field);
value(_Level, [disk, Disk], _Name, host) ->
    {disk, Disks} = value(disk),
    proplists:lookup(Disk, Disks);
value(_Level, [Field, Elem], _Name, host) ->
    Stat = value(Field),
    ElemName = riak_core_metric:join_as_atom([Field, '_', Elem]),
    proplists:lookup(ElemName, Stat).

value(cpu) ->
    [{cpu_nprocs, cpu_sup:nprocs()},
     {cpu_avg1, cpu_sup:avg1()},
     {cpu_avg5, cpu_sup:avg5()},
     {cpu_avg15, cpu_sup:avg15()}];
value(mem) ->
    {Total, Alloc, _} = memsup:get_memory_data(),
    [{mem_total, Total},
     {mem_allocated, Alloc}];
value(disk) ->
    {disk, disksup:get_disk_data()}.
