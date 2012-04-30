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

%% @doc Read only stat for the vnodeq
%% Is a metric for querying by level/field
-module(riak_core_metric_vnodeq).

-behaviour(riak_core_metric).

-export([new/0, value/3, value/4,  update/2, options/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(FIELDS, [running, min, median, mean, max, total]).

new() ->
    vnodeq.

options(vnodeq) ->
    [{App, ?FIELDS} || {App, _Mod} <- riak_core:vnode_modules()].

update(_Arg, vnodeq) ->
    vnodeq.

%% Provide aggregate stats for vnode queues.  Compute instantaneously for now,
%% may need to cache if stats are called heavily (multiple times per seconds)
value(Level, _Name, vnodeq) ->
    Fields = fields(Level),
    vnodeq_stats(Fields, riak_core_vnode_manager:all_vnodes()).

value(Level, Service, _Name, vnodeq) when is_atom(Service) ->
    Fields = fields(Level),
    VnodeMod = proplists:get_value(Service, riak_core:vnode_modules()),
    vnodeq_stats(Fields, riak_core_vnode_manager:all_vnodes(VnodeMod));
value(_Level, [Service, Field], _Name,  vnodeq) ->
    VnodeMod = proplists:get_value(Service, riak_core:vnode_modules()),
    vnodeq_stats([Field], riak_core_vnode_manager:all_vnodes(VnodeMod)).

vnode_info(VNodes) ->
    [{Service, element(2, erlang:process_info(Pid, message_queue_len))} ||
                     {Service, _Index, Pid} <- VNodes].

service_info(VnodesInfo) ->
    lists:foldl(fun({S,MQL}, A) ->
                        orddict:append_list(S, [MQL], A)
                end, orddict:new(), VnodesInfo).

vnodeq_stats(Fields, VNodes) ->
    VnodesInfo = vnode_info(VNodes),
    ServiceInfo = service_info(VnodesInfo),
    lists:flatten([vnodeq_aggregate(S, MQLs, Fields) || {S, MQLs} <- ServiceInfo]).

vnodeq_aggregate(_Service, [],  _Fields) ->
    []; % no vnodes, no stats
vnodeq_aggregate(Service, MQLs0, Fields) ->
    MQLs = lists:sort(MQLs0),
    Len = length(MQLs),
    Total = lists:sum(MQLs),
    Mean = Total div Len,
    Median = case (Len rem 2) of
                 0 -> % even number, average middle two
                     (lists:nth(Len div 2, MQLs) +
                      lists:nth(Len div 2 + 1, MQLs)) div 2;
                 1 ->
                     lists:nth(Len div 2 + 1, MQLs)
             end,
    Stats = [{running, Len}, 
             {min, lists:nth(1, MQLs)},
             {median, Median}, 
             {mean, Mean}, 
             {max, lists:nth(Len, MQLs)},
             {total, Total}],
    [{riak_core_metric:join_as_atom([Service, vnode_suffix(Field), Field]), 
      proplists:get_value(Field, Stats)} || Field <- Fields].

fields(0) ->
    [running, total];
fields(Level) when Level >0 ->
    ?FIELDS.

vnode_suffix(running) ->
    's_';
vnode_suffix(_) ->
    'q_'.

-ifdef(TEST).

%% Check vnodeq aggregation function
vnodeq_aggregate_empty_test() ->
    ?assertEqual([], vnodeq_aggregate(service_vnode, [], ?FIELDS)).

vnodeq_aggregate_odd1_test() ->
    ?assertEqual([{service_vnodes_running, 1},
                  {service_vnodeq_min, 10},
                  {service_vnodeq_median, 10},
                  {service_vnodeq_mean, 10},
                  {service_vnodeq_max, 10},
                  {service_vnodeq_total, 10}],
                 vnodeq_aggregate(service_vnode, [10], ?FIELDS)).

vnodeq_aggregate_odd3_test() ->
    ?assertEqual([{service_vnodes_running, 3},
                  {service_vnodeq_min, 1},
                  {service_vnodeq_median, 2},
                  {service_vnodeq_mean, 2},
                  {service_vnodeq_max, 3},
                  {service_vnodeq_total, 6}],
                 vnodeq_aggregate(service_vnode, [1, 2, 3], ?FIELDS)).

vnodeq_aggregate_odd5_test() ->
    ?assertEqual([{service_vnodes_running, 5},
                  {service_vnodeq_min, 0},
                  {service_vnodeq_median, 1},
                  {service_vnodeq_mean, 2},
                  {service_vnodeq_max, 5},
                  {service_vnodeq_total, 10}],
                 vnodeq_aggregate(service_vnode, [1, 0, 5, 0, 4], ?FIELDS)).

vnodeq_aggregate_even2_test() ->
    ?assertEqual([{service_vnodes_running, 2},
                  {service_vnodeq_min, 10},
                  {service_vnodeq_median, 15},
                  {service_vnodeq_mean, 15},
                  {service_vnodeq_max, 20},
                  {service_vnodeq_total, 30}],
                 vnodeq_aggregate(service_vnode, [10, 20], ?FIELDS)).

vnodeq_aggregate_even4_test() ->
    ?assertEqual([{service_vnodes_running, 4},
                  {service_vnodeq_min, 0},
                  {service_vnodeq_median, 5},
                  {service_vnodeq_mean, 7},
                  {service_vnodeq_max, 20},
                  {service_vnodeq_total, 30}],
                 vnodeq_aggregate(service_vnode, [0, 10, 0, 20], ?FIELDS)).

-endif.
