%% -------------------------------------------------------------------
%%
%% Copyright (c) 2007-2011 Basho Technologies, Inc.  All Rights Reserved.
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

-module(riak_core_stat).

%% API
-export([get_stats/0, get_stats/1, update/1]).

%% Metrics API
-export([stat_specs/0]).

-spec stat_specs() -> riak_core_metric:stat_specs().
stat_specs() ->
    [{riak_core_ignored_gossip_total, [{type, counter}, {group, gossip}]},
     {riak_core_rejected_handoffs, [{type, counter}, {group, gossip}]},
     {riak_core_handoff_timeouts, [{type, counter}, {group, gossip}]},
     {riak_core_gossip_received, [{type, meter}, {group, gossip}]},
     {riak_core_rings_reconciled, [{type, meter}, {group, gossip}]},
     {riak_core_converge_delay, [{type, duration}, {group, gossip}]},
     {riak_core_rebalance_delay, [{type, duration}, {group, gossip}]},
     {riak_core_vnode_queue, [{type, vnodeq}, {group, vnodeq}]},
     {riak_core_host, [{type, host}, {group, system}]},
     {riak_core_erl, [{type, erl}, {group, system}]},
     {riak_core_ring, [{type, ring}, {group, system}]},
     {riak_core_env, [{type, env}, {group, system}]}
    ].

%% @spec get_stats() -> proplist()
%% @doc Get the current aggregation of stats.
get_stats() ->
    produce_stats(5).

get_stats(Level) ->
    produce_stats(Level).

%% @doc Update the given stat
-spec update(Stat::atom()) -> ok.
update(converge_timer_begin) ->
    riak_core_metric_proc:update(riak_core_converge_delay, start);
update(converge_timer_end) ->
    riak_core_metric_proc:update(riak_core_converge_delay, stop);
update(rebalance_timer_begin) ->
    riak_core_metric_proc:update(riak_core_rebalance_delay, start);
update(rebalance_timer_end) ->
    riak_core_metric_proc:update(riak_core_rebalance_delay, stop);
update(rejected_handoffs) ->
    riak_core_metric_proc:update(riak_core_rejected_handoffs, 1);
update(handoff_timeouts) ->
    riak_core_metric_proc:update(riak_core_handoff_timeouts, 1);
update(ignored_gossip) ->
    riak_core_metric_proc:update(riak_core_ignored_gossip_totals, 1);
update(gossip_received) ->
    riak_core_metric_proc:update(riak_core_gossip_received, {1, slide:moment()});
update(rings_reconciled) ->
    riak_core_metric_proc:update(riak_core_rings_reconciled, {1, slide:moment()});
update(_) ->
    ok.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
%% @spec produce_stats(Presentation : atom()) -> proplist()
%% @doc Produce a proplist-formatted view of the current aggregation
%%      of stats.
produce_stats(Level) ->
    lists:flatten([riak_core_metric_proc:value(Name, Level) || {Name, _} <- stat_specs() ]).
