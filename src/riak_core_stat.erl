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

%% query API
-export([list_stats/0, stats/1, stats/2, stats/3, stat/2, stat/3]).

%% Metrics API
-export([stat_specs/0]).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec stat_specs() -> riak_core_metric:stat_specs().
stat_specs() ->
    [{ignored_gossip_total, [{type, counter}, {group, gossip}]},
     {rejected_handoffs, [{type, counter}, {group, gossip}]},
     {handoff_timeouts, [{type, counter}, {group, gossip}]},
     {gossip_received, [{type, meter}, {group, gossip}]},
     {rings_reconciled, [{type, meter}, {group, gossip}]},
     {converge_delay, [{type, duration}, {group, gossip}]},
     {rebalance_delay, [{type, duration}, {group, gossip}]},
     {vnode_queue, [{type, vnodeq}, {group, vnodeq}]},
     {host, [{type, host}, {group, system}]},
     {erl, [{type, erl}, {group, system}]},
     {ring, [{type, ring}, {group, system}]},
     {env, [{type, env}, {group, system}]}
    ].

%%%
list_stats() ->
    AppDict = lists:foldl(fun({{App, Stat, Group}, Pid, _, _}, AccIn) ->
                                  {ok, {Stat, _Opts}=Options} = gen_server:call(Pid, options),
                                  case lists:keyfind(App, 1, AccIn) of
                                      false ->
                                          [{App, [{Group, [Options]}]}|AccIn];
                                      {App, Groups} ->
                                          case lists:keyfind(Group, 1, Groups) of
                                              false ->
                                                  lists:keyreplace(App, 1, AccIn, {App, [{Group, [Options]}|Groups]});
                                              {Group, Stats} ->
                                                  Groups2 = lists:keyreplace(Group, 1, Groups, {Group, [Options|Stats]}),
                                                  lists:keyreplace(App, 1, AccIn, {App, Groups2})
                                          end
                                  end
                          end,
                          [],
                          supervisor:which_children(riak_core_metric_sup)),
    orddict:to_list(AppDict).

stats(Level) when is_integer(Level) ->
    lists:flatten([riak_core_metric_proc:value(Name, Level) || {{_App, Name, _Group}, _, _, _} <-
                                                                    supervisor:which_children(riak_core_metric_sup)]).

stats(App, Level) ->
    lists:flatten([riak_core_metric_proc:value(Name, Level) || {{StatApp, Name, _Group}, _, _, _} <-
                                                                    supervisor:which_children(riak_core_metric_sup),
                  App =:= StatApp]).

stats(App, Group, Level) ->
    lists:flatten([riak_core_metric_proc:value(Name, Level) || {{StatApp, Name, StatGroup}, _, _, _} <-
                                                                    supervisor:which_children(riak_core_metric_sup),
                  App =:= StatApp, Group =:= StatGroup]).

stat(Name, Level) ->
    riak_core_metric_proc:value(Name, Level).

stat(Name, Level, Spec) ->
    riak_core_metric_proc:value(Name, Level, Spec).

%% @spec get_stats() -> proplist()
%% @doc Get the current aggregation of stats.
get_stats() ->
    produce_stats(5).

get_stats(Level) ->
    produce_stats(Level).

%% @doc Update the given stat
-spec update(Stat::atom()) -> ok.
update(converge_timer_begin) ->
    riak_core_metric_proc:update(converge_delay, start);
update(converge_timer_end) ->
    riak_core_metric_proc:update(converge_delay, stop);
update(rebalance_timer_begin) ->
    riak_core_metric_proc:update(rebalance_delay, start);
update(rebalance_timer_end) ->
    riak_core_metric_proc:update(rebalance_delay, stop);
update(rejected_handoffs) ->
    riak_core_metric_proc:update(rejected_handoffs, 1);
update(handoff_timeouts) ->
    riak_core_metric_proc:update(handoff_timeouts, 1);
update(ignored_gossip) ->
    riak_core_metric_proc:update(ignored_gossip_totals, 1);
update(gossip_received) ->
    riak_core_metric_proc:update(gossip_received, {1, slide:moment()});
update(rings_reconciled) ->
    riak_core_metric_proc:update(rings_reconciled, {1, slide:moment()});
update(_) ->
    ok.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
%% @spec produce_stats(Presentation : atom()) -> proplist()
%% @doc Produce a proplist-formatted view of the current aggregation
%%      of stats.
produce_stats(Level) ->
    lists:flatten([ riak_core_metric_proc:value(Name, Level) || {Name, _} <- stat_specs() ]).
