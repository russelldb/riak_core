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

%% @doc A Meter. Wraps spiraltime
-module(riak_core_metric_meter).

-behaviour(riak_core_metric).

%% Behaviour API
-export([new/0, value/3, value/4, update/2, options/1]).

-export([tick/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

new() ->
    {ok, M} = basho_metrics_nifs:meter_new(),
    M.

options(_Stat) ->
    [count, one, five, fifteen].

%% @doc format the number of entries in the last minute as
%% {name, count}.
value(Level, Name, Meter) ->
    [value(Level, Field, Name, Meter) || Field <- fields(Level)].

value(_Level, Field, Name, Meter) ->
    Stats = basho_metrics_nifs:meter_stats(Meter),
    StatName = riak_core_metric:join_as_atom([Name, '_', Field]),
    Stat = proplists:get_value(Field, Stats),
    {StatName, safe_trunc(Stat)}.

update({Amount, _Moment}, Meter) ->
    ok = basho_metrics_nifs:meter_update(Meter, Amount),
    Meter.

tick(Meter) ->
    basho_metrics_nifs:meter_tick(Meter).

fields(0) ->
    [count, one];
fields(Level) when is_integer(Level) ->
    options(ok).

safe_trunc(N) when is_number(N) ->
    trunc(N);
safe_trunc(Else) ->
    Else.

-ifdef(TEST).

display_test_() ->
    {setup,
     fun() ->
             riak_core_metric_meter:new() end,
     fun(_) ->
             ok end,
     fun(Meter) ->
             [?_assertEqual([{stat_count, 0}, {stat_one, 0}], riak_core_metric_meter:value(0, stat, Meter))] ++
                 [?_assertEqual([{stat_count, 0},
                                 {stat_one, 0},
                                 {stat_five, 0},
                                 {stat_fifteen, 0}],
                                riak_core_metric_meter:value(Level, stat, Meter)) || Level <- lists:seq(1, 5)] ++
                 [?_assertEqual({stat_count, 0}, riak_core_metric_meter:value(1, count, stat, Meter)),
                  ?_assertEqual({stat_one, 0}, riak_core_metric_meter:value(1, one, stat, Meter)),
                  ?_assertEqual({stat_five, 0}, riak_core_metric_meter:value(1, five, stat, Meter)),
                  ?_assertEqual({stat_fifteen, 0}, riak_core_metric_meter:value(1, fifteen, stat, Meter))]
     end}.
-endif.
