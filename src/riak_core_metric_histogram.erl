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

%% @doc An histogram . Wraps slide.
-module(riak_core_metric_histogram).

-behaviour(riak_core_metric).

-export([new/0, value/3, value/4, update/2, options/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export_type([display_spec/0]).

-type field()         :: count | mean | median | '95' | '99' | '100'.
-type display_spec()   :: [field() | {field(), Name :: atom()}].

-define(FIELDS, [min, max, mean, count, stddev, p50, p95, p99]).

%% Behaviour
%% @doc a new, fresh histogram
new() ->
    {ok, H} = basho_metrics_nifs:histogram_new(),
    H.

options(_State) ->
    ?FIELDS.

value(Level, Name, Histo)  ->
    Fields = fields(Level),
    Stats = basho_metrics_nifs:histogram_stats(Histo),
    display(Name, Fields, Stats, []).

value(_Level, Field, Name,  Histo) ->
    Stats = basho_metrics_nifs:histogram_stats(Histo),
    FieldName = riak_core_metric:join_as_atom([Name, '_',  Field]),
    {FieldName, proplists:get_value(Field, Stats)}.

%% @doc update histogram with Reading for given Moment
update({Reading, _Moment}, Histo) ->
    ok = basho_metrics_nifs:histogram_update(Histo, Reading),
    Histo.

%% @doc produce a proplist containing only specified fields
display(_Name, [], _Stat, Acc) ->
    lists:reverse(Acc);
display(Name, [Field|Rest], Stats, Acc) ->
    FieldName = riak_core_metric:join_as_atom([Name, '_',  Field]),
    Item = {FieldName, proplists:get_value(Field, Stats)},
    display(Name, Rest, Stats, [Item|Acc]).

fields(0) ->
    [count];
fields(1) ->
    [count, min, max, mean];
fields(_) ->
    ?FIELDS.

-ifdef(TEST).

display_test_() ->
    {setup,
     fun() -> 
             riak_core_metric_histogram:new() end,
     fun(_) -> ok end,
     fun(Histo) ->
             [?_assertEqual([{histo_count, 0}], riak_core_metric_histogram:value(0, histo, Histo)),
              ?_assertEqual([{histo_count, 0},
                             {histo_min, 0},
                             {histo_max, 0},
                             {histo_mean, 0}], riak_core_metric_histogram:value(1, histo, Histo)),
              ?_assertEqual([{histo_count, 0},
                             {histo_min, 0},
                             {histo_max, 0},
                             {histo_mean, 0}], riak_core_metric_histogram:value(1, histo, Histo)),
              ?_assertEqual([{histo_min,0},
                             {histo_max,0},
                             {histo_mean,0},
                             {histo_count,0},
                             {histo_stddev,0},
                             {histo_p50,0},
                             {histo_p95,0},
                             {histo_p99,0}], riak_core_metric_histogram:value(2, histo, Histo)),
             ?_assertEqual({histo_p99, 0}, riak_core_metric_histogram:value(10000, p99, histo, Histo)),
             ?_assertEqual({histo_nonsuch, undefined}, riak_core_metric_histogram:value(-88888, nonsuch, histo, Histo))]
     end}.

-endif.
