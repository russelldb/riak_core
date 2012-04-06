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

-export([new/0, value/2, value/3,  update/2]).

-export_type([display_spec/0]).

-type display()       :: [{field(), integer()}].
-type field()         :: count | mean | median | '95' | '99' | '100'.
-type display_spec()  :: [ args() | fields() | prefix() ].
-type args()          :: {args, {Min::integer(), Max::integer(), Bins::integer(),
                          RoundingMode:: up | down}}.
-type fields()        :: {fields, [fields()]}.
-type prefix()        :: {prefix, atom() | string() | binary() | integer()}.


%% Behaviour
%% @doc a new, fresh histogram
-spec new() -> slide:slide().
new() ->
    {ok, H} = basho_metrics_nifs:histogram_new(),
    H.

%% @doc Sum of readings from now to 'window size' seconds ago.
%%      Returns total number of readings and the sum of those
%%      readings.
-spec value(atom(), slide:slide()) ->
                   {atom(), {non_neg_integer(), number()}}.
value(Name, Histo) ->
    Stats = basho_metrics_nifs:histogram_stats(Histo),
    Count = proplists:get_value(count, Stats),
    {Name, Count}.

%% @doc returns the fields of the histogram defined in the
%%      display spec. Use the 'args' in the display spec
%%      to produce results.
%% @see slide:mean_and_nines/6
-spec value(display_spec(), atom(), slide:slide()) ->
                   display().
value(DisplaySpec, Name,  Histo) ->
    Fields = proplists:get_value(fields, DisplaySpec),
    Prefix = proplists:get_value(prefix, DisplaySpec),
    Stats0 = basho_metrics_nifs:histogram_stats(Histo),
    Stats = lists:map(fun({p50, V}) -> {median, V};
                         ({p95, V}) -> {'95', V};
                         ({p99, V}) -> {'99', V};
                         ({max, V}) -> {'100', V};
                         (E)        -> E end,
                      Stats0),
    FieldPrefix = field_prefix(Prefix, Name),
    display(FieldPrefix, Fields, Stats, []).

%% @doc update histogram with Reading for given Moment
-spec update({integer(), integer()}, slide:slide()) ->
                    slide:slide().
update({Reading, _Moment}, Histo) ->
    ok = basho_metrics_nifs:histogram_update(Histo, Reading),
    Histo.

%% @doc add a prefix Prefix_ to the given Field
-spec field_prefix(atom(), field()) ->
                          atom().
field_prefix(undefined, Name) ->
    Name;
field_prefix(Prefix, Name) ->
    riak_core_metric:join_as_atom([Prefix, '_', Name]).

%% @doc produce a proplist containing only specified fields
-spec display(atom(), [field()], display(), display()) ->
                     display().
display(_Prefix, [], _Stat, Acc) ->
    lists:reverse(Acc);
display(Prefix, [{Field, Name}|Rest], Stats, Acc) ->
    Item = {Name, proplists:get_value(Field, Stats)},
    display(Prefix, Rest, Stats, [Item|Acc]);
display(Prefix, [Field|Rest], Stats, Acc) ->
    Name = riak_core_metric:join_as_atom([Prefix, '_',  Field]),
    Item = {Name, proplists:get_value(Field, Stats)},
    display(Prefix, Rest, Stats, [Item|Acc]).
