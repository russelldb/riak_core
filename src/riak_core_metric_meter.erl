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
-export([new/0, value/2, value/3, update/2]).

-export([tick/1]).

%% @doc create a new meter.
-spec new() -> spiraltime:spiral().
new() ->
    {ok, M} = basho_metrics_nifs:meter_new(),
    M.

%% @doc format the number of entries in the last minute as
%% {name, count}.
-spec value(atom(), spiraltime:spiral()) ->
                   {atom(), integer()}.
value(Name, Meter) ->
    Stats = basho_metrics_nifs:meter_stats(Meter),
    {Name, proplists:get_value(count, Stats)}.

%% @doc format the number of entries in the last minute as
%% {name, count}.
-spec value(_, atom(), spiraltime:spiral()) ->
                   {atom(), integer()}.
value({display_name, Name}, _StatName, Meter) ->
    value(Name, Meter).

%% @doc update the entry for the given Moment by Amount,
%%  in the given Meter
-spec update({integer(), integer()}, spiraltime:spiral()) ->
                    spiraltime:spiral().
update({Amount, _Moment}, Meter) ->
    ok = basho_metrics_nifs:meter_update(Meter, Amount),
    Meter.

tick(Meter) ->
    basho_metrics_nifs:meter_tick(Meter).
