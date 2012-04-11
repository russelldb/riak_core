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

%% @doc A meter and a total counter
-module(riak_core_metric_tmeter).

-behaviour(riak_core_metric).

%% Behaviour API
-export([new/0, value/2, value/3, update/2, tick/1]).

new() ->
    {0, riak_core_metric_meter:new()}.

update({Cnt, Moment}, {Counter, Meter}) ->    
    {Counter+Cnt, riak_core_metric_meter:update({Cnt, Moment}, Meter)}.

value(Name, Value) ->
    CounterName = riak_core_metric:join_as_atom([Name, '_', total]),
    value({CounterName, Name}, Name, Value).

value({CounterName, MeterName}, _Name, {Counter, Meter}) ->
    [{CounterName, Counter},
     riak_core_metric_meter:value(MeterName, Meter)].

tick({_Counter, Meter}) ->
    riak_core_metric_meter:tick(Meter).
