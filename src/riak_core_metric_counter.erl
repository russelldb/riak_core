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

%% @doc A counter. Wraps an integer()
-module(riak_core_metric_counter).

-behaviour(riak_core_metric).

-export([new/0, value/3, value/4,  update/2, options/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% Behaviour
-spec new() -> 0.
new() ->
    0.

options(_State) ->
    [].

value(_Level, Name, Counter) ->
    {Name, Counter}.

value(Level, _Fields, Name, Counter) ->
    value(Level, Name, Counter).

update(Amount, Counter) when is_integer(Amount) ->
    erlang:max(Counter + Amount, 0).

-ifdef(TEST).

display_test_() ->
    {setup,
     fun() -> 
             riak_core_metric_counter:new() end,
     fun(_) -> ok end,
     fun(Counter) ->
             [?_assertEqual({stat, 0}, 
                            riak_core_metric_counter:value(Level, stat, Counter)) 
              || Level <- lists:seq(0, 5)] ++
                 [?_assertEqual({stat, 0}, riak_core_metric_counter:value(1, anything_at_all, stat, Counter))]
     end}.

update_test() ->
    Counter = riak_core_metric_counter:new(),
    Counter1 = riak_core_metric_counter:update(1, Counter),
    ?assertEqual({stat, 1}, riak_core_metric_counter:value(0, stat, Counter1)),
    Counter2 = riak_core_metric_counter:update(999, Counter1),
    ?assertEqual({stat, 1000}, riak_core_metric_counter:value(0, stat, Counter2)),
    Counter3 = riak_core_metric_counter:update(-1000, Counter2),
    ?assertEqual({stat, 0}, riak_core_metric_counter:value(0, stat, Counter3)),
    Counter4 = riak_core_metric_counter:update(-1000, Counter3),
    ?assertEqual({stat, 0}, riak_core_metric_counter:value(0, stat, Counter4)).
                 
-endif.
