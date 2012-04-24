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

%% @doc tracks a duration, min, max, last, mean
-module(riak_core_metric_duration).

-behaviour(riak_core_metric).

%% Behaviour API
-export([new/0, value/3, value/4, update/2, options/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(cuml, {count =  0 :: integer(),
               min        :: integer(),
               max   = -1 :: integer(),
               mean  =  0 :: integer(),
               last       :: integer(),
               start      :: calendar:t_now()}).

new() ->
    #cuml{}.

options(_State) ->
    fields(0).

value(Level, Name, Dur) ->
    display(Name, to_proplist(Dur), fields(Level), []).

value(_Level, Field, Name, Dur) ->
    display(Name, to_proplist(Dur), [Field], []).

-spec update(start, #cuml{}) -> #cuml{};
            (stop, #cuml{}) -> #cuml{}.
update(start, Dur) ->
    Dur#cuml{start=erlang:now()};
update(stop, #cuml{count=N, min=Min, max=Max, mean=Mean, start=T0}) ->
    Duration = timer:now_diff(erlang:now(), T0),
    Min2 = erlang:min(Min, Duration),
    Max2 = erlang:max(Max, Duration),
    Mean2 = ((N * Mean) + Duration) div (N+1),
    #cuml{count=N+1, min=Min2, max=Max2, mean=Mean2, last=Duration, start=undefined}.

%% internal
display(_Stat, _Cuml, [], Acc) ->
    lists:reverse(Acc);
display(Stat, Cuml, [Field|Rest], Acc) ->
    Name = riak_core_metric:join_as_atom([Stat, '_', Field]),
    Value = proplists:get_value(Field, Cuml),
    display(Stat, Cuml, Rest, [{Name, Value}|Acc]).

to_proplist(Cuml) when is_record(Cuml, cuml) ->
    lists:zip(record_info(fields, cuml), tl(tuple_to_list(Cuml))).

fields(_) ->
    [count, min, max, mean, last].

-ifdef(TEST).

display_test_() ->
    {setup,
     fun() ->
             Dur = riak_core_metric_duration:new(),
             Dur1 = riak_core_metric_duration:update(start, Dur),
             riak_core_metric_duration:update(stop, Dur1)
     end,
     fun(_) ->
             ok end,
     fun(Dur) ->
             [?_assertMatch([{dur_count, _},
                             {dur_min, _},
                             {dur_max, _},
                             {dur_mean, _},
                             {dur_last, _}], 
                            riak_core_metric_duration:value(Level, dur, Dur)) || Level <- lists:seq(0, 5)] ++
                 [?_assertMatch([{dur_count, _}], riak_core_metric_duration:value(0, count, dur, Dur)),
                  ?_assertEqual([{dur_buffalo, undefined}], riak_core_metric_duration:value(0, buffalo, dur, Dur))]
     end}.

-endif.
