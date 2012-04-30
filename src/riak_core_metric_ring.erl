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

%% TODO add the rings_reonciled stuff here?
-module(riak_core_metric_ring).

-behaviour(riak_core_metric).

-export([new/0, value/3, value/4, update/2, options/1]).

new() ->
    ring.

options(ring) ->
    fields(5).
        
update(_Arg, ring) ->
    ring.

value(Level, _Name, ring) ->
    {ok, R} = riak_core_ring_manager:get_my_ring(),
    [value(R, F) || F <- fields(Level)].

value(_Level, Field, _Name, ring) ->
    {ok, R} = riak_core_ring_manager:get_my_ring(),
    value(R, Field).

value(Ring, members) ->
    {ring_members, riak_core_ring:all_members(Ring)};
value(Ring, num_partitions) ->
    {ring_num_partitions, riak_core_ring:num_partitions(Ring)};
value(_Ring, creation_size) ->
    {ring_creation_size, app_helper:get_env(riak_core, ring_creation_size)};
value(Ring, ownership) ->    
    {ring_ownership, list_to_binary(lists:flatten(io_lib:format("~p", [dict:to_list(
                        lists:foldl(fun({_P, N}, Acc) ->
                                            case dict:find(N, Acc) of
                                                {ok, V} ->
                                                    dict:store(N, V+1, Acc);
                                                error ->
                                                    dict:store(N, 1, Acc)
                                            end
                                    end, dict:new(), riak_core_ring:all_owners(Ring)))])))}.

fields(0) ->
    [members, num_partitions, creation_size];
fields(_Level) ->
    fields(0) ++ [ownership].
