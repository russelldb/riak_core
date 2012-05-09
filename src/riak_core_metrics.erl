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

%% @doc riak_core_metrics gen_server for stat querying
-module(riak_core_metrics).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% query API
-export([list_stats/0, list_stats/1, 
         list_stats/2, options/1, refresh/0,
         get_stats/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(state, {stats=[]}).

%% @doc display a tree of all stats managed
%% List is cached for N seconds
%% call refresh/0 to refresh stat list
-spec list_stats() -> [term()].
list_stats() ->
    gen_server:call(?SERVER, list_all).

%% @doc a tree of stats for the given App
list_stats(App) ->
    gen_server:call(?SERVER, {list, App}).

list_stats(App, Group) ->
    gen_server:call(?SERVER, {list, App, Group}).

options(Stat) ->
    gen_server:call(?SERVER, {options, Stat}).

get_stats(Level, Path) ->
    gen_server:call(?SERVER, {display, Level, Path}).

refresh() ->
    gen_server:cast(?SERVER, refresh).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    Stats = list_all(),
    timer:send_interval(5000, refresh), %% configureable?
    {ok, #state{stats=Stats}}.

handle_call(list_all, _From, State=#state{stats=Stats}) ->
    {reply, Stats, State};
handle_call({list, App}, _From, State=#state{stats=Stats}) ->
    AppStats = proplists:get_value(App, Stats, []),
    {reply, AppStats, State};
handle_call({list, App, Group}, _From, State=#state{stats=Stats}) ->
    GrpStats = proplists:get_value(Group, proplists:get_value(App, Stats, []), []),
    {reply, GrpStats, State};
handle_call({options, Stat}, _From, State) ->
    Options = riak_core_metric_proc:options(Stat),
    {reply, Options, State};
handle_call({display, Level, Path}, _From, State=#state{stats=Stats}) ->
    StatValues = path(Level, Path, Stats),
    {reply, StatValues, State};
handle_call(refresh, _From, State) ->
    {reply, State#state{stats=list_all()}};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(refresh, State) ->
    {noreply, State#state{stats=list_all()}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
list_all() ->
    lists:foldl(fun({{App, Stat, Group}, Pid, _, _}, AccIn) ->
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
                supervisor:which_children(riak_core_metric_sup)).

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

%% Path is a list of tokens
%% [string() | atom() | binary() | number()]
%% Attempts to resolve Path down to a stat / set of stats to display
path(Level, [], _StatList) ->
    stats(Level);
path(Level, Path, StatList) when not is_list(Path) ->
    path(Level, [Path], StatList);
path(Level, Path, StatList) ->
    case parse_path(StatList, Path) of
        {undefined, undefined, undefined, []} ->
            [];
        {App, undefined, undefined, []} ->
            stats(App, Level);
        {App, Group, undefined, []} ->
            stats(App, Group,  Level);
        {_, _, Name, []} ->
            stat(Name, Level);
        {_, _, Name, [Option]} ->
            stat(Name, Level, Option);
        {_, _, Name, Options} ->
            stat(Name, Level, Options)
    end.

parse_path(Stats, []) ->
    Stats;
parse_path(Stats0, [Hd|Tl]) ->
    case is_member(Hd, Stats0) of
        false ->
            %% It *must* be a stat, or bail
            Grps = lists:flatten([Grp || {_App, Grp} <- Stats0]),
            FlatStats = lists:flatten([S || {_Grp, S} <- Grps]),
            {Stat, Path} =  parse_stat(Hd, Tl, FlatStats),
            {undefined, undefined, Stat, Path};
        {true, App, AppStats} ->
            %% do we have a nothing, a group or a stat next?
            case Tl of
                [] ->
                    {App, undefined, undefined, []};
                [Hd2|Tl2] ->
                    case is_member(Hd2, AppStats) of
                        false -> %% *must* be a stat, or gibberish
                            FlatStats = lists:flatten([S || {_Grp, S} <- AppStats]),
                            {Stat, Path} = parse_stat(Hd2, Tl2, FlatStats),
                            {App, undefined, Stat, Path};
                        {true, Group, GroupStats} ->
                            %% do we have *more*?
                            case Tl2 of
                                [] ->
                                    {App, Group, undefined, []};
                                [MaybeStat|MaybePath] ->
                                    {Stat, Path} = parse_stat(MaybeStat, MaybePath, GroupStats),
                                    {App, Group, Stat, Path}
                            end
                    end
            end
    end.

%% see if the token is in the list, regardless of type
is_member(MaybeMem, Stats) when is_list(MaybeMem) ->
    is_member(MaybeMem, Stats, fun(X) -> atom_to_list(X) end, fun(X) -> list_to_existing_atom(X) end);
is_member(MaybeMem, Stats) when is_binary(MaybeMem) ->
    is_member(MaybeMem, Stats, fun(X) -> atom_to_binary(X, latin1) end, fun(X) -> binary_to_existing_atom(X, latin1) end);
is_member(MaybeMem, Stats) when is_atom(MaybeMem) ->
    is_member(MaybeMem, Stats, fun(X) -> X  end, fun(X) -> X end).

is_member(MaybeMem, Stats, InConv, OutConv) ->
    case lists:member(MaybeMem, [InConv(Mem) || {Mem, _} <- Stats]) of
        true ->
            OutMem = OutConv(MaybeMem),
            {true, OutMem, proplists:get_value(OutMem, Stats)};
        false ->
            false
    end.

parse_stat(Stat0, Path, Stats) ->
    case is_member(Stat0, Stats) of
        false ->
            {undefined, []};
        {true, Stat, Options} ->
            %% match options
            {Stat, parse_options(Options, Path, [])}
    end.

parse_options(_Opts, [], Acc) ->
    lists:reverse(Acc);
parse_options([], _Path, Acc) ->
    lists:reverse(Acc);
parse_options([{Opt, SubOpts}|Options], [Part|Rest]=Path, Acc) ->
    case typless_equals(Opt, Part) of
        true ->
            parse_options(SubOpts, Rest, [Opt|Acc]);
        _  ->
            parse_options(Options, Path, Acc)
    end;
parse_options([Opt|Options], [Part|_Rest]=Path, Acc) ->
    case typless_equals(Opt, Part) of
        true ->
            lists:reverse([Opt|Acc]);
        _ ->
            parse_options(Options, Path, Acc)
    end.

typless_equals(Opt, Part) ->
    as_list(Opt) == as_list(Part).

as_list(Bin) when is_binary(Bin) ->
    binary_to_list(Bin);
as_list(L) when is_list(L) ->
    L;
as_list(N) when is_integer(N) ->
    integer_to_list(N);
as_list(N) when is_float(N) ->
    float_to_list(N);
as_list(A) when is_atom(A) ->
    atom_to_list(A).

