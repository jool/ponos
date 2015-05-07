%%%
%%%   Copyright (c) 2014, Klarna AB
%%%
%%%   Licensed under the Apache License, Version 2.0 (the "License");
%%%   you may not use this file except in compliance with the License.
%%%   You may obtain a copy of the License at
%%%
%%%       http://www.apache.org/licenses/LICENSE-2.0
%%%
%%%   Unless required by applicable law or agreed to in writing, software
%%%   distributed under the License is distributed on an "AS IS" BASIS,
%%%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%%   See the License for the specific language governing permissions and
%%%   limitations under the License.
%%%

%%%_* Module Declaration ===============================================
-module(ponos_statistics).

%%%_* Exports ==========================================================
%% Client API
-export([ get_average_load/1
        , get_call_counter/1
        , get_response_time_min/1
        , get_response_time_max/1
        , get_response_time_percentile/2
        , get_start/1
        ]).

%% Internal API
-export([ add_response_time/2
        , increase_call_counter/1
        , new/0
        , reset_start/1
        ]).

-export_type([ ponos_statistics/0
             ]).


%%%_* Records and Definitions ==========================================
-record(state, {
          call_counter             :: integer(),
          call_counter_per_second  :: gb_trees:tree(),
          response_times           :: gb_trees:tree(),
          start                    :: erlang:timestamp()
         }).


%%%_* Types ------------------------------------------------------------
-opaque ponos_statistics() :: #state{}.

%%%_* Code =============================================================
%%%_* Client API -------------------------------------------------------

-spec get_average_load(ponos_statistics()) -> float().
%% @doc Return average load (calls per second) last 2 seconds.
get_average_load(State) ->
  Now = os:timestamp(),
  Sec = calendar:datetime_to_gregorian_seconds(calendar:now_to_datetime(Now)),
  get_average_load(State, Sec).

-spec get_average_load(ponos_statistics(), integer()) -> float().
get_average_load(State, Second) ->
  Iterator = gb_trees:iterator(State#state.call_counter_per_second),
  case tree_filter(gb_trees:next(Iterator), Second - 3, []) of
    []           -> 0.0;
    FilteredList -> lists:sum(FilteredList) / length(FilteredList)
  end.

%% @private
tree_filter(none, _Second, []) ->
  [];
tree_filter(none, _Second, Acc) ->
  lists:reverse(tl(Acc));
tree_filter({Key, Value, Iterator}, Second, Acc) when Key > Second ->
  tree_filter(gb_trees:next(Iterator), Second, [Value|Acc]);
tree_filter({_, _, Iterator}, Second, Acc) ->
  tree_filter(gb_trees:next(Iterator), Second, Acc). %% skip

-spec get_call_counter(ponos_statistics()) -> integer().
get_call_counter(#state{call_counter = CC}) ->
  CC.

-spec get_start(ponos_statistics()) -> erlang:now().
get_start(#state{start = Start}) ->
  Start.

-spec get_response_time_min(ponos_statistics()) -> integer().
get_response_time_min(State) ->
  case gb_trees:is_empty(State#state.response_times) of
    true ->
      undefined;
    false ->
      {Min, _Count} = gb_trees:smallest(State#state.response_times),
      Min
  end.

get_response_time_max(State) ->
  case gb_trees:is_empty(State#state.response_times) of
    true ->
      undefined;
    false ->
      {Max, _Count} = gb_trees:largest(State#state.response_times),
      Max
  end.

get_response_time_percentile(State, Percentile) ->
  case gb_trees:is_empty(State#state.response_times) of
    true  -> undefined;
    false -> do_get_response_time_percentile(State, Percentile)
  end.

do_get_response_time_percentile(State, Percentile) ->
  RTs = gb_trees:to_list(State#state.response_times),
  calculate_rank_and_return_percentile(expand_response_times(RTs), Percentile).

calculate_rank_and_return_percentile(ResponseTimes, Percentile) ->
  Rank = Percentile/100 * length(ResponseTimes),
  case trunc(Rank) == Rank of
    true  -> lists:nth(trunc(Rank), ResponseTimes);
    false -> lists:nth(trunc(Rank)+1, ResponseTimes)
  end.

expand_response_times(ResponseTimes) ->
  lists:flatten([[K || _ <- lists:seq(1, V)] || {K,V} <- ResponseTimes]).


%%%_* Internal ---------------------------------------------------------
-spec add_response_time(integer(), ponos_statistics()) -> ponos_statistics().
add_response_time(ResponseTime, State = #state{response_times = RT}) ->
  case gb_trees:lookup(ResponseTime, RT) of
    none ->
      State#state{response_times = gb_trees:enter(ResponseTime, 1, RT)};
    {value, Count} ->
      State#state{response_times = gb_trees:enter(ResponseTime, Count+1, RT)}
  end.

-spec increase_call_counter(ponos_statistics()) -> ponos_statistics().
increase_call_counter(#state{call_counter = CC} = State) ->
  (increase_call_counter_per_second(State))#state{call_counter = CC + 1}.

increase_call_counter_per_second(State) ->
  CPS  = State#state.call_counter_per_second,
  Now  = os:timestamp(),
  Secs = calendar:datetime_to_gregorian_seconds(calendar:now_to_datetime(Now)),
  CurrentCount = case gb_trees:lookup(Secs, CPS) of
                   none           -> 0;
                   {value, Count} -> Count
                 end,
  State#state{
    call_counter_per_second = gb_trees:enter(Secs, CurrentCount+1, CPS)
   }.

-spec new() -> ponos_statistics().
new() ->
  #state{
     call_counter            = 0,
     call_counter_per_second = gb_trees:empty(),
     response_times          = gb_trees:empty(),
     start                   = os:timestamp()
    }.

-spec reset_start(ponos_statistics()) -> ponos_statistics().
reset_start(State) ->
  State#state{start = os:timestamp()}.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
