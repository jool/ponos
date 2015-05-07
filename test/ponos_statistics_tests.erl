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

%%% @doc
%%% @copyright Klarna AB, 2015
%%% @end

%%% Module Declaration =================================================
-module(ponos_statistics_tests).

-include_lib("eunit/include/eunit.hrl").

%%%_* Test Cases =======================================================
new_test_() ->
  Stat = ponos_statistics:new(),
  [ ?_assertEqual(0.0, ponos_statistics:get_average_load(Stat))
  , ?_assertEqual(0, ponos_statistics:get_call_counter(Stat))
  , ?_assertEqual(undefined, ponos_statistics:get_response_time_min(Stat))
  , ?_assertEqual(undefined, ponos_statistics:get_response_time_max(Stat))
  , ?_assertEqual(undefined, ponos_statistics:get_response_time_percentile(Stat,1))
  ].

increase_call_counter_test_() ->
  Stat = ponos_statistics:new(),
  [ ?_assertEqual(1, ponos_statistics:get_call_counter(
                       ponos_statistics:increase_call_counter(Stat)))
  , ?_assertEqual(2,
                  begin
                    Stat1 = ponos_statistics:increase_call_counter(Stat),
                    Stat2 = ponos_statistics:increase_call_counter(Stat1),
                    ponos_statistics:get_call_counter(Stat2)
                  end)
  ].

get_response_time_min_test_() ->
  Stat = ponos_statistics:new(),
  Stat1 = ponos_statistics:add_response_time(1432, Stat),
  Stat2 = ponos_statistics:add_response_time(1431, Stat1),
  Stat3 = ponos_statistics:add_response_time(1433, Stat2),
  [ ?_assertEqual(1432, ponos_statistics:get_response_time_min(Stat1))
  , ?_assertEqual(1431, ponos_statistics:get_response_time_min(Stat2))
  , ?_assertEqual(1431, ponos_statistics:get_response_time_min(Stat3))
  ].

get_response_time_max_test_() ->
  Stat = ponos_statistics:new(),
  Stat1 = ponos_statistics:add_response_time(1432, Stat),
  Stat2 = ponos_statistics:add_response_time(1431, Stat1),
  Stat3 = ponos_statistics:add_response_time(1433, Stat2),
  [ ?_assertEqual(1432, ponos_statistics:get_response_time_max(Stat1))
  , ?_assertEqual(1432, ponos_statistics:get_response_time_max(Stat2))
  , ?_assertEqual(1433, ponos_statistics:get_response_time_max(Stat3))
  ].

percentile_even_number_test_() ->
  Stat = ponos_statistics:new(),
  ResponseTimes = [3, 5, 7, 8, 9, 11, 13, 15],
  Stat1 = lists:foldl(fun(RT, Acc) ->
                          ponos_statistics:add_response_time(RT, Acc)
                      end,
                      Stat,
                      ResponseTimes),

  [ ?_assertEqual(5, ponos_statistics:get_response_time_percentile(Stat1, 25))
  , ?_assertEqual(8, ponos_statistics:get_response_time_percentile(Stat1, 50))
  , ?_assertEqual(15, ponos_statistics:get_response_time_percentile(Stat1, 99))
  ].


%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
