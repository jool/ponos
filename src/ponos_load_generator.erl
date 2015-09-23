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
%%% @copyright 2014, Klarna AB
%%% @author Jonathan Olsson <jonathan@klarna.com>
%%% @end

%%%_* Module Declaration ===============================================
%% @private
-module(ponos_load_generator).
-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").

%%%_* Exports ==========================================================
%% API
-export([ get_duration/1
        , get_load_spec/1
        , get_modeled_load/1
        , get_name/1
        , get_running_tasks/1
        , get_start/1
        , get_statistics/1
        , get_task/1
        , is_running/1
        , pause/1
        , start/1
        , start_link/1
        , stop/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%% TODO: Should we terminate with a timer?
%% TODO: Some unit tests are missing
%%%_* Records and Definitions ==========================================
-record(state, {
          duration          :: ponos:duration(),
          is_running        :: boolean(),
          load_spec         :: ponos:load_spec(),
          max_concurrent    :: integer(),
          limit_reported    :: boolean(),
          name              :: ponos:name(),
          next_trigger_time :: number(),
          intensity         :: ponos:intensity(),
          running_tasks     :: integer(),
          statistics        :: ponos_statistics:ponos_statistics(),
          task_runner       :: module(),
          task              :: ponos:task(),
          tick_counter      :: integer()
         }).

-export_type([ load_generator/0
             ]).

-define(PRUNE_INTENSITY_INTERVAL, 2000).

%%%_* Code =============================================================
%%%_* Types ------------------------------------------------------------
-type load_generator() :: pid().

%%%_* External API -----------------------------------------------------
get_duration(LoadGenerator) ->
  gen_server:call(LoadGenerator, get_duration).

get_load_spec(LoadGenerator) ->
  gen_server:call(LoadGenerator, get_load_spec).

get_name(LoadGenerator) ->
  gen_server:call(LoadGenerator, get_name).

get_running_tasks(LoadGenerator) ->
  gen_server:call(LoadGenerator, get_running_tasks).

get_modeled_load(LoadGenerator) ->
  gen_server:call(LoadGenerator, get_modeled_load).

get_start(LoadGenerator) ->
  gen_server:call(LoadGenerator, get_start).

get_statistics(LoadGenerator) ->
  gen_server:call(LoadGenerator, get_statistics).

get_task(LoadGenerator) ->
  gen_server:call(LoadGenerator, get_task).

is_running(LoadGenerator) ->
  gen_server:call(LoadGenerator, is_running).

pause(LoadGenerator) ->
  gen_server:call(LoadGenerator, pause).

start(LoadGenerator) ->
  ok = gen_server:call(LoadGenerator, start),
  LoadGenerator.

start_link(Args) ->
  gen_server:start_link(?MODULE, Args, []).

stop(LoadGenerator) ->
  gen_server:call(LoadGenerator, stop).

%%%_* gen_server callbacks ---------------------------------------------
init(Args) ->
  process_flag(trap_exit, true),
  Name          = element(2, proplists:lookup(name, Args)),
  TaskRunner    = element(2, proplists:lookup(task_runner, Args)),
  RunnerArgs    = element(2, proplists:lookup(task_runner_args, Args)),
  MaxConcurrent = element(2, proplists:lookup(max_concurrent, Args)),
  Duration      = element(2, proplists:lookup(duration, Args)),
  LoadSpec      = element(2, proplists:lookup(load_spec, Args)),

  {ok, State} = ponos_task_runner_callbacks:init(TaskRunner, Name, RunnerArgs),
  {ok, #state{
          duration          = Duration,
          load_spec         = LoadSpec,
          is_running        = false,
          max_concurrent    = MaxConcurrent,
          limit_reported    = false,
          name              = Name,
          next_trigger_time = 0,
          running_tasks     = 0,
          statistics        = ponos_statistics:new(),
          task_runner       = {TaskRunner, State},
          task              = element(2, proplists:lookup(task, Args)),
          tick_counter      = 0
         }}.

handle_call(get_duration, _From, State) ->
  {reply, state_get_duration(State), State};
handle_call(get_load_spec, _From, State) ->
  {reply, state_get_load_spec(State), State};
handle_call(get_name, _From, State) ->
  {reply, state_get_name(State), State};
handle_call(get_modeled_load, _From, State) ->
  {reply, calc_top_modeled_load(State), State};
handle_call(get_running_tasks, _From, State) ->
  {reply, state_get_running_tasks(State), State};
handle_call(get_start, _From, State) ->
  {reply, state_get_start(State), State};
handle_call(get_statistics, _From, State) ->
  {reply, state_get_statistics(State), State};
handle_call(get_task, _From, State) ->
  {reply, state_get_task(State), State};
handle_call(is_running, _From, State) ->
  {reply, state_get_is_running(State), State};
handle_call(pause, _From, State) ->
  {reply, ok, dispatch_pause(State)};
handle_call(stop, _From, State) ->
  {stop, {shutdown, removed}, ok, State};
handle_call(start, _From, State) ->
  NewState = dispatch_start(State),
  {reply, ok, NewState}.

handle_cast({new_response_time, ResponseTime}, State) ->
  S = ponos_statistics:add_response_time(ResponseTime, State#state.statistics),
  {noreply, State#state{statistics = S}};
handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({'EXIT', _Pid, _Reason}, State) ->
  {noreply, state_dec_running_tasks(State)};
handle_info(tick, State) ->
  Start = state_get_start(State),
  TimePassed = time_passed_in_ms(os:timestamp(), Start),
  case status(TimePassed, State) of
    skip ->
      tick(),
      skip(update_intensity(TimePassed, State));
    trigger_task ->
      tick_no_delay(),
      NewState = dispatch_trigger_task(State),
      {noreply, NewState};
    duration_exceeded ->
      {stop, {shutdown, duration_exceeded}, State};
    max_concurrency_reached ->
      tick(),
      max_concurrency(State);
    paused ->
      tick(),
      skip(State)
  end;
handle_info(_Info, State) ->
  {noreply, State}.

skip(State) ->
  {noreply, state_inc_tick_counter(State)}.

max_concurrency(State) ->
  case state_get_limit_reported(State) of
    true ->
      skip(State);
    false ->
      NewState = dispatch_concurrency_limit(State),
      skip(NewState)
  end.

status(TimePassed, State) ->
  Duration              = state_get_duration(State),
  DurationIsExceeded    = duration_is_exceeded(Duration, TimePassed),
  MaxConcurrencyReached = is_max_concurrency_reached(State),
  IsPaused              = not state_get_is_running(State),
  ShouldTriggerTask     = get_next_action(TimePassed, State) == trigger_task,

  %% Please be aware that the ordering of conditions is
  %% important. DurationIsExceeded must come first as the load_generator
  %% must stop if its in this status. Likewise, MaxConcurrencyReached
  %% and IsPaused must override ShouldTriggerTask.
  if
    DurationIsExceeded    -> duration_exceeded;
    MaxConcurrencyReached -> max_concurrency_reached;
    IsPaused              -> paused;
    ShouldTriggerTask     -> trigger_task;
    true                  -> skip
  end.

terminate(shutdown, State) ->
  dispatch_terminate(shutdown, State);
terminate({shutdown, duration_exceeded} = Reason, State) ->
  dispatch_terminate(Reason, State);
terminate({shutdown, removed} = Reason, State) ->
  dispatch_terminate(Reason, State).

%% @private
tick() ->
  erlang:send_after(1, self(), tick).

%% @private
tick_no_delay() ->
  self() ! tick.

code_change(_OldVsn, LoadGenerator, _Extra) ->
  {ok, LoadGenerator}.

%%%_* gen_server dispatch ----------------------------------------------
dispatch_trigger_task(State) ->
  _NewState = trigger_task_and_update_counters(State).

dispatch_concurrency_limit(State) ->
  {TaskRunner, RunnerState} = state_get_task_runner(State),
  Name                      = state_get_name(State),
  ponos_task_runner_callbacks:concurrency_limit(TaskRunner, Name, RunnerState),
  state_set_limit_reported(State).

dispatch_pause(State) ->
  {TaskRunner, S} = state_get_task_runner(State),
  ponos_task_runner_callbacks:pause(TaskRunner, state_get_name(State), S),
  state_set_is_running(State, false).

dispatch_start(State) ->
  {TaskRunner, RunnerState} = state_get_task_runner(State),
  Name                      = state_get_name(State),
  {ok, RunnerState1} =
    ponos_task_runner_callbacks:start(TaskRunner, Name, RunnerState),
  NewState = init_load_generator(TaskRunner, RunnerState1, State),
  tick(),
  NewState.

dispatch_terminate(_Reason, State) ->
  {TaskRunner, S} = state_get_task_runner(State),
  Name            = state_get_name(State),
  ponos_task_runner_callbacks:terminate(TaskRunner, Name, S),
  ok.

%%%_* Internal ---------------------------------------------------------
init_load_generator(TaskRunner, TaskRunnerState, State0) ->
  State      = state_reset_start(State0),

  Start      = state_get_start(State),
  TimePassed = time_passed_in_ms(Start, Start),
  LoadSpec   = state_get_load_spec(State),
  Intensity  = LoadSpec(trunc(TimePassed)),
  State#state{
    intensity   = Intensity,
    is_running  = true,
    task_runner = {TaskRunner, TaskRunnerState},
    next_trigger_time = TimePassed + intensity_ms(Intensity)
   }.

duration_is_exceeded(Duration, TimePassed) ->
  (Duration =/= infinity) andalso (TimePassed >= Duration).

is_max_concurrency_reached(State) ->
  MaxConcurrent = state_get_max_concurrent(State),
  NoConcurrent  = state_get_running_tasks(State),
  MaxConcurrent /= 0 andalso NoConcurrent >= MaxConcurrent.

get_next_action(TimePassed, State) ->
  TriggerTime = state_get_next_trigger_time(State),
  Intensity   = state_get_intensity(State),
  case should_trigger_task(TimePassed, TriggerTime, Intensity) of
    true  -> trigger_task;
    false -> skip
  end.

should_trigger_task(_TimePased, _TriggerTime, Intensity) when Intensity == 0 ->
  false;
should_trigger_task(TimePassed, TriggerTime, _Intensity) ->
  TimePassed >= TriggerTime.

trigger_task_and_update_counters(State) ->
  run_task(State),
  NewState1 = state_inc_tick_counter(State),
  TriggerTime = state_get_next_trigger_time(NewState1),
  NewState2 = update_intensity(TriggerTime, NewState1),
  NewState3 = update_counters(NewState2),
  NewState4 = update_next_trigger_time(NewState3),
  state_clear_limit_reported(NewState4).

run_task(State) ->
  Self = self(),
  spawn_link(fun() -> run_task(State, state_get_task(State), Self) end).

run_task(State, Task, Self) ->
  {TaskRunner, RunnerState} = state_get_task_runner(State),
  Name                      = state_get_name(State),
  Before                    = os:timestamp(),
  ponos_task_runner_callbacks:call(TaskRunner, Name, Task, RunnerState),
  After                     = os:timestamp(),
  ResponseTime              = round(timer:now_diff(After, Before) / 1000),
  gen_server:cast(Self, {new_response_time, ResponseTime}).

update_counters(State) ->
  state_inc_running_tasks(state_inc_call_counter(State)).

update_next_trigger_time(State) ->
  Freq        = freq(state_get_intensity(State)),
  TriggerTime = state_get_next_trigger_time(State),
  state_set_next_trigger_time(State, TriggerTime + Freq).

update_intensity(TimePassed, State) ->
  OldIntensity = state_get_intensity(State),
  LoadSpec     = state_get_load_spec(State),
  Intensity    = LoadSpec(trunc(TimePassed)),

  TriggerTime =
    case intensity_changed(Intensity, OldIntensity) of
      false ->
        state_get_next_trigger_time(State);
      true when OldIntensity == 0 ->
        %% Special case: the old trigger time is not relevant, because
        %% for a 0 intensity we could not calculate a trigger time, so
        %% it still holds the last task's trigger time.
        %%
        %% When scheduling the next task only the current time is
        %% relevant, not the time when the last task was executed.
        TimePassed + freq(Intensity);
      true ->
        new_trigger_time(OldIntensity, Intensity, State)
    end,
  state_set_next_trigger_time( state_set_intensity(State, Intensity)
                             , TriggerTime).

new_trigger_time(OldIntensity, CurrentIntensity, State) ->
  OldTrigger = state_get_next_trigger_time(State),
  do_new_trigger_time(OldIntensity, CurrentIntensity, OldTrigger).

do_new_trigger_time(OldIntensity, CurrentIntensity, OldTriggerTime) ->
  Freq    = freq(CurrentIntensity),
  OldFreq = freq(OldIntensity),
  case freq_increased(OldFreq, Freq) of
    false -> OldTriggerTime - (OldFreq - Freq);
    true  -> OldTriggerTime + abs(OldFreq - Freq)
  end.

freq_increased(OldFreq, Freq) ->
  Freq > OldFreq.

intensity_changed(Current, Previous) ->
  Current =/= Previous.

freq(Intensity) when Intensity == 0 ->
  0.0;
freq(Intensity) ->
  1 / intensity_ms(Intensity).

%% calc_current_load([]) -> 0.0;
%% calc_current_load(Intensities) ->
%%   Period = time_passed_in_ms(os:timestamp(), lists:last(Intensities)),
%%   do_calc_current_load(Intensities, Period).

%% do_calc_current_load(_Intensities, 0)     -> 0.0;
%% do_calc_current_load(Intensities, Period) ->
%%   _CurrentLoad = length(Intensities) / Period * 1000.0.

calc_top_modeled_load(State) ->
  Start    = state_get_start(State),
  LoadSpec = state_get_load_spec(State),
  case Start of
    undefined ->
      0.0;
    Start ->
      LoadSpec(trunc(time_passed_in_ms(os:timestamp(), Start)))
  end.

time_passed_in_ms(Now, Then) ->
  timer:now_diff(Now, Then) / 1000.

intensity_ms(Seconds) ->
  Seconds / 1000.

%% State accessors -----------------------------------------------------
state_get_duration(S)          -> S#state.duration.
state_get_intensity(S)         -> S#state.intensity.
state_get_limit_reported(S)    -> S#state.limit_reported.
state_get_is_running(S)        -> S#state.is_running.
state_get_load_spec(S)         -> S#state.load_spec.
state_get_max_concurrent(S)    -> S#state.max_concurrent.
state_get_name(S)              -> S#state.name.
state_get_next_trigger_time(S) -> S#state.next_trigger_time.
state_get_running_tasks(S)     -> S#state.running_tasks.
state_get_task(S)              -> S#state.task.
state_get_task_runner(S)       -> S#state.task_runner.
state_get_start(S)            -> ponos_statistics:get_start(S#state.statistics).
state_get_statistics(S)        -> S#state.statistics.

state_reset_start(S) ->
  S#state{statistics = ponos_statistics:reset_start(S#state.statistics)}.

state_inc_call_counter(S) ->
  S#state{
    statistics = ponos_statistics:increase_call_counter(S#state.statistics)
   }.

state_inc_running_tasks(S) ->
  S#state{running_tasks = S#state.running_tasks + 1}.

state_dec_running_tasks(S) ->
  S#state{running_tasks = S#state.running_tasks - 1}.

state_inc_tick_counter(S) ->
  S#state{tick_counter = S#state.tick_counter + 1}.

state_set_next_trigger_time(State, TriggerTime) ->
  State#state{next_trigger_time = TriggerTime}.

state_set_intensity(State, Intensity) when is_float(Intensity) ->
  State#state{intensity = Intensity}.

state_set_is_running(State, IsRunning) when is_boolean(IsRunning) ->
  State#state{is_running = IsRunning}.

state_set_limit_reported(State) ->
  State#state{limit_reported = true}.

state_clear_limit_reported(State) ->
  State#state{limit_reported = false}.

%%%_* EUnit Tests ======================================================
-ifdef(TEST).

%% calc_current_load_test_() ->
%%   [ ?_assertEqual(0.0, calc_current_load([]))
%%   , ?_assertEqual(0.2, do_calc_current_load([1, 2], 10000))
%%   , ?_assertEqual(0.0, do_calc_current_load([1], 0))
%%   ].

duration_is_exceeded_test_() ->
  [ ?_assertEqual(false, duration_is_exceeded(infinity, 10))
  , ?_assertEqual(false, duration_is_exceeded(100, 99))
  , ?_assertEqual(true, duration_is_exceeded(100, 100))
  , ?_assertEqual(true, duration_is_exceeded(100, 101))
  ].

should_trigger_task_test_() ->
  [ ?_assertEqual(false, should_trigger_task(0, 0, 0))
  , ?_assertEqual(false, should_trigger_task(1, 2, 0))
  , ?_assertEqual(false, should_trigger_task(2, 1, 0))
  , ?_assertEqual(false, should_trigger_task(999, 1000, 1))
  , ?_assertEqual(true, should_trigger_task(1000, 1000, 1))
  , ?_assertEqual(true, should_trigger_task(1001, 1000, 1))
  ].

get_next_action_test_() ->
  OneCps = 1.0,
  ZeroCps = 0.0,
  State = mk_state(OneCps, 1000),
  [ ?_assertEqual(skip, get_next_action(999, State))
  , ?_assertEqual(trigger_task, get_next_action(1000, State))
  , ?_assertEqual(trigger_task, get_next_action(1001, State))
  , ?_assertEqual(skip, get_next_action(1000, mk_state(ZeroCps, 999)))
  , ?_assertEqual(skip, get_next_action(1000, mk_state(ZeroCps, 1000)))
  , ?_assertEqual(skip, get_next_action(1000, mk_state(ZeroCps, 1001)))
  ].

duration_test() ->
  process_flag(trap_exit, true),
  start_a_load_gen(),
  Reason = {shutdown, duration_exceeded},
  receive
    Exit -> ?assertMatch({'EXIT', _, Reason}, Exit)
  end.

start_a_load_gen() ->
  Args = [ {name, test_duration}
         , {task, fun() -> ok end}
         , {load_spec, ponos_load_specs:make_constant(1.0)}
         , {max_concurrent, 0}
         , {duration, 10}
         , {task_runner, ponos_default_task_runner}
         , {task_runner_args, []}
         ],
  {ok, LoadGen} = start_link(Args),
  start(LoadGen).

do_new_trigger_time_test_() ->
  [ ?_assertEqual(50.0, do_new_trigger_time(10.0, 20.0, 100))
  , ?_assertEqual(150.0, do_new_trigger_time(20.0, 10.0, 100))
  , ?_assertEqual(150.0, do_new_trigger_time(0.0, 20.0, 100))
  , ?_assertEqual(150.0, begin
                           LoadSpec = fun(_) -> 10.0 end,
                           S = mk_state(20.0, 100, LoadSpec),
                           get_next_trigger_time(update_intensity(100, S))
                         end)
  , ?_assertEqual(50.0, begin
                          LoadSpec = fun(_) -> 20.0 end,
                          S = mk_state(10.0, 100, LoadSpec),
                          get_next_trigger_time(update_intensity(100, S))
                        end)
  ].

status_duration_exceeded_test() ->
  State = #state{duration = 100, is_running = false},
  ?assertEqual(duration_exceeded, status(100, State)).

status_max_concurrency_reached_test_() ->
  State = #state{duration = 100, is_running = false, running_tasks = 10},
  [ ?_assertEqual( max_concurrency_reached
                 , status(1, State#state{max_concurrent = 10}))
  , ?_assertNotEqual( max_concurrency_reached
                    , status(1, State#state{max_concurrent = 0}))
  ].

status_paused_test_() ->
  State = #state{ duration = 100
                , is_running = true
                , max_concurrent = 0
                , running_tasks= 10},
  [ ?_assertNotEqual(paused, status(99, State))
  , ?_assertEqual(paused, status(99, State#state{is_running = false}))
  ].

status_trigger_task_test_() ->
  State = #state{ duration = infinity
                , is_running = true
                , max_concurrent = 0
                , load_spec = fun(_) -> 500.0 end
                , next_trigger_time = 10
                , intensity = 500.0
                },
  [ ?_assertEqual(trigger_task, status(10, State))
  , ?_assertEqual(skip, status(9, State))
  ].

get_next_trigger_time(#state{next_trigger_time = NTT}) ->
  NTT.

mk_state(Intensity, TriggerTime) ->
  #state{
     intensity = Intensity,
     next_trigger_time = TriggerTime
    }.

mk_state(Intensity, TriggerTime, LoadSpec) ->
  (mk_state(Intensity, TriggerTime))#state{load_spec = LoadSpec}.

-endif.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
