%%%-------------------------------------------------------------------
%%% @doc The main module for the rev-erlang project.
%%% This module includes functions for starting the application
%%% and interact with the reversible semantics for Erlang
%%% @end
%%%-------------------------------------------------------------------

-module(rev_erlang).
-export([start/0,
         start_refs/1, stop_refs/0,
         eval_opts/1, eval_step/2, eval_mult/3, eval_norm/1]).

-include("rev_erlang.hrl").

%%--------------------------------------------------------------------
%% @doc Starts the GUI
%% @end
%%--------------------------------------------------------------------
start() ->
  rev_erlang_gui:setup_gui(),
  ok.

%%--------------------------------------------------------------------
%% @doc Starts the ETS servers and initializes them
%% @end
%%--------------------------------------------------------------------
start_refs(FunDefs) ->
  ?LOG("starting refs"),
  ref_start(),
  ref_add(?FUN_DEFS,   FunDefs),
  ref_add(?FRESH_PID,  2),
  ref_add(?FRESH_TIME, 1),
  ref_add(?FRESH_VAR,  1).

%%--------------------------------------------------------------------
%% @doc Stops the ETS servers
%% @end
%%--------------------------------------------------------------------
stop_refs() ->
  ?LOG("stopping refs"),
  ref_stop().


%%--------------------------------------------------------------------
%% @doc Returns all the evaluation options for a given System
%% @end
%%--------------------------------------------------------------------
eval_opts(System) ->  
  FwdOpts = fwd_sem:eval_opts(System),
  BwdOpts = bwd_sem:eval_opts(System),
  FwdOpts ++ BwdOpts.

eval_step(System, Option) ->
  #opt{sem = Semantics, type = Type, id = Id} = Option,
  NewSystem =
    case Type of
      ?TYPE_MSG -> Semantics:eval_sched(System,Id);
      ?TYPE_PROC -> Semantics:eval_step(System,cerl:c_int(Id))
    end,
  NewSystem.

%%--------------------------------------------------------------------
%% @doc Performs Steps evaluation steps in System in
%% the Option direction
%% @end
%%--------------------------------------------------------------------
eval_mult(System, Option, Steps) ->
  eval_mult_1(System, Option, Steps, 0).

eval_mult_1(System, _Option, Steps, Steps) ->
  {System, Steps};
eval_mult_1(System, Option, Steps, StepsDone) ->
  Sem = 
    case Option of
      ?MULT_FWD -> fwd_sem;
      ?MULT_BWD -> bwd_sem
    end,  
  Opts = Sem:eval_opts(System),
  case Opts of
    [] ->
      {System, StepsDone};
    _Other ->
      RandIdx = rand:uniform(length(Opts)),
      RandOpt = lists:nth(RandIdx, Opts),
      NewSystem = eval_step(System, RandOpt),
      eval_mult_1(NewSystem, Option, Steps, StepsDone + 1)
  end.

%%--------------------------------------------------------------------
%% @doc Performs evaluation steps (except for sched steps) in System
%% until the system becomes "normalized" (more info on the paper)
%% @end
%%--------------------------------------------------------------------
eval_norm(System) ->
  eval_norm_1(System, 0).

eval_norm_1(System, Steps) ->
  Opts = fwd_sem:eval_procs_opts(System),
  case Opts of
    [] ->
      {System, Steps};
    _Other ->
      RandIdx = rand:uniform(length(Opts)),
      RandOpt = lists:nth(RandIdx, Opts),
      NewSystem = eval_step(System, RandOpt),
      eval_norm_1(NewSystem, Steps + 1)
  end.

ref_add(Id, Ref) ->
    ets:insert(?APP_REF, {Id, Ref}).

ref_start() ->
    ?APP_REF = ets:new(?APP_REF, [set, public, named_table]),
    ok.

ref_stop() ->
    ets:delete(?APP_REF).

% eval(System) ->
%   FwdOpts = fwd_sem:eval_opts(System),
%   BwdOpts = bwd_sem:eval_opts(System),
%   AllOpts = FwdOpts ++ BwdOpts,
%   StrOpts = string:join([utils:opt_to_str(Opt) || Opt <- AllOpts]," "),
%   io:fwrite("Available rules: ~s~n",[StrOpts]),
%   StrOpt =
%     case io:fread("Select rule: ","~s") of
%       {ok,[ReadStr]} -> ReadStr;
%       _ -> error
%     end,
%   {Semantics,Type,Id} = utils:str_to_opt(StrOpt),
%   NewSystem =
%     case Type of
%       sched -> Semantics:eval_sched(System,Id);
%       proc -> Semantics:eval_step(System,Id)
%     end,
%   io:fwrite("~s~n",[utils:pp_system(NewSystem)]),
%   eval(NewSystem).
