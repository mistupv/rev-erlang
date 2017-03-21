-module(bwd_sem).
-export([eval_step/2,eval_sched/1]).

eval_step({Gamma,Procs},_Pid) ->
  {Gamma,Procs}.

eval_sched(System) ->
  System.