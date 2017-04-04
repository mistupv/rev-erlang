-module(bwd_sem).
-export([eval_step/2,eval_sched/1,can_eval/2]).

% TODO: Put defines in some module?
-define(ID_GAMMA,0).

eval_step({Gamma,Procs},Pid) ->
  {Proc,RestProcs} = utils:select_proc(Procs,Pid),
  {Pid,[CurHist|RestHist],_,Mail} = Proc,
  case CurHist of
    {tau,OldEnv,OldExp} ->
      OldProc = {Pid,RestHist,{OldEnv,OldExp},Mail},
      {Gamma,[OldProc|RestProcs]};
    {self,OldEnv,OldExp} ->
      OldProc = {Pid,RestHist,{OldEnv,OldExp},Mail},
      {Gamma,[OldProc|RestProcs]};
    % % TODO: The following cases have not been tested
    % {send,_,_,_} -> false;
    {spawn,SpawnPid,OldEnv,OldExp} ->
      {_SpawnProc,OldRestProcs} = utils:select_proc(RestProcs,SpawnPid),
      OldProc = {Pid,RestHist,{OldEnv,OldExp},Mail},
      {Gamma,[OldProc|OldRestProcs]}
    % {rec,_,_,_} -> false
  end.

eval_sched(System) ->
  System.

can_eval({[],_Procs},?ID_GAMMA) -> false;
can_eval({[{SrcPid,DestPid,_MsgValue}|RestMsgs],Procs},?ID_GAMMA) ->
  SrcProcs = [Item || Item = {P,_,_,_} <- Procs, P == SrcPid],
  case SrcProcs of
    [] -> can_eval({RestMsgs,Procs},?ID_GAMMA);
    [{_P,[{send,DestPid,_E,_M}|_Hist]}] -> true
  end;
can_eval({_Gamma,Procs},Pid) ->
  %io:fwrite("Chosen Pid: ~p~n",[Pid]),
  {Proc,_RestProcs} = utils:select_proc(Procs,Pid),
  {Pid,Hist,{_Env,_Exp},_Mail} = Proc,
  case Hist of
    [] -> false;
    [CurHist|_RestHist] ->
      case CurHist of
        {tau,_,_} -> true;
        {self,_,_} -> true;
        % TODO: Study these cases
        {send,_,_,_} -> false;
        {spawn,_,_,_} -> false;
        {rec,_,_,_} -> false
      end
  end.
