-module(bwd_sem).
-export([eval_step/2,eval_sched/1,can_eval/2]).

-include("rev_erlang.hrl").

eval_step(#sys{msgs = Msgs, procs = Procs},Pid) ->
  {Proc,RestProcs} = utils:select_proc(Procs,Pid),
  #proc{pid = Pid, hist = [CurHist|RestHist]} = Proc,
  case CurHist of
    {tau,OldEnv,OldExp} ->
      OldProc = Proc#proc{hist = RestHist, env = OldEnv, exp = OldExp},
      #sys{msgs = Msgs, procs = [OldProc|RestProcs]};
    {self,OldEnv,OldExp} ->
      OldProc = Proc#proc{hist = RestHist, env = OldEnv, exp = OldExp},
      #sys{msgs = Msgs, procs = [OldProc|RestProcs]};
    % % TODO: The following cases have not been tested
    %{send,DestPid,OldEnv,OldExp} -> ;
    {spawn,SpawnPid,OldEnv,OldExp} ->
      {_SpawnProc,OldRestProcs} = utils:select_proc(RestProcs,SpawnPid),
      OldProc = Proc#proc{hist = RestHist, env = OldEnv, exp = OldExp},
      #sys{msgs = Msgs, procs = [OldProc|OldRestProcs]}
    % {rec,_,_,_} -> false
  end.

eval_sched(System) ->
  System.

can_eval(#sys{msgs = []},?ID_GAMMA) ->
  false;
can_eval(#sys{msgs = [#msg{src = SrcPid,dest = DestPid}|RestMsgs], procs = Procs},?ID_GAMMA) ->
  SrcProcs = [Proc || Proc <- Procs, Proc#proc.pid == SrcPid],
  case SrcProcs of
    [] -> can_eval({RestMsgs,Procs},?ID_GAMMA);
    [#proc{hist = [{send,DestPid,_E,_M}|_Hist]}] -> true
  end;
can_eval(#sys{msgs = Msgs, procs = Procs},Pid) ->
  %io:fwrite("Chosen Pid: ~p~n",[Pid]),
  {Proc,_RestProcs} = utils:select_proc(Procs,Pid),
  #proc{pid = Pid, hist = Hist} = Proc,
  case Hist of
    [] -> false;
    [CurHist|_RestHist] ->
      case CurHist of
        {tau,_,_} -> true;
        {self,_,_} -> true;
        % TODO: Study these cases
        {send,DestPid,_,_} ->
          case lists:keyfind(DestPid,#msg.dest,Msgs) of
            false -> false;
            _Other -> true
          end;
        % it is safe to assume that the spawned process is alive
        {spawn,_,_,_} -> true;
        {rec,_,_,_} -> false
      end
  end.
