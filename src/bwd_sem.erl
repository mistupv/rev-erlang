-module(bwd_sem).
-export([eval_step/2,eval_sched/1,eval_opts/1]).

-include("rev_erlang.hrl").

eval_step(#sys{msgs = Msgs, procs = Procs}, Pid) ->
  {Proc, RestProcs} = utils:select_proc(Procs, Pid),
  #proc{pid = Pid, hist = [CurHist|RestHist]} = Proc,
  case CurHist of
    {tau, OldEnv, OldExp} ->
      OldProc = Proc#proc{hist = RestHist, env = OldEnv, exp = OldExp},
      #sys{msgs = Msgs, procs = [OldProc|RestProcs]};
    {self, OldEnv, OldExp} ->
      OldProc = Proc#proc{hist = RestHist, env = OldEnv, exp = OldExp},
      #sys{msgs = Msgs, procs = [OldProc|RestProcs]};
    % Attention: DestPid and MsgValue are not needed to go back 
    {send, OldEnv, OldExp, _DestPid, {_MsgValue, Time}} ->
      {_Msg, RestMsgs} = utils:select_msg(Msgs, Time),
      OldProc = Proc#proc{hist = RestHist, env = OldEnv, exp = OldExp},
      #sys{msgs = RestMsgs, procs = [OldProc|RestProcs]};
    {spawn, OldEnv, OldExp, SpawnPid} ->
      {_SpawnProc, OldRestProcs} = utils:select_proc(RestProcs, SpawnPid),
      OldProc = Proc#proc{hist = RestHist, env = OldEnv, exp = OldExp},
      #sys{msgs = Msgs, procs = [OldProc|OldRestProcs]};
    {rec, OldEnv, OldExp, _OldMsg, OldMail} ->
      OldProc = Proc#proc{hist = RestHist, env = OldEnv, exp = OldExp, mail = OldMail},
      #sys{msgs = Msgs, procs = [OldProc|RestProcs]}
  end.

eval_sched(System) ->
  System.

eval_opts(System) ->
  eval_procs_opts(System).

eval_procs_opts(#sys{procs = []}) ->
  [];
eval_procs_opts(#sys{procs = [CurProc|RestProcs]}) ->
  Hist = CurProc#proc.hist,
  Pid = CurProc#proc.pid,
  case Hist of
    [] ->
      eval_procs_opts(#sys{procs = RestProcs});
    [CurHist|_RestHist] ->
      case CurHist of
        {tau,_,_} -> [#opt{sem = ?MODULE, type = ?TYPE_PROC, id = Pid, rule = ?RULE_SEQ}|eval_procs_opts(#sys{procs = RestProcs})];
        {self,_,_} -> [#opt{sem = ?MODULE,type = ?TYPE_PROC, id = Pid, rule = ?RULE_SELF}|eval_procs_opts(#sys{procs = RestProcs})];
        % TODO: Study these cases
      %   {send,DestPid,_,_} ->
      %     case lists:keyfind(DestPid,#msg.dest,Msgs) of
      %       false -> false;
      %       _Other -> true
      %     end;
        {spawn,_,_,SpawnPid} ->
          {SpawnProc, _RestProcs} = utils:select_proc(RestProcs, SpawnPid),
          #proc{hist = SpawnHist, mail = SpawnMail} = SpawnProc,
          case {SpawnHist, SpawnMail} of
            {[], []} -> [#opt{sem = ?MODULE, type = ?TYPE_PROC, id = Pid, rule = ?RULE_SPAWN}|eval_procs_opts(#sys{procs = RestProcs})];
            _Other -> eval_procs_opts(#sys{procs = RestProcs})
          end;
        % {rec,_,_,_} ->
        _Other -> eval_procs_opts(#sys{procs = RestProcs})
      end
  end.

