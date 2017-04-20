-module(bwd_sem).
-export([eval_step/2,eval_sched/1,eval_opts/1]).

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
        {tau,_,_} -> [{?MODULE,proc,Pid}|eval_procs_opts(#sys{procs = RestProcs})];
        {self,_,_} -> [{?MODULE,proc,Pid}|eval_procs_opts(#sys{procs = RestProcs})];
        % TODO: Study these cases
      %   {send,DestPid,_,_} ->
      %     case lists:keyfind(DestPid,#msg.dest,Msgs) of
      %       false -> false;
      %       _Other -> true
      %     end;
      % % it is safe to assume that the spawned process is alive
      %   {spawn,_,_,_} -> true;
      %   {rec,_,_,_} -> false
        _Other -> eval_procs_opts(#sys{procs = RestProcs})
      end
  end.

% can_eval(#sys{msgs = []},?ID_GAMMA) ->
%   false;
% can_eval(#sys{msgs = [#msg{src = SrcPid,dest = DestPid}|RestMsgs], procs = Procs},?ID_GAMMA) ->
%   SrcProcs = [Proc || Proc <- Procs, Proc#proc.pid == SrcPid],
%   case SrcProcs of
%     [] -> can_eval({RestMsgs,Procs},?ID_GAMMA);
%     [#proc{hist = [{send,DestPid,_E,_M}|_Hist]}] -> true
%   end;
% can_eval(#sys{msgs = Msgs, procs = Procs},Pid) ->
%   %io:fwrite("Chosen Pid: ~p~n",[Pid]),
%   {Proc,_RestProcs} = utils:select_proc(Procs,Pid),
%   #proc{pid = Pid, hist = Hist} = Proc,
%   case Hist of
%     [] -> false;
%     [CurHist|_RestHist] ->
%       case CurHist of
%         {tau,_,_} -> true;
%         {self,_,_} -> true;
%         % TODO: Study these cases
%         {send,DestPid,_,_} ->
%           case lists:keyfind(DestPid,#msg.dest,Msgs) of
%             false -> false;
%             _Other -> true
%           end;
%         % it is safe to assume that the spawned process is alive
%         {spawn,_,_,_} -> true;
%         {rec,_,_,_} -> false
%       end
%   end.
