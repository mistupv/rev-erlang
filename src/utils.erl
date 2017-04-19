-module(utils).
-export([select_proc/2,list_from_core/1,pp_system/1]).

-include("rev_erlang.hrl").

select_proc(Procs,Pid) ->
  [Proc] = [ P || P <- Procs, P#proc.pid == Pid],
  RestProcs = [ P || P <- Procs, P#proc.pid, P /= Pid],
  {Proc,RestProcs}.

list_from_core(Exp) ->
  case cerl:type(Exp) of
    cons ->
      [cerl:cons_hd(Exp)|list_from_core(cerl:cons_tl(Exp))];
    literal -> [] % Exp == cerl:c_nil()
  end.

pp_system(#sys{msgs = Msgs, procs = Procs}) ->
  [pp_msgs(Msgs),
  ";\n",
  pp_procs(Procs)].

pp_msgs([]) -> "[]";
pp_msgs(Msgs) ->
  MsgsList = [pp_msg(Msg) || Msg <- Msgs],
  ["[",
   string:join(MsgsList,","),
   "]"].

pp_procs(Procs) ->
  ProcsList = [pp_proc(Proc) || Proc <- Procs],
  string:join(ProcsList," &\n").

pp_msg(#msg{src = SrcPid, dest = DestPid, val= MsgValue}) ->
  ["{",
   pp(SrcPid),",",
   pp(DestPid),",",
   pp(MsgValue),
   "}"].

pp_proc(#proc{pid=Pid, hist = Hist, env = Env, exp = Exp, mail = Mail}) ->
  ["{",
   pp(Pid),",",
   pp_hist(Hist),",",
   pp_env(Env),",\n",
   pp(Exp),",\n",
   pp_msgs(Mail),
   "}"].

pp(CoreForm) -> core_pp:format(CoreForm).

pp_env([]) -> "[]";
pp_env(Env) ->
  PairsList = [pp_pair(Var,Val) || {Var,Val} <- Env],
  ["[",
   string:join(PairsList,","),
   "]"].

pp_pair(Var,Val) ->
  ["{",pp(Var)," -> ",pp(Val),"}"].

% TODO: Improve non-empty list case
pp_hist([]) -> "[]";
pp_hist(_Hist) -> "h:hs".
