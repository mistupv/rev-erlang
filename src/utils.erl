-module(utils).
-export([select_proc/2,select_msg/2,
         list_from_core/1,pp_system/1,
         opt_to_str/1,str_to_opt/1,
         moduleNames/1,
         stringToFunName/1,stringToCoreArgs/1]).

-include("rev_erlang.hrl").

select_proc(Procs,Pid) ->
  [Proc] = [ P || P <- Procs, P#proc.pid == Pid],
  RestProcs = [ P || P <- Procs, P#proc.pid /= Pid],
  {Proc,RestProcs}.

select_msg(Msgs,Time) ->
  [Msg] = [ M || M <- Msgs, M#msg.time == Time],
  RestMsgs = [ M || M <- Msgs, M#msg.time /= Time],
  {Msg,RestMsgs}.

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

pp_msg(#msg{time = Time, src = SrcPid, dest = DestPid, val= MsgValue}) ->
  ["{",
   integer_to_list(Time),",",
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

pp_hist([]) -> "[]";
pp_hist([CurHist|_RestHist]) ->
  case CurHist of
    {tau,_,_} ->
      ["tau(t,e):hs"];
    {self,_,_} ->
      ["self(t,e):hs"];
    {send,Time,DestPid,_,_} ->
      ["send(",
       integer_to_list(Time),",",
       pp(DestPid),",t,e):hs"];
    {spawn,SpawnPid,_,_} ->
      ["spawn(",
       pp(SpawnPid),",t,e):hs"];
    {rec,_,_,_} ->
      ["rec(m,t,e):hs"]
  end.

str_to_opt(Str) ->
  SemStr = [lists:nth(1,Str)],
  TypeStr = [lists:nth(2,Str)],
  IdStr = string:substr(Str,3),
  Semantics =
    case SemStr of
      "f" -> fwd_sem;
      "b" -> bwd_sem
    end,
  {Type,Id} =
    case TypeStr of
      "s" -> {sched,list_to_integer(IdStr)};
      "p" -> {proc,cerl:c_int(list_to_integer(IdStr))}
    end,
  {Semantics,Type,Id}.

opt_to_str({Semantics,Type,Id}) ->
  case Semantics of
    fwd_sem -> "f";
    bwd_sem -> "b"
  end
  ++
  case Type of
    sched -> "s" ++ integer_to_list(Id);
    proc -> "p" ++ pp(Id)
  end.

moduleNames(Forms) ->
  FunDefs = cerl:module_defs(Forms),
  FunNames = [cerl:var_name(Var) || {Var,_Fun} <- FunDefs],
  FunNameStrings = [funNameToString({Name,Arity}) || {Name,Arity} <- FunNames, Name =/= 'module_info'],
  FunNameStrings.

funNameToString({Name,Arity}) ->
  atom_to_list(Name) ++ "/" ++ integer_to_list(Arity).

stringToFunName(String) ->
  FunParts = string:tokens(String, "/"),
  Name = list_to_atom(lists:nth(1,FunParts)),
  Arity = list_to_integer(lists:nth(2,FunParts)),
  {Name,Arity}.

stringToCoreArgs(Text) ->
  TextDot = Text ++ ".",
  {ok, String, _} = erl_scan:string(TextDot),
  {ok, Exprs} = erl_parse:parse_exprs(String),
  EvalExprs = [element(2,erl_eval:expr(Expr,[])) || Expr <- Exprs],
  [cerl:abstract(Expr) || Expr <- EvalExprs].

