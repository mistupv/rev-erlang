-module(utils).
-export([select_proc/2,select_msg/2,
         list_from_core/1,
         update_env/2, merge_env/2,
         replace/3, pp_system/1,
         % opt_to_str/1,str_to_opt/1,
         moduleNames/1,
         stringToFunName/1,stringToCoreArgs/1,
         filter_options/2,has_fwd/1,has_bwd/1]).

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

update_env({Key, Value}, Env) ->
  DelEnv = proplists:delete(Key, Env),
  DelEnv ++ [{Key, Value}].

merge_env(Env, []) -> Env;
merge_env(Env, [CurBind|RestBind]) ->
  NewEnv = update_env(CurBind, Env),
  merge_env(NewEnv, RestBind).

% replace VarName by SubExp in SuperExp
replace(Var, SubExp, SuperExp) ->
  VarName = cerl:var_name(Var),
  cerl_trees:map(
    fun (Exp) ->
      case cerl:type(Exp) of
        var ->
          case cerl:var_name(Exp) of
            VarName -> SubExp;
            _Other -> Exp
          end;
        _Other -> Exp
      end
    end, SuperExp).

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

pp_msg(#msg{dest = DestPid, val = MsgValue, time = Time}) ->
  ["(",
   pp(DestPid),",{",
   pp(MsgValue),",",
   integer_to_list(Time),
   "})"].

pp_proc(#proc{pid = Pid, hist = Hist, env = Env, exp = Exp, mail = Mail}) ->
  ["{",
   pp(Pid),",",
   pp_hist(Hist),",",
   pp_env(Env),",\n",
   pp(Exp),",\n",
   pp_mail(Mail),
   "}"].

pp(CoreForm) -> core_pp:format(CoreForm).

pp_env([]) -> "{}";
pp_env(Env) ->
  PairsList = [pp_pair(Var,Val) || {Var,Val} <- Env],
  ["{",
   string:join(PairsList,","),
   "}"].

pp_pair(Var,Val) ->
  [pp(Var)," -> ",pp(Val)].

pp_hist([]) -> "[]";
pp_hist([CurHist|_RestHist]) ->
  case CurHist of
    {tau,_,_} ->
      ["tau(t,e):hs"];
    {self,_,_} ->
      ["self(t,e):hs"];
    {send,_,_,DestPid,{Value,Time}} ->
      ["send(t,e,",
       pp(DestPid),",{",
       pp(Value),",",
       integer_to_list(Time),
       "}):hs"];
    {spawn,_,_,SpawnPid} ->
      ["spawn(t,e,",
       pp(SpawnPid),
       "):hs"];
    {rec,_,_,{Value,Time},_} ->
      ["rec(t,e,{",
       pp(Value),",",
       integer_to_list(Time),
       "},q):hs"]
  end.

pp_mail([]) -> "[]";
pp_mail(Mail) ->
  MailList = [pp_msg_mail(Val, Time) || {Val, Time} <- Mail],
  ["[",
   string:join(MailList,","),
   "]"].

pp_msg_mail(Val, Time) ->
  ["{",pp(Val),",",
   integer_to_list(Time),"}"].

% str_to_opt(Str) ->
%   SemStr = [lists:nth(1,Str)],
%   TypeStr = [lists:nth(2,Str)],
%   IdStr = string:substr(Str,3),
%   Semantics =
%     case SemStr of
%       "f" -> fwd_sem;
%       "b" -> bwd_sem
%     end,
%   {Type,Id} =
%     case TypeStr of
%       "s" -> {sched,list_to_integer(IdStr)};
%       "p" -> {proc,cerl:c_int(list_to_integer(IdStr))}
%     end,
%   {Semantics,Type,Id}.

% opt_to_str({Semantics,Type,Id}) ->
%   case Semantics of
%     fwd_sem -> "f";
%     bwd_sem -> "b"
%   end
%   ++
%   case Type of
%     sched -> "s" ++ integer_to_list(Id);
%     proc -> "p" ++ pp(Id)
%   end.

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
  cerl:c_var({Name,Arity}).

stringToCoreArgs([]) ->
  [];
stringToCoreArgs(Text) ->
  TextDot = Text ++ ".",
  {ok, String, _} = erl_scan:string(TextDot),
  {ok, Exprs} = erl_parse:parse_exprs(String),
  EvalExprs = [element(2,erl_eval:expr(Expr,[])) || Expr <- Exprs],
  [cerl:abstract(Expr) || Expr <- EvalExprs].

filter_options([], _) -> [];
filter_options([CurOpt|RestOpts], Id) ->
  #opt{id = OptId} = CurOpt,
  case (OptId == Id) of
    true -> [CurOpt|filter_options(RestOpts,Id)];
    false -> filter_options(RestOpts,Id)
  end.

has_fwd([]) -> false;
has_fwd([#opt{sem = ?FWD_SEM}|_RestOpts]) -> true;
has_fwd([_CurOpt|RestOpts]) -> has_fwd(RestOpts).


has_bwd([]) -> false;
has_bwd([#opt{sem = ?BWD_SEM}|_RestOpts]) -> true;
has_bwd([_CurOpt|RestOpts]) -> has_bwd(RestOpts).