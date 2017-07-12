-module(utils).
-export([fundef_lookup/2, fundef_rename/1, build_var/1,
         select_proc/2, select_msg/2, select_proc_with_time/2,
         list_from_core/1,
         update_env/2, merge_env/2,
         replace/3, pp_system/1,
         % opt_to_str/1,str_to_opt/1,
         moduleNames/1,
         stringToFunName/1,stringToCoreArgs/1, toCore/1,
         filter_options/2, has_fwd/1, has_bwd/1, has_norm/1,
         is_queue_minus_msg/3, topmost_rec/1]).

-include("rev_erlang.hrl").

fundef_lookup(FunName, FunDefs) ->
  {_, FunDef} = lists:keyfind(FunName, 1, FunDefs),
  FunDef.

fundef_rename(FunDef) ->
  FunVars = cerl:fun_vars(FunDef),
  FunBody = cerl:fun_body(FunDef),
  RenamedVars = pars_rename(FunVars),
  {RenamedExp, _} =
    cerl_trees:mapfold(fun (Exp, Acc) ->
                          case cerl:type(Exp) of
                            var ->
                              case cerl:var_name(Exp) of
                                {_FunName, _FunArity} ->
                                  NewExp = Exp,
                                  NewAcc = Acc;
                              _OtherName ->
                                case lists:keyfind(Exp, 1, Acc) of
                                  false ->
                                    NewExp = fresh_var(),
                                    NewAcc = [{Exp,NewExp}] ++ Acc;
                                  {Exp, NewVar} ->
                                    NewExp = NewVar,
                                    NewAcc = Acc
                                end
                              end;
                            _Other ->
                              NewExp = Exp,
                              NewAcc = Acc
                          end,
                          {NewExp, NewAcc}
                        end,
                        RenamedVars,
                        FunBody),
  NewFunDef = cerl:c_fun([NewVar || {_, NewVar} <- RenamedVars], RenamedExp),
  NewFunDef.

pars_rename(Vars) ->
  [{Var, fresh_var()} || Var <- Vars].

build_var(Num) ->
  NumAtom = list_to_atom("y_" ++ integer_to_list(Num)),
  cerl:c_var(NumAtom).

select_proc(Procs, Pid) ->
  [Proc] = [ P || P <- Procs, P#proc.pid == Pid],
  RestProcs = [ P || P <- Procs, P#proc.pid /= Pid],
  {Proc, RestProcs}.

select_msg(Msgs, Time) ->
  [Msg] = [ M || M <- Msgs, M#msg.time == Time],
  RestMsgs = [ M || M <- Msgs, M#msg.time /= Time],
  {Msg, RestMsgs}.

select_proc_with_time(Procs, Time) ->
  ProcMailPairs = [ {Proc, Proc#proc.mail} || Proc <- Procs],
  lists:filter( fun ({_, Mail}) ->
                  case Mail of
                    [] ->
                      false;
                    [{_, MsgTime}|_RestMsgs] ->
                      MsgTime == Time
                  end
                end,
                ProcMailPairs).


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
  string:join(ProcsList," |\n").

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
   string:join(PairsList,", "),
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
stringToCoreArgs(Str) ->
  StrDot = Str ++ ".",
  {ok, ParsedStr, _} = erl_scan:string(StrDot),
  {ok, Exprs} = erl_parse:parse_exprs(ParsedStr),
  CoreExprs = [toCore(Expr) || Expr <- Exprs],
  CoreExprs.

toCore(Expr) ->
  case Expr of
    {atom, _, Atom} ->
      cerl:c_atom(Atom);
    {integer, _, Int} ->
      cerl:c_int(Int);
    {float, _, Float} ->
      cerl:c_float(Float);
    {string, _, String} ->
      cerl:c_string(String);
    {tuple, _, TupleEs} ->
      cerl:c_tuple_skel([toCore(E) || E <- TupleEs]);
    {cons, _, Head, Tail} ->
      cerl:c_cons_skel(toCore(Head), toCore(Tail));
    {nil, _} ->
      cerl:c_nil()
  end.

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

has_norm([]) -> false;
has_norm([#opt{sem = ?FWD_SEM, rule = Rule}|RestOpts]) ->
  case Rule of
    ?RULE_SCHED -> has_norm(RestOpts);
    _OtherRule -> true
  end;
has_norm([_CurOpt|RestOpts]) -> has_norm(RestOpts).

% returns true if Queue\Msg == OtherQueue
is_queue_minus_msg(Queue, Msg, OtherQueue) ->
  ThisQueue = lists:delete(Msg, Queue),
  ThisQueue == OtherQueue.

topmost_rec([]) -> no_rec;
topmost_rec([CurHist|RestHist]) ->
  case CurHist of
    {rec,_,_,_,_} -> CurHist;
    _Other -> topmost_rec(RestHist)
  end.

fresh_var() ->
  VarNum = ref_lookup(?FRESH_VAR),
  ref_add(?FRESH_VAR, VarNum + 1),
  utils:build_var(VarNum).

ref_add(Id, Ref) ->
    ets:insert(?APP_REF, {Id, Ref}).

ref_lookup(Id) ->
    ets:lookup_element(?APP_REF, Id, 2).
