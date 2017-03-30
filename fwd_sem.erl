-module(fwd_sem).
-export([eval_step/2,eval_sched/1,can_eval/2]).

-define(ID_GAMMA,0).

eval_conc(self,Var,Pid) -> [{Var,Pid}];
eval_conc(send,FullMsg,Gamma) -> Gamma ++ [FullMsg].
eval_conc(spawn,Var,CallName,CallArgs,NewEnv) -> 
  freshpidserver ! {self(),new_pid},
  NewPid = {c_literal,[],receive FreshPid -> FreshPid end}, 
  NewProc = {NewPid,
              {NewEnv,{c_apply,[],CallName,CallArgs}},
              []},
  {{Var,NewPid},NewProc}.
eval_conc(rec,Var,ReceiveClauses,Pid,Env,Exp,Mail) ->
  case length(Mail) of
    0 ->
      io:fwrite("Error: No messages in mailbox~n"),
        {Pid,{Env,Exp},Mail};
    _Other ->
      case matchrec(ReceiveClauses,Mail) of
        no_match ->
          io:fwrite("Error: No matching messages~n"),
          {Pid,{Env,Exp},Mail};
        {Bindings,NewExp,NewMail} ->
          NewEnv = Env ++ [{Var,NewExp}] ++ Bindings,
          {Pid,{NewEnv,Exp},NewMail}
      end
  end.

eval_seq(Env,Exp) ->
  case cerl:type(Exp) of
    var ->
      [Value] = [Val || {Var,Val} <- Env, Var == Exp],
      {Env,Value,tau};
    cons ->
      case is_exp(cerl:cons_hd(Exp)) of
        true -> eval_seq(Env,cerl:cons_hd(Exp));
        false -> eval_seq(Env,cerl:cons_tl(Exp))
      end;
    tuple ->
      eval_list(Env,cerl:tuples_es(Exp));
    apply -> 
      ApplyArgs = cerl:apply_args(Exp),
      ApplyOp = cerl:apply_op(Exp),
      case is_exp(ApplyArgs) of
        true ->
          {NewEnv,NewApplyArgs,Label} = eval_seq(Env,ApplyArgs),
          NewExp = cerl:update_c_apply(Exp,
                                       ApplyOp,
                                       NewApplyArgs),
          {NewEnv,NewExp,Label};
        false ->
          fdserver ! {self(),ApplyOp},
          receive
            FunDef ->
              FunBody = cerl:fun_body(FunDef),
              FunArgs = cerl:fun_vars(FunDef)
          end,
          NewEnv = lists:zip(FunArgs, ApplyArgs),
          {NewEnv,FunBody,tau}
      end;
    'case' ->
      CaseArg = cerl:case_arg(Exp),
      case is_exp(CaseArg) of
        true ->
          {NewEnv,NewCaseArg,Label} = eval_seq(Env,CaseArg),
          NewExp = cerl:update_c_case(Exp,
                                      NewCaseArg,
                                      cerl:case_clauses(Exp)),
          {NewEnv,NewExp,Label};
        false ->
          CaseClauses = cerl:case_clauses(Exp),
          case cerl_clauses:reduce(CaseClauses,[CaseArg]) of
            {true,{Clause,Bindings}} ->
              % TODO: Improve Env++Bindings (i.e., no duplicates)
              ClauseBody = cerl:clause_body(Clause),
              NewEnv = Env++Bindings,
              {NewEnv,ClauseBody,tau};
            {false,_} ->
              io:fwrite("Error: No matching clause~n") 
          end
      end;
    'let' ->
      LetArg = cerl:let_arg(Exp),
      case is_exp(LetArg) of
        true ->
          {NewEnv,NewLetArg,Label} = eval_seq(Env,LetArg),
          NewExp = cerl:update_c_let(Exp,
                                     cerl:let_vars(Exp),
                                     NewLetArg,
                                     cerl:let_body(Exp)),
          {NewEnv,NewExp,Label};
        false ->
          LetVars = cerl:let_vars(Exp),
          NewEnv =
            case cerl:let_arity(Exp) of
              1 -> lists:zip(LetVars,[LetArg]);
              _Other -> lists:zip(LetVars,LetArg)
            end
           ++ Env,
          NewExp = cerl:let_body(Exp),
          {NewEnv,NewExp,tau}
      end;
    call ->
      CallArgs = cerl:call_args(Exp),
      CallModule = cerl:call_module(Exp),
      CallName = cerl:call_name(Exp),
      case {CallModule, CallName} of
        {{c_literal,_,'erlang'},{c_literal,_,'spawn'}} -> 
          freshvarserver ! {self(),new_var},
          Var = receive NewVar -> NewVar end,
          FunName = lists:nth(2,CallArgs),
          FunCoreArgs = lists:nth(3,CallArgs),
          % here, Core just transforms Args to a literal
          %({c_literal,[],[e_1,e_2]} without transforming e_1)
          {c_literal,_,FunArgs} = FunCoreArgs,
          {Env,Var,{spawn,{Var,FunName,FunArgs}}};
        _Other ->
        % should we also eval module or name?
          case is_exp(CallArgs) of
            true ->
              {NewEnv,NewCallArgs,Label} = eval_list(Env,CallArgs),
              NewExp = cerl:update_c_call(Exp,
                                          CallModule,
                                          CallName,
                                          NewCallArgs),
              {NewEnv,NewExp,Label};
            false ->
              case CallModule of
                % improve error
                {c_literal,_,'erlang'} -> 
                  case CallName of
                    {c_literal,_,'self'} ->
                      freshvarserver ! {self(),new_var},
                      Var = receive NewVar -> NewVar end,
                      {Env,Var,{self,Var}};
                    {c_literal,_,'!'} ->
                      DestPid = lists:nth(1,CallArgs),
                      MsgValue = lists:nth(2,CallArgs),
                      {Env,MsgValue,{send,DestPid,MsgValue}};
                    _OtherName ->   
                      erlang:error(undef_name)
                  end;
                _OtherModule ->
                  erlang:error(undef_call)
              end    
          end
      end;
    seq ->
      % we could also replace 'seq' by 'let' statements
      SeqArg = cerl:seq_arg(Exp),
      case is_exp(SeqArg) of
        true ->
          {NewEnv,NewSeqArg,Label} = eval_seq(Env,SeqArg),
          NewExp = cerl:update_c_seq(Exp,
                                     NewSeqArg,
                                     cerl:seq_body(Exp)),
          {NewEnv,NewExp,Label};
        false ->
          % not sure about this
          NewExp = cerl:seq_body(Exp),
          {Env,NewExp,tau}
      end;
    'receive' ->
        freshvarserver ! {self(),new_var},
        Var = receive NewVar -> NewVar end,
        {Env,Var,{rec,Var,cerl:receive_clauses(Exp)}}
  end.

eval_step({Gamma,Procs},Pid) ->
  %io:fwrite("Chosen Pid: ~p~n",[Pid]),
  {Proc,RestProcs} = utils:select_proc(Procs,Pid),
  {Pid,{Env,Exp},Mail} = Proc,
  {NewEnv,NewExp,Label} = eval_seq(Env,Exp),
  NewSystem = 
    % Labels can contain more or less information than in the papers
    case Label of
      tau -> 
        {Gamma,[{Pid,{NewEnv,NewExp},Mail}] ++ RestProcs};
      {self,Var} ->
        NewBind = eval_conc(self,Var,Pid),
        {Gamma,[{Pid,{NewEnv++NewBind,NewExp},Mail}] ++ RestProcs};
      {send,DestPid,MsgValue} ->
        NewGamma = eval_conc(send,{Pid,DestPid,MsgValue},Gamma),
        {NewGamma,[{Pid,{NewEnv,NewExp},Mail}] ++ RestProcs};
      {spawn,{Var,CallName,CallArgs}} ->
        {NewBind,NewProc} = eval_conc(spawn,Var,CallName,CallArgs,NewEnv),
        {Gamma,[{Pid,{NewEnv++NewBind,NewExp},Mail}] ++ [NewProc] ++ RestProcs};
      {rec,Var,ReceiveClauses} ->
        NewProc = eval_conc(rec,Var,ReceiveClauses,Pid,NewEnv,NewExp,Mail),
        {Gamma,[NewProc] ++ RestProcs}
    end,
  NewSystem.

eval_sched({Gamma,Procs}) ->
  GammaLen = length(Gamma),
  case GammaLen of
    0 ->
      {Gamma,Procs};
    _Other ->
      RandIdx = rand:uniform(GammaLen),
      RandMsg = lists:nth(RandIdx,Gamma),
      % TODO: delete removes 1st message equal to the message,
      % not the nth message. It can be improved...
      NewGamma = lists:delete(RandMsg,Gamma),
      {_SrcPid,DestPid,MsgValue} = RandMsg,
      % TODO: Fix case when DestPid process does not exist
      {Proc,RestProcs} = utils:select_proc(Procs,DestPid),
      {Pid,{Env,Exp},Mail} = Proc,
      NewMail = Mail ++ [MsgValue],
      NewProc = {Pid,{Env,Exp},NewMail},
      {NewGamma,[NewProc] ++ RestProcs}
  end.


is_exp([]) -> false;
is_exp(Exp) when is_list(Exp) ->
  lists:any(fun is_exp/1, Exp);
is_exp(Exp) -> 
  case cerl:type(Exp) of
    literal -> false;
    nil -> false;
    cons -> is_exp(cerl:cons_hd(Exp)) or is_exp(cerl:cons_tl(Exp));
    tuple -> is_exp(cerl:tuples_es(Exp));
    %values -> lists:all(is_exp, cerl:values_es(Exp));
    _Other -> true
  end.

eval_list(Env,[]) ->
  {Env,[]};
eval_list(Env,[Exp|Exps]) ->
  case is_exp(Exp) of
    true ->
      {NewEnv,NewExp,Label} = eval_seq(Env,Exp),
      {NewEnv,[NewExp|Exps],Label};
    false ->
      % Not sure about this
      {NewEnv,NewExp,Label} = eval_list(Env,Exps),
      {NewEnv,[Exp|NewExp],Label}
  end.

matchrec(Clauses,Mail) ->
  matchrec(Clauses,Mail,[]).

matchrec(_,[],_) ->
  no_match;
matchrec(Clauses,[CurMsg|RestMsgs],AccMsgs) ->
  case cerl_clauses:reduce(Clauses,[CurMsg]) of
    {true,{Clause,Bindings}} ->
      ClauseBody = cerl:clause_body(Clause),
      NewMsgs =  AccMsgs ++ RestMsgs,
      {Bindings,ClauseBody,NewMsgs};
    {false,_} ->
      matchrec(Clauses,RestMsgs,[CurMsg] ++ AccMsgs)
  end.
can_eval({[],_Procs},?ID_GAMMA) ->
  false;
can_eval({[{_SrcPid,DestPid,_MsgValue}|RestMsgs],Procs},?ID_GAMMA) ->
  DestProcs = [{P,S,M} || {P,S,M} <- Procs, P == DestPid],
  case DestProcs of
    [] -> can_eval({RestMsgs,Procs},?ID_GAMMA);
    _Other -> true
  end;
can_eval({_Gamma,Procs},Pid) ->
  {Proc,_RestProcs} = utils:select_proc(Procs,Pid),
  {Pid,{_Env,Exp},Mail} = Proc,
  case is_exp(Exp) of
    true ->
      case cerl:type(Exp) of
        'receive' when length(Mail) == 0 -> false;
        'receive' ->
          ReceiveClauses = cerl:receive_clauses(Exp),
          case matchrec(ReceiveClauses, Mail) of
            no_match -> false;
            _Other -> true
          end;
          _Other -> true
      end;
    false -> false
  end.




