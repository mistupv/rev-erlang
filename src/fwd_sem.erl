-module(fwd_sem).
-export([eval_step/2,eval_sched/1,can_eval/2]).

-include("rev_erlang.hrl").

eval_conc(self,Var,Pid) -> [{Var,Pid}];
eval_conc(send,FullMsg,Msgs) -> Msgs ++ [FullMsg].
eval_conc(spawn,Var,CallName,CallArgs,NewEnv) ->
  % TODO: Check if this is working
  freshpidserver ! {self(),new_pid},
  NewPid = cerl:c_int(receive FreshPid -> FreshPid end),
  NewProc = #proc{pid = NewPid, env = NewEnv, exp = cerl:c_apply(CallName,CallArgs)},
  {{Var,NewPid},NewProc}.
eval_conc(rec,Var,ReceiveClauses,Pid,Hist,Env,Exp,Mail) ->
  % Remove this case? This will never happen...
  case length(Mail) of
    0 ->
      io:fwrite("Error: No messages in mailbox~n"),
        #proc{pid = Pid, hist = Hist, env = Env, exp = Exp, mail = Mail};
    _Other ->
      case matchrec(ReceiveClauses,Mail) of
        no_match ->
          io:fwrite("Error: No matching messages~n"),
          #proc{pid = Pid, hist = Hist, env = Env, exp = Exp, mail = Mail};
        {Bindings,NewExp,NewMail} ->
          NewEnv = Env ++ [{Var,NewExp}] ++ Bindings,
          #proc{pid = Pid, hist = [{rec,Mail,Env,Exp}|Hist],
                env = NewEnv, exp = Exp, mail = NewMail}
      end
  end.

eval_seq(Env,Exp) ->
  case cerl:type(Exp) of
    var ->
      [Value] = [Val || {Var,Val} <- Env, Var == Exp],
      {Env,Value,tau};
    cons ->
      % I think we must return the updated expression
      ConsHdExp = cerl:cons_hd(Exp),
      ConsTlExp = cerl:cons_tl(Exp),
      case is_exp(cerl:cons_hd(Exp)) of
        true ->
          {NewEnv,NewConsHdExp,Label} = eval_seq(Env,ConsHdExp),
          NewExp = cerl:update_c_cons(Exp,
                                      NewConsHdExp,
                                      ConsTlExp);
        false ->
          {NewEnv,NewConsTlExp,Label} = eval_seq(Env,ConsTlExp),
          NewExp = cerl:update_c_cons(Exp,
                                      ConsHdExp,
                                      NewConsTlExp)
      end,
      {NewEnv,NewExp,Label};
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
          NewEnv = utils:zip_core(FunArgs, ApplyArgs),
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
          FunArgs = lists:nth(3,CallArgs),
          % here, Core just transforms Args to a literal
          %({c_literal,[],[e_1,e_2]} without transforming e_1)
          %{c_literal,_,FunArgs} = FunCoreArgs,
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

eval_step(#sys{msgs = Msgs, procs = Procs},Pid) ->
  %io:fwrite("Chosen Pid: ~p~n",[Pid]),
  {Proc,RestProcs} = utils:select_proc(Procs,Pid),
  #proc{pid = Pid, hist = Hist, env = Env, exp = Exp, mail = Mail} = Proc,
  {NewEnv,NewExp,Label} = eval_seq(Env,Exp),
  NewSystem = 
    % Labels can contain more or less information than in the papers
    case Label of
      tau ->
        NewProc = Proc#proc{hist = [{tau,Env,Exp}|Hist], env = NewEnv, exp = NewExp},
        #sys{msgs = Msgs, procs = [NewProc|RestProcs]};
        %{Msgs,[{Pid,[tau|Hist],{NewEnv,NewExp},Mail}] ++ RestProcs};
      {self,Var} ->
        NewBind = eval_conc(self,Var,Pid),
        NewProc = Proc#proc{hist = [{self,Env,Exp}|Hist], env = NewEnv++NewBind, exp = NewExp},
        #sys{msgs = Msgs, procs = [NewProc|RestProcs]};
      {send,DestPid,MsgValue} ->
        NewMsgs = eval_conc(send,#msg{src = Pid, dest = DestPid, val = MsgValue}, Msgs),
        NewProc = Proc#proc{hist = [{send,DestPid,Env,Exp}|Hist], env = NewEnv, exp = NewExp},
        #sys{msgs = NewMsgs, procs = [NewProc|RestProcs]};
      {spawn,{Var,CallName,CallArgs}} ->
        {NewBind,SpawnProc} = eval_conc(spawn,Var,CallName,CallArgs,NewEnv),
        #proc{pid = SpawnPid} = SpawnProc,
        NewProc = Proc#proc{hist = [{spawn,SpawnPid,Env,Exp}|Hist], env = NewEnv++[NewBind], exp = NewExp},
        #sys{msgs = Msgs, procs = [NewProc] ++ [SpawnProc] ++ RestProcs};
        % TODO: Put 'rec' hist here
      {rec,Var,ReceiveClauses} ->
        NewProc = eval_conc(rec,Var,ReceiveClauses,Pid,Hist,NewEnv,NewExp,Mail),
        #sys{msgs = Msgs, procs = [NewProc] ++ RestProcs}
    end,
  NewSystem.

eval_sched(#sys{msgs = Msgs, procs = Procs}) ->
  MsgsLen = length(Msgs),
  case MsgsLen of
    0 ->
      #sys{msgs = Msgs, procs = Procs};
    _Other ->
      RandIdx = rand:uniform(MsgsLen),
      RandMsg = lists:nth(RandIdx,Msgs),
      % TODO: delete removes 1st message equal to the message,
      % not the nth message. It can be improved...
      NewMsgs = lists:delete(RandMsg,Msgs),
      #msg{dest = DestPid, val = MsgValue} = RandMsg,
      % TODO: Fix case when DestPid process does not exist
      {Proc,RestProcs} = utils:select_proc(Procs,DestPid),
      #proc{mail = Mail} = Proc,
      NewMail = Mail ++ [MsgValue],
      NewProc = Proc#proc{mail = NewMail},
      {NewMsgs,[NewProc] ++ RestProcs}
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
can_eval(#sys{msgs = []},?ID_GAMMA) ->
  false;
% TODO: Should the selected message be random, rather than the one in the head?
can_eval(#sys{msgs = [#msg{dest = DestPid}|RestMsgs], procs = Procs},?ID_GAMMA) ->
  DestProcs = [Proc || Proc <- Procs, Proc#proc.pid == DestPid],
  case DestProcs of
    [] -> can_eval({RestMsgs,Procs},?ID_GAMMA);
    _Other -> true
  end;
can_eval(#sys{procs = Procs},Pid) ->
  {Proc,_RestProcs} = utils:select_proc(Procs,Pid),
  Exp  = Proc#proc.exp,
  Mail = Proc#proc.mail,
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




