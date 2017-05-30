-module(fwd_sem).
-export([eval_step/2,eval_sched/2,eval_opts/1,eval_procs_opts/1]).

-include("rev_erlang.hrl").

eval_conc(self,Var,Pid) -> [{Var,Pid}];
eval_conc(send,FullMsg,Msgs) -> Msgs ++ [FullMsg].
eval_conc(spawn,Var,CallName,CallArgs,NewEnv) ->
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
  case is_list(Exp) of
    true -> eval_list(Env,Exp);
    false -> eval_seq_1(Env,Exp)
  end.

eval_seq_1(Env,Exp) ->
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
          % standard zip is used here (pretty-printer forces it)
          NewEnv = lists:zip(FunArgs,ApplyArgs),
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
          % here, Core just transforms Args to a literal
          %({c_literal,[],[e_1,e_2]} without transforming e_1)
          FunArgs = utils:list_from_core(lists:nth(3,CallArgs)),
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
                      freshtimeserver ! {self(),new_time},
                      Time = receive NewTime -> NewTime end,
                      DestPid = lists:nth(1,CallArgs),
                      MsgValue = lists:nth(2,CallArgs),
                      {Env,MsgValue,{send,Time,DestPid,MsgValue}};
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
  {Proc,RestProcs} = utils:select_proc(Procs,Pid),
  #proc{pid = Pid, hist = Hist, env = Env, exp = Exp, mail = Mail} = Proc,
  {NewEnv,NewExp,Label} = eval_seq(Env,Exp),
  NewSystem = 
    % Labels can contain more or less information than in the papers
    case Label of
      tau ->
        NewProc = Proc#proc{hist = [{tau,Env,Exp}|Hist], env = NewEnv, exp = NewExp},
        #sys{msgs = Msgs, procs = [NewProc|RestProcs]};
      {self,Var} ->
        NewBind = eval_conc(self,Var,Pid),
        NewProc = Proc#proc{hist = [{self,Env,Exp}|Hist], env = NewEnv++NewBind, exp = NewExp},
        #sys{msgs = Msgs, procs = [NewProc|RestProcs]};
      {send,Time,DestPid,MsgValue} ->
        % TODO: Update send label up to current version of semantics
        NewMsgs = eval_conc(send,#msg{time = Time, src = Pid, dest = DestPid, val = MsgValue}, Msgs),
        NewProc = Proc#proc{hist = [{send,Time,DestPid,Env,Exp}|Hist], env = NewEnv, exp = NewExp},
        #sys{msgs = NewMsgs, procs = [NewProc|RestProcs]};
        % TODO: Update spawn label up to current version of semantics (if needed)
      {spawn,{Var,CallName,CallArgs}} ->
        {NewBind,SpawnProc} = eval_conc(spawn,Var,CallName,CallArgs,NewEnv),
        #proc{pid = SpawnPid} = SpawnProc,
        NewProc = Proc#proc{hist = [{spawn,SpawnPid,Env,Exp}|Hist], env = NewEnv++[NewBind], exp = NewExp},
        #sys{msgs = Msgs, procs = [NewProc|[SpawnProc|RestProcs]]};
        % TODO: Put 'rec' hist here
        % TODO: Update rec label up to current version of semantics
      {rec,Var,ReceiveClauses} ->
        NewProc = eval_conc(rec,Var,ReceiveClauses,Pid,Hist,NewEnv,NewExp,Mail),
        #sys{msgs = Msgs, procs = [NewProc|RestProcs]}
    end,
  NewSystem.

eval_sched(#sys{msgs = Msgs, procs = Procs},Id) ->
  {Msg,RestMsgs} = utils:select_msg(Msgs,Id),
  #msg{dest = DestPid} = Msg,
  {Proc,RestProcs} = utils:select_proc(Procs,DestPid),
  #proc{mail = Mail} = Proc,
  % TODO: Check if msg is delivered "as is"
  NewMail = [Msg|Mail],
  NewProc = Proc#proc{mail = NewMail},
  #sys{msgs = RestMsgs, procs = [NewProc|RestProcs]}.

is_exp([]) -> false;
is_exp(Exp) when is_list(Exp) ->
  lists:any(fun is_exp/1, Exp);
is_exp(Exp) -> 
  case cerl:type(Exp) of
    literal -> false;
    nil -> false;
    cons -> is_exp(cerl:cons_hd(Exp)) or is_exp(cerl:cons_tl(Exp));
    tuple -> is_exp(cerl:tuple_es(Exp));
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

eval_opts(System) ->
  SchedOpts = eval_sched_opts(System),
  ProcsOpts = eval_procs_opts(System),
  SchedOpts ++ ProcsOpts.

eval_sched_opts(#sys{msgs = []}) ->
  [];
eval_sched_opts(#sys{msgs = [CurMsg|RestMsgs], procs = Procs}) ->
  DestPid = CurMsg#msg.dest,
  DestProcs = [ P || P <- Procs, P#proc.pid == DestPid],
  case DestProcs of
    [] ->
      eval_sched_opts(#sys{msgs = RestMsgs, procs = Procs});
    _Other ->
      Time = CurMsg#msg.time,
      [#opt{sem = ?MODULE, type = ?TYPE_MSG, id = Time, rule = ?RULE_SCHED}|eval_sched_opts(#sys{msgs = RestMsgs, procs = Procs})]
  end.

eval_procs_opts(#sys{procs = []}) ->
  [];
eval_procs_opts(#sys{procs = [CurProc|RestProcs]}) ->
  Exp = CurProc#proc.exp,
  Pid = CurProc#proc.pid,
  case is_exp(Exp) of
    true ->
      case cerl:type(Exp) of
      %   call ->
      % CallArgs = cerl:call_args(Exp),
      % CallModule = cerl:call_module(Exp),
      % CallName = cerl:call_name(Exp),

      % case CallModule of
      %   {c_literal,_,'erlang'} -> 
      %   case CallName of
      %     {c_literal,_,'spawn'} ->
      %     {c_literal,_,'self'} ->
      %     {c_literal,_,'!'} ->
      %     _Other ->
      %   end;
      %   _Other -> erlang:error(undef_call)
      % end

      % end;
        'receive' ->
          ReceiveClauses = cerl:receive_clauses(Exp),
          Mail = CurProc#proc.mail,
          case matchrec(ReceiveClauses, Mail) of
            no_match -> eval_procs_opts(#sys{procs = RestProcs});
            _Other -> [#opt{sem = ?MODULE, type = ?TYPE_PROC, id = Pid, rule = ?RULE_RECEIVE}|eval_procs_opts(#sys{procs = RestProcs})]
          end;
        % RULE_SEQ here is wrong  
        _Other -> [#opt{sem = ?MODULE, type = ?TYPE_PROC, id = Pid, rule = ?RULE_SEQ}|eval_procs_opts(#sys{procs = RestProcs})]
      end;
    false -> eval_procs_opts(#sys{procs = RestProcs})
  end.

