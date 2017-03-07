-module(interp).
-export([start/2]).

-include_lib("wx/include/wx.hrl").

-define(ID_FORWARD_STEP,  40).
-define(ID_BACKWARD_STEP, 41).
                      
start(ModuleFile, {Fun,Args}) ->
  {ok,_,CoreForms} = compile:file(ModuleFile,[to_core,binary]),
  Stripper = fun(Tree) -> cerl:set_ann(Tree, []) end,
  CleanCoreForms = cerl_trees:map(Stripper,CoreForms),
  FunDefs = cerl:module_defs(CleanCoreForms),

  Wx=wx:new(),
  F=wxFrame:new(Wx, -1, "rev-erlang"),
  Panel = wxPanel:new(F),
  BoxSizer = wxBoxSizer:new(?wxVERTICAL),
  %SzFlags = [{proportion, 0}, {border, 4}, {flag, ?wxALL}],
  ButtonForward = wxButton:new(Panel, ?ID_FORWARD_STEP, [{label,"forward"}]),
  wxButton:connect(ButtonForward, command_button_clicked, []),
  ButtonBackward = wxButton:new(Panel, ?ID_BACKWARD_STEP, [{label,"backward"}]),
  wxButton:connect(ButtonBackward, command_button_clicked, []),
  wxSizer:add(BoxSizer, ButtonForward, []),
  wxSizer:add(BoxSizer, ButtonBackward, []),
  wxPanel:setSizer(Panel, BoxSizer),
  wxFrame:show(F),
  FunDefServer = spawn(fundefserver,start,[FunDefs]),
  case lists:member(fdserver,registered()) of
    true -> unregister(fdserver);
    false -> ok
  end,
  register(fdserver,FunDefServer),
  SchedServer = spawn(schedserver,start,[]),
  case lists:member(schedserver,registered()) of
    true -> unregister(schedserver);
    false -> ok
  end,
  register(schedserver,SchedServer),
  FreshServer = spawn(freshserver,start,[]),
  case lists:member(freshserver,registered()) of
    true -> unregister(freshserver);
    false -> ok
  end,
  register(freshserver,FreshServer),
  Gamma = [],
  %InitF = {c_var,[],Fun},
  Procs = [{{c_literal,[],1},
           {[],{c_apply,[],{c_var,[],Fun},Args}},
           []}],
  System = {Gamma,Procs},
  eval(System),
  fdserver ! terminate,
  schedserver ! terminate,
  freshserver ! terminate.

eval(System) ->
  io:fwrite("~p~n",[System]),
  NewSystem =
    receive
      {wx,?ID_FORWARD_STEP,_,_,_} ->
        schedserver!{self(),System},
        receive
          gamma -> eval_sched(System,forward);
          Pid -> eval_step(System,Pid,forward)
        end;
      {wx,?ID_BACKWARD_STEP,_,_,_} ->
        % Backward scheduling == Forward scheduling?
        schedserver!{self(),System},
        receive
          gamma -> eval_sched(System,backward);
          Pid -> eval_step(System,Pid,backward)
        end
    end,
  eval(NewSystem).
   
eval_sched(System,forward) ->
  System;
eval_sched(System,backward) ->
  System.

% TODO: split eval_step in 2 modules (1 fwd, 1 bwd)?
eval_step({Gamma,Procs},Pid,forward) ->
  %io:fwrite("Chosen Pid: ~p~n",[Pid]),
  [Proc] = [{P,S,M} || {P,S,M} <- Procs, P == Pid],
  {Pid,{Env,Exp},Mail} = Proc,
  RestProcs = [{P,S,M} || {P,S,M} <- Procs, P /= Pid],
  {NewEnv,NewExp,Label} = eval_seq(Env,Exp),
  NewSystem = 
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
        {Gamma,[{Pid,{NewEnv++NewBind,NewExp},Mail}] ++ [NewProc] ++ RestProcs}
    end,
  NewSystem;

eval_step({Gamma,Procs},_Pid,backward) ->
  {Gamma,Procs}.

eval_conc(self,Var,Pid) -> [{Var,Pid}];
eval_conc(send,FullMsg,Gamma) -> Gamma ++ FullMsg.
eval_conc(spawn,Var,CallName,CallArgs,NewEnv) -> 
  {self(),new_pid} ! freshserver,
  NewPid = {c_literal,[],receive FreshPid -> FreshPid end}, 
  NewProc = {NewPid,
              {NewEnv,{c_apply,[],{c_var,[],CallName},CallArgs}},
              []},
  {{Var,NewPid},NewProc}.

eval_seq(Env,Exp) ->
  case cerl:type(Exp) of
    % literal ->
    %   {Env,Exp,tau};
    % nil -> 
    % {Env,Exp,tau};
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
        {'erlang','spawn'} -> 
          % TODO: Fresh names!
          Var = {c_var,[],y},
          {Env,Var,{spawn,{Var,CallName,CallArgs}}};
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
                      Var = {c_var,[],y},
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
      end
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
