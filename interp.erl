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
  Gamma = [],
  %InitF = {c_var,[],Fun},
  Procs = [{1,
           {[],{c_apply,[],{c_var,[],Fun},Args}},
           []}],
  System = {Gamma,Procs},
  eval(System),
  fdserver ! terminate.
  %ok.%.

  %fact:InitF(Args).
  % debug
  %io:fwrite("~p~n",[CleanCoreForms]).%,
%FinalSystem = eval(System).

eval(System) ->
 % debug
  io:fwrite("~p~n",[System]),
  NewSystem =
    receive
      {wx,?ID_FORWARD_STEP,_,_,_} -> eval_step(System,forward);
      {wx,?ID_BACKWARD_STEP,_,_,_} -> eval_step(System,backward)
    end,
  eval(NewSystem).
   

eval_step({Gamma,Procs},forward) ->
 % TODO: Define a better eval strategy
  {Pid,{Env,Exp},Mail} = hd(Procs),

  case cerl:type(Exp) of
    apply -> 
      ApplyOp = cerl:apply_op(Exp),
      ApplyArgs = cerl:apply_args(Exp),
      fdserver ! {self(),ApplyOp},
      receive
        FunDef ->
          FunBody = cerl:fun_body(FunDef),
          FunArgs = cerl:fun_vars(FunDef)
      end,
      NewEnv = lists:zip(FunArgs, ApplyArgs),
      NewExp = FunBody,
      % TODO: Define a better eval strategy
      {Gamma,[{Pid,{NewEnv,FunBody},Mail}]};
    'case' ->
      CaseArg = cerl:case_arg(Exp),
      case is_exp(CaseArg) of
        true ->
          {NewEnv,NewCaseArg} = eval_exp(Env,CaseArg),
          NewExp = cerl:update_c_case(Exp,
                                      NewCaseArg,
                                      cerl:case_clauses(Exp)),
          {Gamma,[{Pid,{NewEnv,NewExp},Mail}]};
        false ->
          CaseClauses = cerl:case_clauses()
      end  
      
  end;

eval_step({Gamma,Procs},backward) ->
  {Gamma,Procs}.

is_exp(Exp) -> 
  case cerl:type(Exp) of
    literal -> false;
    % numbers
    var -> true;
    cons -> is_exp(cerl:cons_hd(Exp)) and is_exp(cerl:cons_tl(Exp));
    tuple -> lists:all(is_exp, cerl:tuples_es(Exp));
    values -> lists:all(is_exp, cerl:values_es(Exp))
  end.
    
eval_exp(Env,Exp) -> 
  case cerl:type(Exp) of
    var -> {Env,hd([Val || {Var,Val} <- Env, Var == Exp])};
    _ -> {Env,Exp}
  end.

