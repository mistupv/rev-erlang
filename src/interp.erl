-module(interp).
-export([start/3]).

-include("rev_erlang.hrl").
-include_lib("wx/include/wx.hrl").
                      
start(ModuleFile,Fun,Args) ->
  {ok,_,CoreForms} = compile:file(ModuleFile,[to_core,binary]),
  Stripper = fun(Tree) -> cerl:set_ann(Tree, []) end,
  CleanCoreForms = cerl_trees:map(Stripper,CoreForms),
  FunDefs = cerl:module_defs(CleanCoreForms),

  % Wx=wx:new(),
  % F=wxFrame:new(Wx, -1, "rev-erlang"),
  % Panel = wxPanel:new(F),
  % BoxSizer = wxBoxSizer:new(?wxVERTICAL),
  % %SzFlags = [{proportion, 0}, {border, 4}, {flag, ?wxALL}],
  % ButtonForward = wxButton:new(Panel, ?ID_FORWARD_STEP, [{label,"forward"}]),
  % wxButton:connect(ButtonForward, command_button_clicked, []),
  % ButtonBackward = wxButton:new(Panel, ?ID_BACKWARD_STEP, [{label,"backward"}]),
  % wxButton:connect(ButtonBackward, command_button_clicked, []),
  % wxSizer:add(BoxSizer, ButtonForward, []),
  % wxSizer:add(BoxSizer, ButtonBackward, []),
  % wxPanel:setSizer(Panel, BoxSizer),
  % wxFrame:show(F),
  FunDefServer = spawn(fundefserver,start,[FunDefs]),
  case lists:member(fdserver,registered()) of
    true -> unregister(fdserver);
    false -> ok
  end,
  register(fdserver,FunDefServer),
  FreshPidServer = spawn(freshpidserver,start,[]),
  case lists:member(freshpidserver,registered()) of
    true -> unregister(freshpidserver);
    false -> ok
  end,
  register(freshpidserver,FreshPidServer),
  FreshTimeServer = spawn(freshtimeserver,start,[]),
  case lists:member(freshtimeserver,registered()) of
    true -> unregister(freshtimeserver);
    false -> ok
  end,
  register(freshtimeserver,FreshTimeServer),
  FreshVarServer = spawn(freshvarserver,start,[]),
  case lists:member(freshvarserver,registered()) of
    true -> unregister(freshvarserver);
    false -> ok
  end,
  register(freshvarserver,FreshVarServer),
  Proc = #proc{pid = cerl:c_int(1),
               exp = cerl:c_apply(Fun,Args)},
  Procs = [Proc],
  System = #sys{procs = Procs},
  io:fwrite("~s~n",[utils:pp_system(System)]),
  eval(System),
  fdserver ! terminate,
  freshpidserver ! terminate,
  freshtimeserver ! terminate,
  freshvarserver ! terminate.

eval(System) ->
  FwdOpts = fwd_sem:eval_opts(System),
  BwdOpts = bwd_sem:eval_opts(System),
  AllOpts = FwdOpts ++ BwdOpts,
  StrOpts = string:join([utils:opt_to_str(Opt) || Opt <- AllOpts]," "),
  io:fwrite("Available rules: ~s~n",[StrOpts]),
  StrOpt =
    case io:fread("Select rule: ","~s") of
      {ok,[ReadStr]} -> ReadStr;
      _ -> error
    end,
  {Semantics,Type,Id} = utils:str_to_opt(StrOpt),
  NewSystem =
    case Type of
      sched -> Semantics:eval_sched(System,Id);
      proc -> Semantics:eval_step(System,Id)
    end,
  io:fwrite("~s~n",[utils:pp_system(NewSystem)]),
  eval(NewSystem).
