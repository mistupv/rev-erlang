-module(interp).
-export([start/2]).

-include_lib("wx/include/wx.hrl").

-define(ID_FORWARD_STEP,  40).
-define(ID_BACKWARD_STEP, 41).

-define(ID_GAMMA,0).
                      
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
  FreshPidServer = spawn(freshpidserver,start,[]),
  case lists:member(freshpidserver,registered()) of
    true -> unregister(freshpidserver);
    false -> ok
  end,
  register(freshpidserver,FreshPidServer),
  FreshVarServer = spawn(freshvarserver,start,[]),
  case lists:member(freshvarserver,registered()) of
    true -> unregister(freshvarserver);
    false -> ok
  end,
  register(freshvarserver,FreshVarServer),
  Gamma = [],
  %InitF = {c_var,[],Fun},
  Procs = [{{c_literal,[],1},
           {[],{c_apply,[],{c_var,[],Fun},Args}},
           []}],
  System = {Gamma,Procs},
  io:fwrite("~p~n",[System]),
  eval(System),
  fdserver ! terminate,
  schedserver ! terminate,
  freshpidserver ! terminate,
  freshvarserver ! terminate.

eval(System) ->
  
  Semantics =
    receive
      {wx,?ID_FORWARD_STEP,_,_,_} ->
        fwd_sem;
      {wx,?ID_BACKWARD_STEP,_,_,_} ->
        bwd_sem
    end,
  schedserver!{self(),Semantics,System},
  {NewSystem,Evaluated} =
    receive
      null_pid ->
        {System,false};
      ?ID_GAMMA ->
        {Semantics:eval_sched(System),true};
      Pid ->
        {Semantics:eval_step(System,Pid),true}
    end,
  case Evaluated of
    false ->
      io:fwrite("System is reduced!~n");
    true ->
      io:fwrite("~p~n",[NewSystem])
  end,
  eval(NewSystem).
