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
          gamma -> fwd_sem:eval_sched(System);
          Pid -> fwd_sem:eval_step(System,Pid)
        end;
      {wx,?ID_BACKWARD_STEP,_,_,_} ->
        % Backward scheduling == Forward scheduling?
        schedserver!{self(),System},
        receive
          gamma -> bwd_sem:eval_sched(System);
          Pid -> bwd_sem:eval_step(System,Pid)
        end
    end,
  eval(NewSystem).
