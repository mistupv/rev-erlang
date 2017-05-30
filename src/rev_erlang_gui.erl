-module(rev_erlang_gui).
-export([setup_gui/0]).

-include("rev_erlang.hrl").
-include("rev_erlang_gui.hrl").
-include_lib("wx/include/wx.hrl").

setup_gui() ->
  Server = wx:new(),
  Frame = wxFrame:new(Server, -1, ?APP_STRING,[{size,?FRAME_SIZE_INIT}]),
  ref_start(),
  ref_add(?FILE_PATH, "."),
  ref_add(?STATUS, #status{}),
  ref_add(?FRAME, Frame),
  setupMenu(),
  wxFrame:createStatusBar(Frame, [{id, ?STATUS_BAR}]),
  wxEvtHandler:connect(Frame, close_window),
  wxEvtHandler:connect(Frame, command_button_clicked),
  wxEvtHandler:connect(Frame, command_menu_selected),
  wxEvtHandler:connect(Frame, command_text_updated),
  setupMainPanel(Frame),
  wxFrame:show(Frame),
  loop(),
  utils_gui:stop_servers(),
  ref_stop().

setupMainPanel(Parent) ->
  MainPanel = wxPanel:new(Parent),
  MainSizer = wxBoxSizer:new(?wxHORIZONTAL),

  LeftPanel = wxPanel:new(MainPanel),
  LeftSizer = setupLeftSizer(LeftPanel),
  wxWindow:setSizerAndFit(LeftPanel, LeftSizer),

  RightPanel = wxPanel:new(MainPanel),
  RightSizer = setupRightSizer(RightPanel),
  wxWindow:setSizerAndFit(RightPanel, RightSizer),

  wxSizer:add(MainSizer, LeftPanel),
  wxSizer:add(MainSizer, RightPanel),
  wxWindow:setSizer(MainPanel, MainSizer),
  MainPanel.

setupLeftSizer(Parent) ->
  Notebook = wxNotebook:new(Parent, ?LEFT_NOTEBOOK),
  ref_add(?LEFT_NOTEBOOK, Notebook),
  CodePanel = setupCodePanel(Notebook),
  StatePanel = setupStatePanel(Notebook),
  wxNotebook:addPage(Notebook, CodePanel, "Code"),
  wxNotebook:addPage(Notebook, StatePanel, "State"),
  % wxNotebook:layout(Notebook),
  LeftSizer = wxBoxSizer:new(?wxVERTICAL),
  wxSizer:add(LeftSizer, Notebook),
  LeftSizer.

setupCodePanel(Parent) ->
  CodePanel = wxPanel:new(Parent),   
  CodeText = wxTextCtrl:new(CodePanel, ?CODE_TEXT,
                             [{style,?wxTE_MULTILINE bor ?wxTE_READONLY},
                              {size,{460,460}}]),
  ref_add(?CODE_TEXT,CodeText),
  FundefStaticText = wxStaticText:new(CodePanel, ?wxID_ANY, "Funs: "),
  FunChoice = wxChoice:new(CodePanel, ?wxID_ANY),
  ref_add(?FUN_CHOICE,FunChoice),
  InputStaticText1 = wxStaticText:new(CodePanel, ?wxID_ANY, "Input args: ["),
  InputStaticText2 = wxStaticText:new(CodePanel, ?wxID_ANY, "]"),
  InputTextCtrl = wxTextCtrl:new(CodePanel, ?INPUT_TEXT,
                                 [{style, ?wxBOTTOM},
                                  {value, ""}]),
  ref_add(?INPUT_TEXT,InputTextCtrl),
  StartButton = wxButton:new(CodePanel, ?START_BUTTON,
                             [{label, "START"}, {size, {60, -1}}]),
  ref_add(?START_BUTTON,StartButton),
  wxButton:disable(StartButton),

  CodeSizer = wxBoxSizer:new(?wxVERTICAL),
  InputSizer = wxBoxSizer:new(?wxHORIZONTAL),
  BorderSizer = wxBoxSizer:new(?wxVERTICAL),

  wxSizer:add(CodeSizer, CodeText),
  wxSizer:addSpacer(CodeSizer, 10),
  wxSizer:add(CodeSizer, InputSizer),

  wxSizer:add(InputSizer, FundefStaticText),
  wxSizer:add(InputSizer, FunChoice),
  wxSizer:addSpacer(InputSizer, 10),
  wxSizer:add(InputSizer, InputStaticText1),
  wxSizer:add(InputSizer, InputTextCtrl),
  wxSizer:add(InputSizer, InputStaticText2),
  wxSizer:addSpacer(InputSizer, 10),
  wxSizer:add(InputSizer, StartButton, [{flag,?wxALIGN_RIGHT}]),

  wxSizer:add(BorderSizer, CodeSizer, [{flag, ?wxALL}, {border, 10}]),
  wxWindow:setSizer(CodePanel, BorderSizer),
  CodePanel.

 setupStatePanel(Parent) ->
  StatePanel = wxPanel:new(Parent),
  StateText = wxTextCtrl:new(StatePanel, ?STATE_TEXT,
                             [{style,?wxTE_MULTILINE bor ?wxTE_READONLY},
                              {size,{460,460}}]),
  ref_add(?STATE_TEXT, StateText),
  StateSizer = wxBoxSizer:new(?wxVERTICAL),
  BorderSizer = wxBoxSizer:new(?wxVERTICAL),

  wxSizer:add(StateSizer, StateText),
  wxSizer:add(BorderSizer, StateSizer, [{flag, ?wxALL}, {border, 10}]),
  wxWindow:setSizer(StatePanel, BorderSizer),
  StatePanel.

setupRightSizer(Parent) ->
  Notebook = wxNotebook:new(Parent, ?RIGHT_NOTEBOOK),
  ref_add(?RIGHT_NOTEBOOK, Notebook),
  ManuPanel = setupManualPanel(Notebook),
  % SemiPanel = setupSemiPanel(Notebook),
  AutoPanel = setupAutoPanel(Notebook),
  wxNotebook:addPage(Notebook, ManuPanel, "Manual"),
  % wxNotebook:addPage(Notebook, SemiPanel, "Semi"),
  wxNotebook:addPage(Notebook, AutoPanel, "Automatic"),
  % wxNotebook:layout(Notebook),
  RightSizer = wxBoxSizer:new(?wxVERTICAL),
  wxSizer:add(RightSizer, Notebook),
  RightSizer.

setupManualPanel(Parent) ->
  ManuPanel = wxPanel:new(Parent),
  PidStaticText = wxStaticText:new(ManuPanel, ?wxID_ANY, "Pid:"),
  PidTextCtrl = wxTextCtrl:new(ManuPanel, ?PID_TEXT, [{style,?wxBOTTOM}]),
  ref_add(?PID_TEXT, PidTextCtrl),

  ForwardStaticText  = wxStaticText:new(ManuPanel, ?wxID_ANY, "Available forward rules: "),
  BackwardStaticText = wxStaticText:new(ManuPanel, ?wxID_ANY, "Available backward rules: "),

  ForwardButtons = setupRuleButtons(ManuPanel, ?FORW_SEQ_BUTTON, ?FORW_SCHED_BUTTON),
  BackwardButtons = setupRuleButtons(ManuPanel, ?BACK_SEQ_BUTTON, ?BACK_SCHED_BUTTON),

  ManuSizer = wxBoxSizer:new(?wxVERTICAL),
  ProcSizer = wxBoxSizer:new(?wxHORIZONTAL),
  ButtonSizer = wxBoxSizer:new(?wxVERTICAL),
  BorderSizer = wxBoxSizer:new(?wxVERTICAL),

  wxSizer:add(ManuSizer, ProcSizer),
  wxSizer:addSpacer(ManuSizer, 15),
  wxSizer:add(ManuSizer, ButtonSizer),

  wxSizer:add(ProcSizer, PidStaticText, [{flag,?wxCENTRE}]),
  wxSizer:add(ProcSizer, PidTextCtrl, [{flag,?wxCENTRE}]),
  
  wxSizer:add(ButtonSizer, ForwardStaticText),
  addButtonsToSizer(ButtonSizer, ForwardButtons),
  wxSizer:addSpacer(ButtonSizer, 15),
  wxSizer:add(ButtonSizer, BackwardStaticText),
  addButtonsToSizer(ButtonSizer, BackwardButtons),

  wxSizer:add(BorderSizer, ManuSizer, [{flag, ?wxALL}, {border, 10}]),
  wxWindow:setSizer(ManuPanel, BorderSizer),
  ManuPanel.

% setupSemiPanel(Parent) ->
%   SemiPanel = wxPanel:new(Parent),
%   SemiPanel.

setupAutoPanel(Parent) ->
  AutoPanel = wxPanel:new(Parent),
  StepStaticText = wxStaticText:new(AutoPanel, ?wxID_ANY, "Steps:"),
  StepTextCtrl = wxTextCtrl:new(AutoPanel, ?PID_TEXT, [{style,?wxBOTTOM}]),
  ref_add(?STEP_TEXT, StepTextCtrl),
  HorizontalLine = wxStaticLine:new(AutoPanel, [{style, ?wxLI_HORIZONTAL},
                                                {size, {200, -1}}]),
  ForwardButton = wxButton:new(AutoPanel, ?FORWARD_BUTTON,
                               [{label, "Forward"}]),
  BackwardButton = wxButton:new(AutoPanel, ?BACKWARD_BUTTON,
                                [{label, "Backward"}]),
  NormalizeButton = wxButton:new(AutoPanel, ?NORMALIZE_BUTTON,
                                [{label, "Normalize"}]),
  wxButton:disable(ForwardButton),
  wxButton:disable(BackwardButton),
  wxButton:disable(NormalizeButton),
  ref_add(?FORWARD_BUTTON, ForwardButton),
  ref_add(?BACKWARD_BUTTON, BackwardButton),
  ref_add(?NORMALIZE_BUTTON, NormalizeButton),

  AutoSizer = wxBoxSizer:new(?wxVERTICAL),
  StepSizer = wxBoxSizer:new(?wxHORIZONTAL),
  StepButtonSizer = wxBoxSizer:new(?wxHORIZONTAL),
  SchedButtonSizer = wxBoxSizer:new(?wxHORIZONTAL),
  BorderSizer = wxBoxSizer:new(?wxVERTICAL),

  wxSizer:add(AutoSizer, StepSizer, [{flag, ?wxALIGN_CENTER_HORIZONTAL}]),
  wxSizer:addSpacer(AutoSizer, 15),
  wxSizer:add(AutoSizer, StepButtonSizer, [{flag, ?wxALIGN_CENTER_HORIZONTAL}]),
  wxSizer:add(AutoSizer, HorizontalLine, [{flag, ?wxTOP bor ?wxBOTTOM},
                                          {border, 15}]),
  wxSizer:add(AutoSizer, SchedButtonSizer, [{flag, ?wxALIGN_CENTER_HORIZONTAL}]),

  wxSizer:add(StepSizer, StepStaticText),
  wxSizer:add(StepSizer, StepTextCtrl),

  wxSizer:add(StepButtonSizer, ForwardButton),
  wxSizer:addSpacer(StepButtonSizer, 5),
  wxSizer:add(StepButtonSizer, BackwardButton),

  wxSizer:add(SchedButtonSizer, NormalizeButton),

  wxSizer:add(BorderSizer, AutoSizer, [{flag, ?wxALL bor ?wxALIGN_CENTER_HORIZONTAL}, {border, 10}]),
  wxWindow:setSizer(AutoPanel, BorderSizer),
  AutoPanel.

addButtonsToSizer(Sizer,Buttons) ->
  FirstRowSizer = wxBoxSizer:new(?wxHORIZONTAL),
  SecondRowSizer = wxBoxSizer:new(?wxHORIZONTAL),
  LastRowSizer = wxBoxSizer:new(?wxHORIZONTAL),

  FirstRowButtons = lists:sublist(Buttons,1,3),
  SecondRowButtons = lists:sublist(Buttons,4,3),
  LastRowButtons = lists:sublist(Buttons,7,3),

  [wxSizer:add(FirstRowSizer,Button) || Button <- FirstRowButtons],
  [wxSizer:add(SecondRowSizer,Button) || Button <- SecondRowButtons],
  [wxSizer:add(LastRowSizer,Button) || Button <- LastRowButtons],

  wxSizer:add(Sizer,FirstRowSizer),
  wxSizer:add(Sizer,SecondRowSizer),
  wxSizer:add(Sizer,LastRowSizer).

setupRuleButtons(Parent,First,Last) ->
  Refs = lists:seq(First,Last),
  RuleButtons = [wxButton:new(Parent, Ref,
                [{label, utils_gui:get_button_label(Ref)}]) || Ref <- Refs],
  RuleRefPairs = lists:zip(RuleButtons,Refs),
  [ref_add(Ref, Button) || {Button, Ref} <- RuleRefPairs],
  utils_gui:disable_rule_buttons(Refs),
  RuleButtons.

setupMenu() ->
  MenuBar = wxMenuBar:new(),
  File = wxMenu:new(),
  Help = wxMenu:new(),
  wxMenuBar:append(MenuBar,File,"&File"),
  wxMenuBar:append(MenuBar,Help,"&Help"),
  wxMenu:append(File,?OPEN,"Open\tCtrl-O"),
  wxMenu:append(File,?EXIT,"Quit\tCtrl-Q"),
  wxMenu:append(Help,?ABOUT,"About"),
  Frame = ref_lookup(?FRAME),
  wxFrame:setMenuBar(Frame,MenuBar).

loadFile(File) ->
  Frame = ref_lookup(?FRAME),
  case compile:file(File,[to_core,binary]) of
    {ok,_,CoreForms} ->
      Stripper = fun(Tree) -> cerl:set_ann(Tree, []) end,
      CleanCoreForms = cerl_trees:map(Stripper,CoreForms),
      FunDefs = cerl:module_defs(CleanCoreForms),
      CodeText = ref_lookup(?CODE_TEXT),
      wxTextCtrl:setValue(CodeText,core_pp:format(CleanCoreForms)),
      % update status
      ref_add(?STATUS,#status{loaded = {true,FunDefs}}),
      utils_gui:set_choices(utils:moduleNames(CleanCoreForms)),
      StartButton = ref_lookup(?START_BUTTON),
      wxButton:enable(StartButton),
      % TODO: Improve this status text
      wxFrame:setStatusText(Frame,"Loaded!");
    _Other ->
      % TODO: Improve this status text
      wxFrame:setStatusText(Frame,"Error when loading file")
  end.

openDialog(Parent) ->
  Caption = "Select an Erlang file",
  Wildcard = "Erlang source|*.erl| All files|*",
  DefaultDir = ref_lookup(?FILE_PATH),
  DefaultFile = "",
  Dialog = wxFileDialog:new(Parent, [{message, Caption},
                                     {defaultDir, DefaultDir},
                                     {defaultFile, DefaultFile},
                                     {wildCard, Wildcard},
                                     {style, ?wxFD_OPEN bor
                                          ?wxFD_FILE_MUST_EXIST}]),
  case wxDialog:showModal(Dialog) of
      ?wxID_OK ->
        File = wxFileDialog:getPaths(Dialog),
        loadFile(File);
      _Other -> continue
  end,
  wxDialog:destroy(Dialog).

init_system(Fun,Args) ->
  Proc = #proc{pid = cerl:c_int(1),
               exp = cerl:c_apply(Fun,Args)},
  Procs = [Proc],
  System = #sys{procs = Procs},
  ref_add(?SYSTEM, System),
  Status = ref_lookup(?STATUS),
  NewStatus = Status#status{running = true},
  ref_add(?STATUS, NewStatus).

start(Fun,Args) ->
  Status = ref_lookup(?STATUS),
  #status{loaded = {true,FunDefs}} = Status,
  utils_gui:stop_servers(),
  rev_erlang:start_servers(FunDefs),
  init_system(Fun,Args),
  refresh(),
  LeftNotebook = ref_lookup(?LEFT_NOTEBOOK),
  wxNotebook:setSelection(LeftNotebook, ?PAGEPOS_STATE),
  % TODO: Improve this status text with fun and args
  utils_gui:update_status_text("Started!").

refresh_buttons(Options) ->
  PidTextCtrl = ref_lookup(?PID_TEXT),
  PidText = wxTextCtrl:getValue(PidTextCtrl),
  ManualButtons = lists:seq(?FORW_SEQ_BUTTON, ?BACK_SCHED_BUTTON),
  case string:to_integer(PidText) of
    {error, _} ->
      utils_gui:disable_rule_buttons(ManualButtons);
    {PidInt, _} ->
      PidCerl = cerl:c_int(PidInt),
      FiltOpts = utils:filter_options(Options,PidCerl),
      FiltButtons = lists:map(fun utils_gui:option_to_button/1, FiltOpts),

      [utils_gui:set_button_if(Button, FiltButtons) ||
                               Button <- ManualButtons]
  end,
  HasFwdOptions = utils:has_fwd(Options),
  HasBwdOptions = utils:has_bwd(Options),
  utils_gui:set_fwd_button_if(HasFwdOptions),
  utils_gui:set_bwd_button_if(HasBwdOptions).

refresh() ->
  case utils_gui:is_app_running() of
    false -> ok;
    true ->
      System = ref_lookup(?SYSTEM),
      Options = rev_erlang:eval_opts(System),
      refresh_buttons(Options),
      StateText = ref_lookup(?STATE_TEXT),
      wxTextCtrl:setValue(StateText,utils:pp_system(System))
  end.

start() ->
  InputTextCtrl = ref_lookup(?INPUT_TEXT),
  InputText = wxTextCtrl:getValue(InputTextCtrl),
  FunChoice = ref_lookup(?FUN_CHOICE),
  NumChoice = wxChoice:getSelection(FunChoice),
  StringChoice = wxChoice:getString(FunChoice,NumChoice),
  Fun = utils:stringToFunName(StringChoice),
  Args = utils:stringToCoreArgs(InputText),
  ?LOG("start fun " ++ StringChoice ++" with args " ++ InputText),
  start(Fun,Args).

exec_with(Button) ->
  System = ref_lookup(?SYSTEM),
  PidTextCtrl = ref_lookup(?PID_TEXT),
  PidText = wxTextCtrl:getValue(PidTextCtrl),
  case string:to_integer(PidText) of
    {error, _} ->
      ok;
    {PidInt, _} ->
      PidCerl = cerl:c_int(PidInt),
      PartOption = utils_gui:button_to_option(Button),
      Option = PartOption#opt{id = PidCerl},
      NewSystem = rev_erlang:eval_step(System, Option),
      ref_add(?SYSTEM, NewSystem)
      % TODO: What should we say in the status text?
      % update_status_text("")
  end.

eval_mult(Button) ->
  System = ref_lookup(?SYSTEM),
  StepTextCtrl = ref_lookup(?STEP_TEXT),
  StepText = wxTextCtrl:getValue(StepTextCtrl),
  case string:to_integer(StepText) of
    {error, _} ->
      ok;
    {Steps, _} ->
      Option =
        case Button of
          ?FORWARD_BUTTON -> ?MULT_FWD;
          ?BACKWARD_BUTTON -> ?MULT_BWD
        end,
      {NewSystem, _StepsDone} = rev_erlang:eval_mult(System, Option, Steps),
      ref_add(?SYSTEM, NewSystem)
      % TODO: What should we say in the status text?
      % DoneSteps/Steps
  end.

eval_norm() ->
  System = ref_lookup(?SYSTEM),
  NewSystem = rev_erlang:eval_norm(System),
  ref_add(?SYSTEM, NewSystem).

loop() ->
    receive
        %% ------------------- Button handlers ------------------- %%
        #wx{id = ?START_BUTTON, event = #wxCommand{type = command_button_clicked}} ->
          start(),
          loop();
        #wx{id = ?NORMALIZE_BUTTON, event = #wxCommand{type = command_button_clicked}} ->
          eval_norm(),
          refresh(),
          loop();
        #wx{id = RuleButton, event = #wxCommand{type = command_button_clicked}}
          when (RuleButton >= ?FORW_SEQ_BUTTON) and (RuleButton =< ?BACK_SCHED_BUTTON) ->
          exec_with(RuleButton),
          refresh(),
          loop();
        #wx{id = RuleButton, event = #wxCommand{type = command_button_clicked}}
          when (RuleButton == ?FORWARD_BUTTON) or (RuleButton == ?BACKWARD_BUTTON) ->
          eval_mult(RuleButton),
          refresh(),
          loop();
        %% -------------------- Text handlers -------------------- %%
        #wx{id = ?PID_TEXT, event = #wxCommand{type = command_text_updated}} ->
          refresh(),
          loop();
        #wx{id = ?STEP_TEXT, event = #wxCommand{type = command_text_updated}} ->
          refresh(),
          loop();
        #wx{id = _RestIds, event = #wxCommand{type = command_text_updated}} ->
          loop();
        %% -------------------- Menu handlers -------------------- %%
        #wx{id = ?OPEN, event = #wxCommand{type = command_menu_selected}} ->
          Frame = ref_lookup(?FRAME),
          openDialog(Frame),
          loop();
        #wx{id = ?EXIT, event = #wxCommand{type = command_menu_selected}} ->
          Frame = ref_lookup(?FRAME),
          wxFrame:destroy(Frame);
        %% ------------------- Other handlers -------------------- %%
        #wx{event = #wxClose{type = close_window}} ->
          Frame = ref_lookup(?FRAME),
          wxFrame:destroy(Frame);
        %% ---------------- Non-supported events ----------------- %%
        Other ->
          io:format("main loop does not implement ~p~n", [Other]),
          loop()
    end.

ref_add(Id, Ref) ->
    ets:insert(?GUI_REF, {Id, Ref}).

ref_lookup(Id) ->
    ets:lookup_element(?GUI_REF, Id, 2).

ref_start() ->
    ?GUI_REF = ets:new(?GUI_REF, [set, public, named_table]),
    ok.

ref_stop() ->
    ets:delete(?GUI_REF).
