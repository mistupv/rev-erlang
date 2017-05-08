-module(rev_erlang_gui).
-export([setup_gui/0]).

-include("rev_erlang.hrl").
-include("rev_erlang_gui.hrl").
-include_lib("wx/include/wx.hrl").

setup_gui() ->
  Server = wx:new(),
  Frame = wxFrame:new(Server, -1, ?APP_STRING,[{size,?FRAME_SIZE_INIT}]),
  ref_start(),
  ref_add(?FRAME, Frame),
  setupMenu(),
  wxFrame:createStatusBar(Frame, [{id, ?STATUS_BAR}]),
  wxEvtHandler:connect(Frame, close_window),
  wxEvtHandler:connect(Frame, command_button_clicked),
  wxEvtHandler:connect(Frame, command_checkbox_clicked),
  wxEvtHandler:connect(Frame, command_menu_selected),
  %wxEvtHandler:connect(Frame, command_text_updated),
  Panel = wxPanel:new(Frame),
  LeftColumnSizer = setupLeftColumnSizer(Panel),
  RightColumnSizer = setupRightColumnSizer(Panel),
  MainSizer = wxBoxSizer:new(?wxHORIZONTAL),
  wxSizer:add(MainSizer, LeftColumnSizer,
              [{flag,?wxLEFT bor ?wxRIGHT bor ?wxTOP},{border,25}]),
  wxSizer:add(MainSizer, RightColumnSizer),
  wxPanel:setSizer(Panel, MainSizer),
  wxFrame:show(Frame),
  loop().

setupLeftColumnSizer(Parent) ->
  StateText = wxTextCtrl:new(Parent, 1001,
                             [{style,?wxTE_MULTILINE bor ?wxTE_READONLY},
                              {size,{460,460}}]),
  InputStaticText = wxStaticText:new(Parent, ?wxID_ANY, "Input args:"),
  InputTextCtrl = wxTextCtrl:new(Parent, 1000,
                                 [{style, ?wxBOTTOM}]),
  StartButton = wxButton:new(Parent, ?wxID_ANY,
                             [{label, "START"}, {size, {60, -1}}]),
  wxButton:disable(StartButton),

  StateSizer = wxBoxSizer:new(?wxVERTICAL),
  InputSizer = wxBoxSizer:new(?wxHORIZONTAL),

  wxSizer:add(StateSizer, StateText),
  wxSizer:addSpacer(StateSizer,10),
  wxSizer:add(StateSizer, InputSizer),

  wxSizer:add(InputSizer, InputStaticText),
  wxSizer:add(InputSizer, InputTextCtrl),
  wxSizer:addSpacer(InputSizer, 10),
  wxSizer:add(InputSizer, StartButton, [{flag,?wxALIGN_RIGHT}]),
  StateSizer.

setupRightColumnSizer(Parent) ->
  PidStaticText = wxStaticText:new(Parent,?wxID_ANY,"Pid:"),
  PidTextCtrl = wxTextCtrl:new(Parent, 1001,[{style,?wxBOTTOM}]),
  % ForwardCheckBox = wxCheckBox:new(Parent,?wxID_ANY,"Show forward rules"),
  % BackwardCheckBox = wxCheckBox:new(Parent,?wxID_ANY,"Show backward rules"),
  % wxCheckBox:setValue(ForwardCheckBox,true),
  % wxCheckBox:setValue(BackwardCheckBox,true),

  RandButton = wxButton:new(Parent, ?RAND_BUTTON, [{label,getButtonLabel(?RAND_BUTTON)}]),
  ForwRandButton = wxButton:new(Parent, ?FORW_RAND_BUTTON, [{label,getButtonLabel(?FORW_RAND_BUTTON)}]),
  BackRandButton = wxButton:new(Parent, ?BACK_RAND_BUTTON, [{label,getButtonLabel(?BACK_RAND_BUTTON)}]),

  ForwardButtons = setupRuleButtons(Parent, ?FORW_SEQ_BUTTON, ?FORW_SCHED_BUTTON),
  BackwardButtons = setupRuleButtons(Parent,?BACK_SEQ_BUTTON, ?BACK_SCHED_BUTTON),

  CtrlsSizer = wxBoxSizer:new(?wxVERTICAL),
  ProcInfoSizer = wxBoxSizer:new(?wxHORIZONTAL),
  RuleInfoSizer = wxBoxSizer:new(?wxVERTICAL),
  RuleButtonSizer = wxBoxSizer:new(?wxVERTICAL),
  RandButtonSizer = wxBoxSizer:new(?wxHORIZONTAL),

  wxSizer:addSpacer(CtrlsSizer,25),
  wxSizer:add(CtrlsSizer, ProcInfoSizer),
  % wxSizer:addSpacer(CtrlsSizer,15),
  % wxSizer:add(CtrlsSizer, RuleInfoSizer),
  wxSizer:addSpacer(CtrlsSizer,15),
  wxSizer:add(CtrlsSizer, RuleButtonSizer),

  wxSizer:add(ProcInfoSizer,PidStaticText, [{flag,?wxCENTRE}]),
  wxSizer:add(ProcInfoSizer,PidTextCtrl, [{flag,?wxCENTRE}]),

  % wxSizer:add(RuleInfoSizer,ForwardCheckBox),
  % wxSizer:add(RuleInfoSizer,BackwardCheckBox),
  
  wxSizer:add(RuleButtonSizer,RandButtonSizer),
  wxSizer:add(RandButtonSizer,RandButton),
  wxSizer:add(RandButtonSizer,ForwRandButton),
  wxSizer:add(RandButtonSizer,BackRandButton),
  wxSizer:addSpacer(RuleButtonSizer,15),
  addButtonsToSizer(RuleButtonSizer,ForwardButtons),
  wxSizer:addSpacer(RuleButtonSizer,15),
  addButtonsToSizer(RuleButtonSizer,BackwardButtons),
  CtrlsSizer.

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

getButtonLabel(Button) ->
  case Button of
    ?FORW_SEQ_BUTTON -> "Seq";
    ?FORW_CHECK_BUTTON -> "Check";
    ?FORW_SEND_BUTTON -> "Send";
    ?FORW_RECEIVE_BUTTON -> "Receive";
    ?FORW_SPAWN_BUTTON -> "Spawn";
    ?FORW_SELF_BUTTON -> "Self";
    ?FORW_SCHED_BUTTON -> "Sched";
    ?BACK_SEQ_BUTTON -> "Seq";
    ?BACK_CHECK_BUTTON -> "Check";
    ?BACK_SEND_BUTTON -> "Send";
    ?BACK_RECEIVE_BUTTON -> "Receive";
    ?BACK_SPAWN_BUTTON -> "Spawn";
    ?BACK_SELF_BUTTON -> "Self";
    ?BACK_SCHED_BUTTON -> "Sched";
    ?RAND_BUTTON -> "Random";
    ?FORW_RAND_BUTTON -> "Forward";
    ?BACK_RAND_BUTTON -> "Backward"
  end.

setupRuleButtons(Parent,First,Last) ->
  [wxButton:new(Parent, Button,
                [{label,getButtonLabel(Button)}]) || Button <- lists:seq(First,Last)].

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

ref_add(Id, Ref) ->
    ets:insert(?NT_REF, {Id, Ref}).

ref_lookup(Id) ->
    ets:lookup_element(?NT_REF, Id, 2).

ref_start() ->
    ?NT_REF = ets:new(?NT_REF, [set, public, named_table]),
    ok.

ref_stop() ->
    ets:delete(?NT_REF).

loop() ->
    receive
        %% -------------------- Menu handlers -------------------- %%
        #wx{id = ?EXIT, event = #wxCommand{type = command_menu_selected}} ->
            ok;
        Other ->
            io:format("main loop does not implement ~p~n", [Other]),
            loop()
    end.
