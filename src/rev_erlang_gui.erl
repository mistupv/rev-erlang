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
  Panel = wxPanel:new(Frame),
  LeftColumnSizer = setupLeftColumnSizer(Panel),
  RightColumnSizer = setupRightColumnSizer(Panel),
  MainSizer = wxBoxSizer:new(?wxHORIZONTAL),
  wxSizer:add(MainSizer, LeftColumnSizer,
              [{flag,?wxLEFT bor ?wxRIGHT bor ?wxTOP},{border,25}]),
  wxSizer:add(MainSizer, RightColumnSizer),
  wxPanel:setSizer(Panel, MainSizer),
  wxFrame:show(Frame).

setupLeftColumnSizer(Parent) ->
  StateText = wxTextCtrl:new(Parent, 1001,
                             [{style,?wxTE_MULTILINE bor ?wxTE_READONLY},
                              {size,{460,260}}]),
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
  ForwardCheckBox = wxCheckBox:new(Parent,?wxID_ANY,"Show forward rules"),
  BackwardCheckBox = wxCheckBox:new(Parent,?wxID_ANY,"Show backward rules"),
  wxCheckBox:setValue(ForwardCheckBox,true),
  wxCheckBox:setValue(BackwardCheckBox,true),
  ButtonBackward = wxButton:new(Parent, ?ID_BACKWARD_STEP, [{label,"backward"}]),
  ButtonForward = wxButton:new(Parent, ?ID_FORWARD_STEP, [{label,"forward"}]),

  CtrlsSizer = wxBoxSizer:new(?wxVERTICAL),
  ProcInfoSizer = wxBoxSizer:new(?wxHORIZONTAL),
  RuleInfoSizer = wxBoxSizer:new(?wxVERTICAL),
  RuleButtonSizer = wxBoxSizer:new(?wxHORIZONTAL),

  wxSizer:addSpacer(CtrlsSizer,25),
  wxSizer:add(CtrlsSizer, ProcInfoSizer),
  wxSizer:addSpacer(CtrlsSizer,15),
  wxSizer:add(CtrlsSizer, RuleInfoSizer),
  wxSizer:addSpacer(CtrlsSizer,15),
  wxSizer:add(CtrlsSizer, RuleButtonSizer),

  wxSizer:add(ProcInfoSizer,PidStaticText, [{flag,?wxCENTRE}]),
  wxSizer:add(ProcInfoSizer,PidTextCtrl, [{flag,?wxCENTRE}]),

  wxSizer:add(RuleInfoSizer,ForwardCheckBox),
  wxSizer:add(RuleInfoSizer,BackwardCheckBox),

  % wxButton:connect(ButtonForward, command_button_clicked, []),
  % wxButton:connect(ButtonBackward, command_button_clicked, []),
  
  wxSizer:add(RuleButtonSizer, ButtonForward),
  wxSizer:add(RuleButtonSizer, ButtonBackward),
  CtrlsSizer.

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
