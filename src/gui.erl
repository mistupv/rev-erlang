-module(gui).
-export([setup/0]).

-include("rev_erlang.hrl").
-include_lib("wx/include/wx.hrl").

setup() ->
  Server = wx:new(),
  Frame = wxFrame:new(Server, -1, "Reversible Erlang",[{size,{800,600}}]),
  Panel = wxPanel:new(Frame),

  % widgets
  ButtonBackward = wxButton:new(Panel, ?ID_BACKWARD_STEP, [{label,"backward"}]),
  ButtonForward = wxButton:new(Panel, ?ID_FORWARD_STEP, [{label,"forward"}]),


  % sizers
  MainSizer = wxBoxSizer:new(?wxHORIZONTAL),
  StateSizer = wxBoxSizer:new(?wxVERTICAL),
  CtrlsSizer = wxBoxSizer:new(?wxVERTICAL),
  ProcInfoSizer = wxBoxSizer:new(?wxHORIZONTAL),
  RuleInfoSizer = wxBoxSizer:new(?wxHORIZONTAL),

  PidStaticText = wxStaticText:new(Panel,?wxID_ANY,"Pid: "),
  PidTextCtrl = wxTextCtrl:new(Panel, 1001,[{style,?wxBOTTOM}]),

  wxSizer:add(ProcInfoSizer,PidStaticText),
  wxSizer:add(ProcInfoSizer,PidTextCtrl),

  StateText = wxTextCtrl:new(Panel, 1001, [{style,?wxTE_MULTILINE bor ?wxTE_READONLY},
                                {pos, {30,30}},
                                   {size,{400,160}}%{size, ?wxDefaultSize},
  ]),
  wxSizer:add(StateSizer, StateText, []),
  % SzFlags = [{proportion, 0}, {border, 4}, {flag, ?wxALL}],

  % wxButton:connect(ButtonForward, command_button_clicked, []),
  % wxButton:connect(ButtonBackward, command_button_clicked, []),
  
  wxSizer:add(RuleInfoSizer, ButtonForward),
  wxSizer:add(RuleInfoSizer, ButtonBackward),


  wxSizer:add(MainSizer, StateSizer, [{proportion,0},{flag,?wxALL},{border,25}]),
  wxSizer:add(MainSizer, CtrlsSizer, [{proportion,0},{flag,?wxTOP},{border,25}]),
  wxSizer:add(CtrlsSizer, ProcInfoSizer, [{proportion,0}]),
  wxSizer:add(CtrlsSizer, RuleInfoSizer),
  wxPanel:setSizer(Panel, MainSizer),

  wxFrame:show(Frame).