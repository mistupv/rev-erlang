-module(utils_gui).
-export([is_app_loaded/0, is_app_running/0,
         get_button_label/1, option_to_button/1, button_to_option/1,
         disable_rule_buttons/1, set_button_if/2, set_fwd_button_if/1,
         set_bwd_button_if/1,
         set_choices/1, stop_servers/0, update_status_text/1]).

-include("rev_erlang.hrl").
-include("rev_erlang_gui.hrl").
-include_lib("wx/include/wx.hrl").

is_app_loaded() ->
  Status = ref_lookup(?STATUS),
  #status{loaded = LoadedStatus} = Status,
  case LoadedStatus of
    {true, _} -> true;
    _Other -> false
  end.

is_app_running() ->
  Status = ref_lookup(?STATUS),
  #status{running = RunningStatus} = Status,
  RunningStatus.

get_button_label(Button) ->
  case Button of
    ?FORW_SEQ_BUTTON ->     "Seq";
    ?FORW_CHECK_BUTTON ->   "Check";
    ?FORW_SEND_BUTTON ->    "Send";
    ?FORW_RECEIVE_BUTTON -> "Receive";
    ?FORW_SPAWN_BUTTON ->   "Spawn";
    ?FORW_SELF_BUTTON ->    "Self";
    ?FORW_SCHED_BUTTON ->   "Sched";
    ?BACK_SEQ_BUTTON ->     "Seq";
    ?BACK_CHECK_BUTTON ->   "Check";
    ?BACK_SEND_BUTTON ->    "Send";
    ?BACK_RECEIVE_BUTTON -> "Receive";
    ?BACK_SPAWN_BUTTON ->   "Spawn";
    ?BACK_SELF_BUTTON ->    "Self";
    ?BACK_SCHED_BUTTON ->   "Sched"
  end.

button_to_option(Button) ->
  case Button of
    ?FORW_SEQ_BUTTON     -> #opt{sem = ?FWD_SEM, type = ?TYPE_PROC, rule = ?RULE_SEQ};
    ?FORW_CHECK_BUTTON   -> #opt{sem = ?FWD_SEM, type = ?TYPE_PROC, rule = ?RULE_CHECK};
    ?FORW_SEND_BUTTON    -> #opt{sem = ?FWD_SEM, type = ?TYPE_PROC, rule = ?RULE_SEND};
    ?FORW_RECEIVE_BUTTON -> #opt{sem = ?FWD_SEM, type = ?TYPE_PROC, rule = ?RULE_RECEIVE};
    ?FORW_SPAWN_BUTTON   -> #opt{sem = ?FWD_SEM, type = ?TYPE_PROC, rule = ?RULE_SPAWN};
    ?FORW_SELF_BUTTON    -> #opt{sem = ?FWD_SEM, type = ?TYPE_PROC, rule = ?RULE_SELF};
    ?FORW_SCHED_BUTTON   -> #opt{sem = ?FWD_SEM, type = ?TYPE_MSG,  rule = ?RULE_SCHED};
    ?BACK_SEQ_BUTTON     -> #opt{sem = ?BWD_SEM, type = ?TYPE_PROC, rule = ?RULE_SEQ};
    ?BACK_CHECK_BUTTON   -> #opt{sem = ?BWD_SEM, type = ?TYPE_PROC, rule = ?RULE_CHECK};
    ?BACK_SEND_BUTTON    -> #opt{sem = ?BWD_SEM, type = ?TYPE_PROC, rule = ?RULE_SEND};
    ?BACK_RECEIVE_BUTTON -> #opt{sem = ?BWD_SEM, type = ?TYPE_PROC, rule = ?RULE_RECEIVE};
    ?BACK_SPAWN_BUTTON   -> #opt{sem = ?BWD_SEM, type = ?TYPE_PROC, rule = ?RULE_SPAWN};
    ?BACK_SELF_BUTTON    -> #opt{sem = ?BWD_SEM, type = ?TYPE_PROC, rule = ?RULE_SELF};
    ?BACK_SCHED_BUTTON   -> #opt{sem = ?BWD_SEM, type = ?TYPE_MSG,  rule = ?RULE_SCHED}
  end.

option_to_button(Option) ->
  case Option of
    #opt{sem = ?FWD_SEM, rule = ?RULE_SEQ} ->     ?FORW_SEQ_BUTTON;
    #opt{sem = ?FWD_SEM, rule = ?RULE_CHECK} ->   ?FORW_CHECK_BUTTON;
    #opt{sem = ?FWD_SEM, rule = ?RULE_SEND} ->    ?FORW_SEND_BUTTON;
    #opt{sem = ?FWD_SEM, rule = ?RULE_RECEIVE} -> ?FORW_RECEIVE_BUTTON;
    #opt{sem = ?FWD_SEM, rule = ?RULE_SPAWN} ->   ?FORW_SPAWN_BUTTON;
    #opt{sem = ?FWD_SEM, rule = ?RULE_SELF} ->    ?FORW_SELF_BUTTON;
    #opt{sem = ?FWD_SEM, rule = ?RULE_SCHED} ->   ?FORW_SCHED_BUTTON;
    #opt{sem = ?BWD_SEM, rule = ?RULE_SEQ} ->     ?BACK_SEQ_BUTTON;
    #opt{sem = ?BWD_SEM, rule = ?RULE_CHECK} ->   ?BACK_CHECK_BUTTON;
    #opt{sem = ?BWD_SEM, rule = ?RULE_SEND} ->    ?BACK_SEND_BUTTON;
    #opt{sem = ?BWD_SEM, rule = ?RULE_RECEIVE} -> ?BACK_RECEIVE_BUTTON;
    #opt{sem = ?BWD_SEM, rule = ?RULE_SPAWN} ->   ?BACK_SPAWN_BUTTON;
    #opt{sem = ?BWD_SEM, rule = ?RULE_SELF} ->    ?BACK_SELF_BUTTON;
    #opt{sem = ?BWD_SEM, rule = ?RULE_SCHED} ->   ?BACK_SCHED_BUTTON
  end.

disable_rule_buttons(Buttons) ->
  [wxButton:disable(ref_lookup(Button)) || Button <- Buttons].

set_button_if(Button, EnabledButtons) ->
  case lists:member(Button, EnabledButtons) of
    true -> wxButton:enable(ref_lookup(Button));
    false -> wxButton:disable(ref_lookup(Button))
  end.

set_fwd_button_if(Cond) ->
  FwdRef = ref_lookup(?FORWARD_BUTTON),
  NormRef = ref_lookup(?NORMALIZE_BUTTON),
  case Cond of
    true ->
      wxButton:enable(FwdRef),
      wxButton:enable(NormRef);
    false ->
      wxButton:disable(FwdRef),
      wxButton:disable(NormRef)
  end.

set_bwd_button_if(Cond) ->
  BwdRef = ref_lookup(?BACKWARD_BUTTON),
  case Cond of
    true -> wxButton:enable(BwdRef);
    false -> wxButton:disable(BwdRef)
  end.

set_choices(Choices) ->
  FunChoice = ref_lookup(?FUN_CHOICE),
  wxChoice:clear(FunChoice),
  [wxChoice:append(FunChoice, Choice) || Choice <- Choices].

stop_servers() ->
  case is_app_running() of
    true ->
      rev_erlang:stop_servers(),
      ok;
    false -> ok
  end.

update_status_text(String) ->
  Frame = ref_lookup(?FRAME),
  wxFrame:setStatusText(Frame, String).

ref_lookup(Id) ->
    ets:lookup_element(?GUI_REF, Id, 2).