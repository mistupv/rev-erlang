-module(utils_gui).
-export([is_app_loaded/0, is_app_running/0,
         option_to_button_label/1, button_to_option/1,
         disable_rule_buttons/1, set_button_label_if/2, set_ref_button_if/2,
         set_choices/1, stop_refs/0, update_status_text/1,
         sttext_single/1, sttext_mult/2, sttext_norm/1]).

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

get_label_from_option(Option) ->
  case Option of
    #opt{rule = ?RULE_SEQ}     -> "Seq";
    #opt{rule = ?RULE_SEND}    -> "Send";
    #opt{rule = ?RULE_RECEIVE} -> "Receive";
    #opt{rule = ?RULE_SPAWN}   -> "Spawn";
    #opt{rule = ?RULE_SELF}    -> "Self";
    #opt{rule = ?RULE_SCHED}   -> ?NULL_LABEL
  end.

get_rule_from_button(Button) ->
  Label = wxButton:getLabel(ref_lookup(Button)),
  case Label of
     "Seq"     -> ?RULE_SEQ;
     "Send"    -> ?RULE_SEND;
     "Receive" -> ?RULE_RECEIVE;
     "Spawn"   -> ?RULE_SPAWN;
     "Self"    -> ?RULE_SELF
  end.

button_to_option(Button) ->
  case Button of
    ?FORW_INT_BUTTON ->
      Rule = get_rule_from_button(Button),
      #opt{sem = ?FWD_SEM, type = ?TYPE_PROC, rule = Rule};
    ?FORW_SCH_BUTTON ->
      #opt{sem = ?FWD_SEM, type = ?TYPE_MSG, rule = ?RULE_SCHED};
    ?BACK_INT_BUTTON ->
      Rule = get_rule_from_button(Button),
      #opt{sem = ?BWD_SEM, type = ?TYPE_PROC, rule = Rule};
    ?BACK_SCH_BUTTON ->
      #opt{sem = ?BWD_SEM, type = ?TYPE_MSG}
  end.

option_to_button_label(Option) ->
  #opt{sem = Sem, type = Type} = Option,
  Label = get_label_from_option(Option),
  Button =
    case Sem of
      ?FWD_SEM ->
        case Type of
          ?TYPE_MSG  -> ?FORW_SCH_BUTTON;
          ?TYPE_PROC -> ?FORW_INT_BUTTON
        end;
      ?BWD_SEM ->
        case Type of
          ?TYPE_MSG  -> ?BACK_SCH_BUTTON;
          ?TYPE_PROC -> ?BACK_INT_BUTTON
        end
    end,
  {Button, Label}.

disable_rule_buttons(Buttons) ->
  [wxButton:disable(ref_lookup(Button)) || Button <- Buttons].

set_button_label_if(Button, EnabledButtonLabels) ->
  RefButton = ref_lookup(Button),
  case lists:keyfind(Button, 1, EnabledButtonLabels) of
    false ->
      wxButton:disable(RefButton),
      case Button of
        ?FORW_INT_BUTTON -> wxButton:setLabel(RefButton, "Seq");
        ?BACK_INT_BUTTON -> wxButton:setLabel(RefButton, "Seq");
        _Other -> ok
      end;
    {Button, Label} ->
      wxButton:enable(RefButton),
      case Label of
        ?NULL_LABEL -> ok;
        Label -> wxButton:setLabel(RefButton, Label)
      end
  end.

set_ref_button_if(Ref, Cond) ->
  RefButton = ref_lookup(Ref),
  case Cond of
    true ->
      wxButton:enable(RefButton);
    false ->
      wxButton:disable(RefButton)
  end.

set_choices(Choices) ->
  FunChoice = ref_lookup(?FUN_CHOICE),
  wxChoice:clear(FunChoice),
  [wxChoice:append(FunChoice, Choice) || Choice <- Choices].

stop_refs() ->
  case is_app_running() of
    true ->
      rev_erlang:stop_refs(),
      ok;
    false -> ok
  end.

sttext_single(Button) ->
  Option = button_to_option(Button),
  #opt{sem = Sem} = Option,
  SemStr =
    case Sem of
      ?FWD_SEM -> " forward ";
      ?BWD_SEM -> " backward "
    end,
  LabelStr = get_label_from_option(Option),
  FullStr = "Fired" ++ SemStr ++ LabelStr ++ " rule",
  update_status_text(FullStr).

sttext_norm(Steps) ->
  StepsStr = integer_to_list(Steps),
  FullStr = StepsStr ++ " steps done",
  update_status_text(FullStr).

sttext_mult(StepsDone, Steps) ->
  StepsDoneStr = integer_to_list(StepsDone),
  StepsStr = integer_to_list(Steps),
  FullStr = StepsDoneStr ++ " of " ++ StepsStr ++ " steps done",
  update_status_text(FullStr).

update_status_text(String) ->
  Frame = ref_lookup(?FRAME),
  wxFrame:setStatusText(Frame, String).

ref_lookup(Id) ->
    ets:lookup_element(?GUI_REF, Id, 2).
