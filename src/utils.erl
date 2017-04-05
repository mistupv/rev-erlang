-module(utils).
-export([select_proc/2,zip_core/2,pp_system/1]).

select_proc(Procs,Pid) ->
  [Proc] = [Item || Item = {P,_,_,_} <- Procs, P == Pid],
  RestProcs = [Item || Item = {P,_,_,_} <- Procs, P /= Pid],
  {Proc,RestProcs}.

zip_core([],{c_literal,_,[]}) ->
  [];
zip_core([E|Es],{c_cons,_Ann,Head,Rest}) ->
  [{E,Head}|zip_core(Es,Rest)].

pp_system({Gamma,Procs}) ->
  pp_gamma(Gamma) ++ ";" ++ pp_procs(Procs).

pp_gamma([]) -> "[]";
pp_gamma(Gamma) -> "[" ++ string:join([pp_msg(X) || X <- Gamma],",") ++ "]".

pp_procs(Procs) -> "".

pp_msg({SrcPid,DestPid,MsgValue}) ->
  "{" ++ pp(SrcPid) ++ "," ++ pp(DestPid) ++ "," ++ pp(MsgValue) ++ "}".

% TODO: Ask Tama about this
pp(X) -> "".%erl_prettypr:format(X).