-module(utils).
-export([select_proc/2,zip_core/2,pp_system/1]).

-include("rev_erlang.hrl").

select_proc(Procs,Pid) ->
  [Proc] = [ P || P <- Procs, P#proc.pid == Pid],
  RestProcs = [ P || P <- Procs, P#proc.pid, P /= Pid],
  {Proc,RestProcs}.

% TODO: Improve zip_core
% zip_core([],{c_literal,_,[]}) ->
%   [];
% zip_core([E|Es],{c_cons,_Ann,Head,Rest}) ->
%   [{E,Head}|zip_core(Es,Rest)].

% pp_system(#sys{msgs = Msgs, procs = Procs}) ->
%   pp_msgs(Msgs) ++ ";" ++ pp_procs(Procs).

% pp_msgs([]) -> "[]";
% pp_msgs(Msgs) -> "[" ++ string:join([pp_msg(X) || X <- Msgs],",") ++ "]".

% pp_procs(_Procs) -> "".

% pp_msg(#msg{src = SrcPid, dest = DestPid, val= MsgValue}) ->
%   "{" ++ pp(SrcPid) ++ "," ++ pp(DestPid) ++ "," ++ pp(MsgValue) ++ "}".
% %    io:fwrite(" ARBRE ~s~n",[lists:flatten()]),
% % TODO: Ask Tama about this
% pp(X) -> core_pp:format(X).