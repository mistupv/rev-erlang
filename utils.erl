-module(utils).
-export([select_proc/2]).

select_proc(Procs,Pid) ->
  [Proc] = [{P,S,M} || {P,S,M} <- Procs, P == Pid],
  RestProcs = [{P,S,M} || {P,S,M} <- Procs, P /= Pid],
  {Proc,RestProcs}.
