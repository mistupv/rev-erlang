-module(utils).
-export([select_proc/2]).

select_proc(Procs,Pid) ->
  [Proc] = [Item || Item = {P,_,_} <- Procs, P == Pid],
  RestProcs = [Item || Item = {P,_,_} <- Procs, P /= Pid],
  {Proc,RestProcs}.

