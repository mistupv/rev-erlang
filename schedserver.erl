-module(schedserver).
-export([start/0]).

-define(ID_GAMMA,0).

start() ->
  loop().

loop() ->
  receive
    {SenderPid,Semantics,{Gamma,Procs}} ->
      Pids = [?ID_GAMMA] ++ [Pid || {Pid,_,_,_} <- Procs],
      RandPid = randPid(Semantics,Pids,{Gamma,Procs}),
      SenderPid ! RandPid,
      loop();
    terminate -> ok
  end.

randPid(_Semantics,[],_System) ->
  null_pid;
randPid(Semantics,Pids,System) ->
  PidsLen = length(Pids),
  RandElem = rand:uniform(PidsLen),
  RandPid = lists:nth(RandElem,Pids),
  case Semantics:can_eval(System,RandPid) of
    false ->
      RestPids = lists:delete(RandPid,Pids),
      randPid(Semantics,RestPids,System);
    true -> RandPid
  end.
