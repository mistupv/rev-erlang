-module(schedserver).
-export([start/0]).

-include("rev_erlang.hrl").

start() ->
  loop().

loop() ->
  receive
    {SenderPid,Semantics,#sys{msgs = Msgs, procs = Procs}} ->
      Pids = [?ID_GAMMA] ++ [Pid || #proc{pid = Pid} <- Procs],
      RandPid = randPid(Semantics,Pids,#sys{msgs = Msgs, procs = Procs}),
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