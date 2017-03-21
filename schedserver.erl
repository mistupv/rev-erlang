-module(schedserver).
-export([start/0]).

start() ->
  loop().

loop() ->
  receive
    {Pid,System} ->
      RandPid = randPid(System),
      Pid ! RandPid,
      loop();
    terminate -> ok
  end.

% TODO: Make unique case and call randPid without unevaluable processes
randPid({Gamma,Procs}) ->
  case Gamma of
    [] ->
      Pids = [Pid || {Pid,_,_} <- Procs],
      PidsLen = length(Pids),
      RandPid = random:uniform(PidsLen);
    _Other ->
      Pids = [gamma] ++ [Pid || {Pid,_,_} <- Procs],
      GammaPidsLen = length(Pids),
      RandPid = random:uniform(GammaPidsLen)
  end,
  lists:nth(RandPid,Pids).
  