-module(freshserver).
-export([start/0]).

start() ->
  loop(2).

loop(NewPid) ->
  receive
    {Pid,new_pid} ->
      Pid ! NewPid,
      loop(NewPid + 1);
    terminate -> ok
  end.
