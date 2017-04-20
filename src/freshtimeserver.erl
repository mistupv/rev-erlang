-module(freshtimeserver).
-export([start/0]).

start() ->
  loop(1).

loop(NewTime) ->
  receive
    {Pid,new_time} ->
      Pid ! NewTime,
      loop(NewTime + 1);
    terminate -> ok
  end.
