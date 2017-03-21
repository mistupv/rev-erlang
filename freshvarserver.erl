-module(freshvarserver).
-export([start/0]).

start() ->
  loop(1).

loop(FreshNum) ->
  receive
    {Pid,new_var} ->
      NewVar = build_var(FreshNum),
      Pid ! NewVar,
      loop(FreshNum + 1);
    terminate -> ok
  end.

build_var(Num) ->
  NumAtom = list_to_atom("y_" ++ integer_to_list(Num)),
  cerl:c_var(NumAtom).