-module(fundefserver).
-export([start/1]).

start(FunDefs) ->
  loop(FunDefs).

loop(FunDefs) ->
  receive
    {Pid,FunName} ->
      {_,FunDef} = lists:keyfind(FunName,1,FunDefs),%findDef(FunName,FunDefs),
      Pid ! FunDef,
      loop(FunDefs);
    terminate -> ok
  end.
