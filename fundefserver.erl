-module(fundefserver).
-export([start/1]).

start(FunDefs) ->
  loop(FunDefs).

loop(FunDefs) ->
  receive
    {Pid,FunName} ->
      FunDef = findDef(FunName,FunDefs),
      Pid ! FunDef,
      loop(FunDefs);
    terminate -> ok
  end.

findDef(FunName,FunDefs) ->
  FunDefList = [FunD || {FunN, FunD} <- FunDefs, FunN == FunName],
  hd(FunDefList).
