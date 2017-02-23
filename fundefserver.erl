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
  
  FunDefList = [FunD || {FunN, FunD} <- FunDefs, FunN == FunName],%, cerl:var_name(FunDef)],
  %io:fwrite("~p~n",[FunDefList])
  hd(FunDefList)
  % case length(FunDefList) of
  %   1 -> hd(FunDefList);
  %   _Other -> nothing
  % end.
.
