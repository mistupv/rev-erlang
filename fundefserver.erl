-module(fundefserver).
-export([start/1]).

start(FunDefs) ->
  loop(FunDefs).

loop(FunDefs) ->
  receive
    {Pid,{FunName,FunArity}} ->
      FunBody = findBody(FunName,FunArity,FunDefs),
      Pid ! FunBody,
      loop(FunDefs)
  end.

findBody(FunName,FunArity,FunDefs) ->
  hd[Body || {Name,Body} <- FunDefs, cerl:var_name(Name) == {FunName,FunArity}].
