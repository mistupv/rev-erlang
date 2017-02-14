-module(interp).
-export([start/2]).

%-record(proc_parts, { pid,
%                      theta,
%                      exp,
%                      mailbox }).
                      
  start(ModuleFile, {Fun,Args}) ->
  {ok,ModuleName,CoreForms} = compile:file(ModuleFile,[to_core,binary]),
  Stripper = fun(Tree) -> cerl:set_ann(Tree, []) end, 
  Gamma = [],
  Procs = [{1,{[],{c_apply,[],{c_var,[],Fun}}},Args}],
  System = {Gamma,Procs},
  io:fwrite("~p~n",[cerl_trees:map(Stripper,CoreForms)]),
  Procs.
  