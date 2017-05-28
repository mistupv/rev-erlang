-module(rev_erlang).
-export([start/0,
         start_servers/1,stop_servers/0,
         eval_opts/1, eval_step/2]).

-include("rev_erlang.hrl").
                      
%start(ModuleFile,Fun,Args) ->
start() ->
  rev_erlang_gui:setup_gui(),
  ok.

start_servers(FunDefs) ->
  FunDefServer = spawn(fundefserver,start,[FunDefs]),
  case lists:member(fdserver,registered()) of
    true -> unregister(fdserver);
    false -> ok
  end,
  register(fdserver,FunDefServer),
  FreshPidServer = spawn(freshpidserver,start,[]),
  case lists:member(freshpidserver,registered()) of
    true -> unregister(freshpidserver);
    false -> ok
  end,
  register(freshpidserver,FreshPidServer),
  FreshTimeServer = spawn(freshtimeserver,start,[]),
  case lists:member(freshtimeserver,registered()) of
    true -> unregister(freshtimeserver);
    false -> ok
  end,
  register(freshtimeserver,FreshTimeServer),
  FreshVarServer = spawn(freshvarserver,start,[]),
  case lists:member(freshvarserver,registered()) of
    true -> unregister(freshvarserver);
    false -> ok
  end,
  register(freshvarserver,FreshVarServer).

stop_servers() ->
  fdserver ! terminate,
  freshpidserver ! terminate,
  freshtimeserver ! terminate,
  freshvarserver ! terminate.

eval_opts(System) ->  
  FwdOpts = fwd_sem:eval_opts(System),
  BwdOpts = bwd_sem:eval_opts(System),
  FwdOpts ++ BwdOpts.

eval_step(System, Option) ->
  #opt{sem = Semantics, type = Type, id = Id} = Option,
  NewSystem =
    case Type of
      ?TYPE_MSG -> Semantics:eval_sched(System,Id);
      ?TYPE_PROC -> Semantics:eval_step(System,Id)
    end,
  NewSystem.

% eval(System) ->
%   FwdOpts = fwd_sem:eval_opts(System),
%   BwdOpts = bwd_sem:eval_opts(System),
%   AllOpts = FwdOpts ++ BwdOpts,
%   StrOpts = string:join([utils:opt_to_str(Opt) || Opt <- AllOpts]," "),
%   io:fwrite("Available rules: ~s~n",[StrOpts]),
%   StrOpt =
%     case io:fread("Select rule: ","~s") of
%       {ok,[ReadStr]} -> ReadStr;
%       _ -> error
%     end,
%   {Semantics,Type,Id} = utils:str_to_opt(StrOpt),
%   NewSystem =
%     case Type of
%       sched -> Semantics:eval_sched(System,Id);
%       proc -> Semantics:eval_step(System,Id)
%     end,
%   io:fwrite("~s~n",[utils:pp_system(NewSystem)]),
%   eval(NewSystem).
