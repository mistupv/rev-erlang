
-define(APP_STRING, "Reversible Erlang").

-define(ID_GAMMA,0).

-define(FWD_SEM, fwd_sem).
-define(BWD_SEM, bwd_sem).

-define(TYPE_MSG, msg).
-define(TYPE_PROC, proc).

-define(RULE_SEQ, seq).
-define(RULE_CHECK, check).
-define(RULE_SEND, send).
-define(RULE_RECEIVE, 'receive').
-define(RULE_SPAWN, spawn).
-define(RULE_SELF, self).
-define(RULE_SCHED, sched).

% ets defs
-define(FILE_PATH, 508).
-define(GUI_REF, '_._gui').

-define(MULT_FWD, mult_fwd).
-define(MULT_BWD, mult_bwd).

% TODO: Add types
-record(proc, {pid,
               hist = [],
               env  = [],
               exp,
               mail = []}).

% TODO: Add types
-record(msg, {time,
              src,
              dest,
              val}).

% TODO: Add types
-record(sys, {msgs  = [],
              procs = []}).

-record(opt, {sem, % forward or backward
              type,  % proc or msg
              id, % integer
              rule}). % seq, spawn, ...
