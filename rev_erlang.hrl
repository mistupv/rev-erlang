
-define(ID_GAMMA,0).

-define(ID_FORWARD_STEP,  40).
-define(ID_BACKWARD_STEP, 41).

% TODO: Add types
-record(proc, {pid,
               hist = [],
               env  = [],
               exp,
               mail = []}).

% TODO: Add types
-record(msg, {src,
              dest,
              val}).

% TODO: Add types
-record(sys, {msgs  = [],
              procs = []}).