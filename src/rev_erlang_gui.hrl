-record(status, {loaded = false,
                 running = false}).

-define(FRAME_SIZE_INIT, {800, 600}).

-define(ABOUT, ?wxID_ABOUT).
-define(EXIT, ?wxID_EXIT).
-define(OPEN, ?wxID_OPEN).



-define(FORW_SEQ_BUTTON,401).
-define(FORW_CHECK_BUTTON,402).
-define(FORW_SEND_BUTTON,403).
-define(FORW_RECEIVE_BUTTON,404).
-define(FORW_SPAWN_BUTTON,405).
-define(FORW_SELF_BUTTON,406).
-define(FORW_SCHED_BUTTON,407).
-define(BACK_SEQ_BUTTON,411).
-define(BACK_CHECK_BUTTON,412).
-define(BACK_SEND_BUTTON,413).
-define(BACK_RECEIVE_BUTTON,414).
-define(BACK_SPAWN_BUTTON,415).
-define(BACK_SELF_BUTTON,416).
-define(BACK_SCHED_BUTTON,417).
-define(RAND_BUTTON,421).
-define(FORW_RAND_BUTTON,422).
-define(BACK_RAND_BUTTON,423).

-define(SYSTEM,500).
-define(STATUS,501).
-define(FRAME, 509).
-define(INPUT_TEXT,502).
-define(PID_TEXT,508).
-define(STATE_TEXT,510).
-define(CODE_TEXT,512).
-define(STATUS_BAR, 529).
-define(START_BUTTON,400).
-define(FUN_CHOICE,511).
-define(LEFT_NOTEBOOK,520).
-define(RIGHT_NOTEBOOK,521).

-define(PAGEPOS_CODE,0).
-define(PAGEPOS_STATE,1).
-define(PAGEPOS_MANU,0).
-define(PAGEPOS_SEMI,1).
-define(PAGEPOS_AUTO,2).
