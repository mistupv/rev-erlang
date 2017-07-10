-module(acknowledge).
-export([main/0]).

main() ->
  MyPid = self(),
  OtherPid = spawn(?MODULE, fun other_main/0, []),
  OtherPid ! MyPid,
  receive
    ack -> ok;
    _Other -> not_ok
  end.

other_main() ->
  receive
    Pid -> Pid ! ack
  end.
