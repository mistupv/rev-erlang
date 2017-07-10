-module(client_server).
-export([main/0]).

main() -> 
  S = spawn(?MODULE, fun server/0, []),
  spawn(?MODULE, fun client/1, [S]),
  client(S).

server() ->
  receive
    {P, _M} ->
      P ! ack,
      server()
  end.

client(S) ->
  S ! {self(), req},
  receive
    ack -> ok
  end.
