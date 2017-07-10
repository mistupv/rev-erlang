-module(hello_world).
-export([main/0]).

main() -> 
  P2 = spawn(?MODULE, fun echo/0, []),
  P3 = spawn(?MODULE, fun target/0, []),
  P3 ! hello,
  P2 ! {P3, world}.

target() ->
  receive
    A ->
      receive
        B -> {A, B}
      end
  end.

echo() ->
  receive
    {P, M} -> P ! M
  end.
