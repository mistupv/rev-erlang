-module(fact).
-export([factorial/1]).

factorial(X) ->
  case X of
    z -> {s,z};
    {s,Y} -> mult(X, factorial(Y))
  end.

mult(X,Y) ->
  case X of
    z -> z;
    {s,Z} -> sum(Y, mult(Z,Y))
  end.

sum(X,Y) ->
  case X of
    z -> Y;
    {s,Z} -> W = sum(Z, Y), {s,W}
  end.
