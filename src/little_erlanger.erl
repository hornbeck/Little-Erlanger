-module(little_erlanger).
-export([double/1, add/2, multimath/1]).

double(X) ->
  X * 2.

add(X, Y) ->
  X + Y.

multimath({add, X, Y}) ->
  {add, X + Y};
multimath({double, X}) ->
  {double, X * 2}.
