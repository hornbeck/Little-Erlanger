-module(little_erlanger).
-export([double/1, add/2, multimath/1, multimathcase/1, try_to_add/2, add_with_words/2]).

double(X) ->
  X * 2.

add(X, Y) when is_integer(X), is_integer(Y) -> 
    X + Y.

try_to_add(X, Y) ->
    try add(X, Y)
    catch
        error:Error ->
            {badArgument, Error}
    end.

multimath({add, X, Y}) ->
  {add, X + Y};
multimath({double, X}) ->
  {double, X * 2}.

multimathcase(AddOrDouble) ->
    case AddOrDouble of
        {add, X, Y} ->
            {ok, X + Y};
        {double, X} ->
            {ok, X * 2}
    end.

add_with_words(First, Second) ->
  case Second of
    zero -> First + 0;
    one  -> First + 1;
    two  -> First + 2;
    three -> First + 3;
    four -> First + 4;
    five -> First + 5;
    six  -> First + 6;
    seven -> First + 7;
    eight -> First + 8;
    nine -> First + 9;
    Second when is_number(Second) -> First + Second
  end.
