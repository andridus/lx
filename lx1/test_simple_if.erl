-module(test_simple_if).
-export([test_if/1]).

-spec test_if(any()) -> binary().
test_if(X_1) ->
    case X_1 > 0 of
        true -> <<"positive"/utf8>>;
        false -> <<"negative"/utf8>>
    end.
