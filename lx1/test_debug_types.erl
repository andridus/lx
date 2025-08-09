-module(test_debug_types).
-export([test_simple/0]).

-spec test_simple() -> function().
test_simple() ->
    (ADD_1 = fun(X_2, Y_3) ->
        5
    end),
    ADD_1.
