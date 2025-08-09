-module(test_debug_add).
-export([test_add/0]).

-spec test_add() -> function().
test_add() ->
    (ADD_1 = fun(X_2, Y_3) ->
        X_2 + Y_3
    end),
    ADD_1.
