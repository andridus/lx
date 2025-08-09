-module(test_debug_normal_func).
-export([add/2]).

-spec add(integer(), integer()) -> integer().
add(X_1, Y_2) ->
    X_1 + Y_2.
