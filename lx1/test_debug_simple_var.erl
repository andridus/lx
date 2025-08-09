-module(test_debug_simple_var).
-export([test_var/0]).

-spec test_var() -> integer().
test_var() ->
    (X_1 = 5),
    (Y_2 = 10),
    X_1 + Y_2.
