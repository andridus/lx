-module(simple_comparison).
-export([test/0]).

-spec test() -> boolean().
test() ->
    A_1 = 10,
    B_2 = 5,
    A_1 == B_2.
