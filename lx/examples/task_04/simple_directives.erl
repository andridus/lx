-module(simple_directives).
-export([debug_example/0, type_inspection/0]).

-spec debug_example() -> integer().
debug_example() ->
    X_1 = 42,
    Y_2 = 45,
    X_1 + Y_2.

-spec type_inspection() -> integer().
type_inspection() ->
    A_3 = 10,
    B_4 = <<"world"/utf8>>,
    C_5 = true,
    A_3 + 5.
