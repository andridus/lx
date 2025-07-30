-module(complex_directives).
-export([complex_directives/0, nested_directives/0]).

-spec complex_directives() -> integer().
complex_directives() ->
    A_1 = 5,
    B_2 = 3,
    C_3 = 2,
    ,
    ,
    (A_1 + B_2) * C_3.

-spec nested_directives() -> integer().
nested_directives() ->
    X_4 = 10,
    Y_5 = 20,
    ,
    ,
    X_4 + Y_5.
