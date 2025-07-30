-module(simple_arithmetic).
-export([simple_arithmetic/0, multiplication/0, division/0, subtraction/0, test/0]).

-spec simple_arithmetic() -> integer().
simple_arithmetic() ->
    A_1 = 10,
    B_2 = 5,
    A_1 + B_2.

-spec multiplication() -> integer().
multiplication() ->
    3 * 4.

-spec division() -> integer().
division() ->
    15 / 3.

-spec subtraction() -> integer().
subtraction() ->
    10 - 3.

-spec test() -> integer().
test() ->
    10 + 5.
