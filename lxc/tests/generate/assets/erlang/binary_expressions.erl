-module(binary_expressions).

-export([test_binary_expressions/0]).

test_binary_expressions() ->
    A = 10,
    B = 5,

    Sum = A + B,
    Diff = A - B,
    Product = A * B,
    Quotient = A / B,

    Is_equal = A =:= B,
    Is_not_equal = A =/= B,
    Is_less = A < B,
    Is_greater = A > B,

    Logical_and = true and false,
    Logical_or = true or false,
    Logical_andalso = true andalso false,
    Logical_orelse = true orelse false,

    Concat = "hello" ++ " world",

    {Sum, Diff, Product, Quotient, Is_equal, Is_not_equal, Is_less, Is_greater, Logical_and, Logical_or, Logical_andalso, Logical_orelse, Concat}.