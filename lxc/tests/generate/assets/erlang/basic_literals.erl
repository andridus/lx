-module(basic_literals).

-export([test_literals/0]).

test_literals() ->
    X = 42,
    Y = 3.14,
    Z = "hello world",
    Flag = true,
    Status = ok,
    Empty = nil,
    {X, Y, Z, Flag, Status, Empty}.