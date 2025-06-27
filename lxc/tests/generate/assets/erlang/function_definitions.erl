-module(function_definitions).

-export([factorial/1, greet/1, add/2, test_functions/0]).

factorial(0) ->
    1;
factorial(N) when N > 0 ->
    N * factorial(N - 1).

greet(Name) ->
    "Hello, " ++ Name.

add(X, Y) ->
    X + Y.

test_functions() ->
    Result1 = factorial(5),
    Result2 = greet("world"),
    Result3 = add(10, 20),

    {Result1, Result2, Result3}.