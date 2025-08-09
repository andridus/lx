-module(test_simple_syntax).
-export([test_lambda_simple/0]).

-spec test_lambda_simple() -> any().
test_lambda_simple() ->
    LAMBDA_1 = fun(X_2, Y_3) ->
        X_2 + Y_3
    end,
    LAMBDA_1(3, 4).
