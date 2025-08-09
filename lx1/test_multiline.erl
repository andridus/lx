-module(test_multiline).
-export([test_lambda_multiline/0]).

-spec test_lambda_multiline() -> any().
test_lambda_multiline() ->
    LAMBDA_1 = fun(X_2, Y_3) ->
        X_2 + Y_3
    end,
    LAMBDA_1(5, 6).
