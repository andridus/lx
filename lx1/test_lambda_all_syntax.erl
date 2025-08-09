-module(test_lambda_all_syntax).
-export([test_lambda_one_head_single/0, test_lambda_one_head_multi/0, test_lambda_multi_head_single/0, test_lambda_multi_head_multi/0]).

-spec test_lambda_one_head_single() -> any().
test_lambda_one_head_single() ->
    LAMBDA_1 = fun(X_2, Y_3) ->
        X_2 + Y_3
    end,
    LAMBDA_1(1, 2).
-spec test_lambda_one_head_multi() -> any().
test_lambda_one_head_multi() ->
    LAMBDA_1 = fun(X_2, Y_3) ->
        RESULT_4 = X_2 + Y_3,
    RESULT_4 * 2
    end,
    LAMBDA_1(3, 4).
-spec test_lambda_multi_head_single() -> any().
test_lambda_multi_head_single() ->
    LAMBDA_1 = fun
        (X_2, Y_3) ->
            X_2 + Y_3;
        (X_2) ->
            X_2 * 2;
        () ->
            0
    end,
    LAMBDA_1(5, 6).
-spec test_lambda_multi_head_multi() -> any().
test_lambda_multi_head_multi() ->
    LAMBDA_1 = fun
        (X_2, Y_3) ->
            SUM_5 = X_2 + Y_3,
    SUM_5 * 2;
        (X_2) ->
            DOUBLED_6 = X_2 * 2,
    DOUBLED_6 + 1;
        () ->
            42
    end,
    LAMBDA_1(7, 8).
