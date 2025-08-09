-module(test_multihead_final).
-export([test_lambda_oneline/0, test_lambda_multiline/0, test_lambda_multihead/0, test_lambda_do_block/0]).

-spec test_lambda_oneline() -> any().
test_lambda_oneline() ->
    LAMBDA_1 = fun(X_2, Y_3) ->
        X_2 + Y_3
    end,
    LAMBDA_1(3, 4).
-spec test_lambda_multiline() -> any().
test_lambda_multiline() ->
    LAMBDA_1 = fun(X_2, Y_3) ->
        RESULT_4 = X_2 + Y_3,
    RESULT_4 * 2
    end,
    LAMBDA_1(5, 6).
-spec test_lambda_multihead() -> any().
test_lambda_multihead() ->
    LAMBDA_1 = fun
        (ok) ->
            <<"success"/utf8>>;
        (error) ->
            <<"failure"/utf8>>;
        (__5) ->
            <<"unknown"/utf8>>
    end,
    LAMBDA_1(ok).
-spec test_lambda_do_block() -> any().
test_lambda_do_block() ->
    LAMBDA_1 = fun(X_2) ->
        DOUBLED_6 = X_2 * 2,
    DOUBLED_6 + 1
    end,
    LAMBDA_1(10).
