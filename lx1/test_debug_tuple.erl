-module(test_debug_tuple).
-export([test_case/1]).

-spec test_case(any()) -> any().
test_case(RESULT_1) ->
    case RESULT_1 of
        {SUCCESS_2, DATA_3} ->
            DATA_3;
        {ERROR_4, REASON_5} ->
            REASON_5;
        __6 ->
            <<"unknown"/utf8>>;
    end.
