-module(test_simple_case).
-export([test_case/0]).

-spec test_case() -> binary().
test_case() ->
    RESULT_1 = {ok, <<"data"/utf8>>},
    case RESULT_1 of
        {ok, DATA_2} ->
            <<"Success"/utf8>>;
        __3 ->
            <<"Error"/utf8>>;
    end.
