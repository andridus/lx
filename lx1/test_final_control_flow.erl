-module(test_final_control_flow).
-export([test_if/0, test_case/0, test_with/0]).

-spec test_if() -> binary().
test_if() ->
    case 5 > 3 of
        true -> <<"positive"/utf8>>;
        false -> <<"negative"/utf8>>
    end.
-spec test_case() -> binary().
test_case() ->
    RESULT_1 = {ok, <<"data"/utf8>>},
    case RESULT_1 of
        {ok, DATA_2} ->
            <<"Success"/utf8>>;
        __3 ->
            <<"Error"/utf8>>;
    end.
-spec test_with() -> binary().
test_with() ->
    RESULT_1 = {ok, <<"user"/utf8>>},
    case RESULT_1 of
        {ok, USER_4} -> <<"Found user"/utf8>>;
        _ -> <<"Not found"/utf8>>
    end.
