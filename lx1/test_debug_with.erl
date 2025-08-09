-module(test_debug_with).
-export([test_with/0]).

-spec test_with() -> any().
test_with() ->
    RESULT_1 = {ok, <<"data"/utf8>>},
    case RESULT_1 of
        {ok, DATA_2} -> DATA_2;
        _ -> error(no_match)
    end.
