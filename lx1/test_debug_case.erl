-module(test_debug_case).
-export([test_case/1]).

-spec test_case(any()) -> binary().
test_case(X_1) ->
    case X_1 of
        1 ->
            <<"one"/utf8>>;
        2 ->
            <<"two"/utf8>>;
        __2 ->
            <<"other"/utf8>>;
    end.
