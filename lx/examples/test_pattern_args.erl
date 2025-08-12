-module(test_pattern_args).
-export([test_pattern/1, main/0]).

-spec test_pattern(any()) -> {any(), any()}.
test_pattern(_1) ->
    {NAME_2, AGE_3}.
-spec main() -> {any(), any()}.
main() ->
    RESULT_4 = test_pattern({<<"João"/utf8>>, 30}),
    RESULT_4.
