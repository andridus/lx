-module(match_with_guard).
-export([test_guard/0]).

-spec test_guard() -> {atom(), integer()}.
test_guard() ->
case 5 of
    X when X > 0 ->
        Y_aaaa = X * 2,
        {ok, Y_aaaa};
    Other ->
        Other
end.

