-module(directives_transparent).
-export([fun/1, main/0]).

-spec fun(any()) -> any().
fun(A_1) ->
    A_1,
    .
-spec main() -> any().
main() ->
    X_2 = 42,
    fun(X_2).
