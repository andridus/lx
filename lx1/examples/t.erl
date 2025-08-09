-module(t).
-export([factorial/1]).

-spec factorial(integer()) -> integer().
factorial(0) ->
    1;
factorial(N_1) ->
    N_1 * factorial(N_1 - 1).
