-module(complex_test).
-export([main/0]).

main() ->

    (1 + (2 * 3)),
    {1, 2, 3},
    [1 | [2 | [3 | [4 | []]]]],
    #{a => 1, b => 2}.

