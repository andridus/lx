-module(final_demo).
-export([main/0]).

main() ->

    (1 + (2 * 3)),
    {ok, 42, "hello"},
    [1 | [2 | [3 | [4 | [5 | []]]]]],
    #{name => "Lx", version => 1, features => [maps | [tuples | [lists | []]]]}.

