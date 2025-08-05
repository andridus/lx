-module(simple_erl_demo).
-export([main/0]).
main() ->
    ok, {op, 1, ++, 1, 2, 3, 4}.