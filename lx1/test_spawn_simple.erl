-module(test_spawn_simple).
-export([test_spawn/0]).

-spec test_spawn() -> pid().
test_spawn() ->
    PID_1 = spawn(fun() ->
        42
    end),
    PID_1.
