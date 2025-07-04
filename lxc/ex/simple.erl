-module(simple).
-export([simple/0]).

-spec simple() -> any().
simple() ->
begin
    X = 1,
    X + 1
end.

