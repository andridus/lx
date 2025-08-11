-module(t).
-moduledoc "Something" .
-export([test_directive/0]).

-doc "Test function".
-spec test_directive() -> atom().
test_directive() ->
    ok.
