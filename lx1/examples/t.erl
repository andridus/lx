-module(t).
-export([test/0]).

-spec test() -> map().
test() ->
    #{<<"name"/utf8>> => <<"João"/utf8>>, age => 30, active => true}.
