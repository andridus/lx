-module(t).
-export([process_mixed_args/1]).

-spec process_mixed_args(integer() | binary() | atom()) -> integer() | binary().
process_mixed_args(X_1) ->
    X_1 * 2;
process_mixed_args(S_2) ->
    S_2;
process_mixed_args(ok) ->
    <<"success"/utf8>>.
