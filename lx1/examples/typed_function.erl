-module(typed_function).
-export([multiply/2, concat_strings/2, compare_values/2]).

-spec multiply(integer(), float()) -> float().
multiply(A_1, B_2) ->
    A_1 * B_2.
-spec concat_strings(binary(), binary()) -> binary().
concat_strings(S1_3, S2_4) ->
    S1_3 ++ S2_4.
-spec compare_values(integer(), integer()) -> boolean().
compare_values(X_5, Y_6) ->
    X_5 > Y_6.
