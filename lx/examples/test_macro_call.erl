-module(test_macro_call).
-export([main/0]).
main(0) ->
    a([[1 | [2 | [3 | []]]] | []]).