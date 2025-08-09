-module(test_binary_simple).
-export([test_binary/0, test_binary_with_options/0]).

-spec test_binary() -> binary().
test_binary() ->
    BINARY_1 = <<1, 2, 3>>,
    BINARY_1.
-spec test_binary_with_options() -> binary().
test_binary_with_options() ->
    VERSION_2 = 1,
    BINARY_1 = <<VERSION_2:8, 255:16/big>>,
    BINARY_1.
