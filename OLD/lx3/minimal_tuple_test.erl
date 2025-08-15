-module(minimal_tuple_test).
-export([test/0]).

test() ->
    % Testa tuplas de aridade 1, 2, 3
    io:format("~p~n", [minimal_tuple_parser:parse([{ '{', 1 }, { integer, 1, 1 }, { '}', 1 }])]),
    io:format("~p~n", [minimal_tuple_parser:parse([{ '{', 1 }, { integer, 1, 1 }, { ',', 1 }, { integer, 1, 2 }, { '}', 1 }])]),
    io:format("~p~n", [minimal_tuple_parser:parse([{ '{', 1 }, { integer, 1, 1 }, { ',', 1 }, { integer, 1, 2 }, { ',', 1 }, { integer, 1, 3 }, { '}', 1 }])]).