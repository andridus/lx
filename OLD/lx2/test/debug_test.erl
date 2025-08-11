-module(debug_test).

-include_lib("eunit/include/eunit.hrl").

%% Debug test to see what's being generated
debug_parse_test() ->
    Source = "x = 42",
    case lx2:parse(Source) of
        {ok, AST} ->
            io:format("=== PARSED AST ===~n"),
            io:format("AST: ~p~n", [AST]),
            io:format("Length: ~p~n", [length(AST)]),
            case AST of
                [Node] ->
                    io:format("Node: ~p~n", [Node]),
                    io:format("Node type: ~p~n", [element(1, Node)]),
                    io:format("Node size: ~p~n", [tuple_size(Node)]);
                _ ->
                    io:format("Multiple nodes~n")
            end,
            ?assert(true);
        {error, Error} ->
            io:format("Parse error: ~p~n", [Error]),
            ?assert(false)
    end.

%% Test with file
debug_file_test() ->
    Filename = "examples/test_simple.lx",
    case file:read_file(Filename) of
        {ok, Content} ->
            Source = binary_to_list(Content),
            io:format("=== SOURCE ===~n"),
            io:format("~s~n", [Source]),
            io:format("=== PARSING ===~n"),
            case lx2:parse(Source) of
                {ok, AST} ->
                    io:format("=== PARSED AST ===~n"),
                    io:format("AST: ~p~n", [AST]),
                    io:format("Length: ~p~n", [length(AST)]),
                    lists:foreach(fun(Node) ->
                        io:format("Node: ~p~n", [Node])
                    end, AST),
                    ?assert(true);
                {error, Error} ->
                    io:format("Parse error: ~p~n", [Error]),
                    ?assert(false)
            end;
        {error, Reason} ->
            io:format("File read error: ~p~n", [Reason]),
            ?assert(false)
    end.

%% Test individual tokens
debug_tokens_test() ->
    Source = "x = 42",
    case lx2_lexer:string(Source) of
        {ok, Tokens, _} ->
            io:format("=== TOKENS ===~n"),
            io:format("Tokens: ~p~n", [Tokens]),
            ?assert(true);
        {error, Error} ->
            io:format("Token error: ~p~n", [Error]),
            ?assert(false)
    end.