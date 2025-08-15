-module(working_test).

-include_lib("eunit/include/eunit.hrl").

%% Test what's currently working
working_parse_test() ->
    Source = "x = 42",
    case lx2:parse(Source) of
        {ok, AST} ->
            io:format("=== WORKING AST ===~n"),
            io:format("AST: ~p~n", [AST]),
            [Node] = AST,
            io:format("Node: ~p~n", [Node]),
            io:format("Node type: ~p~n", [element(1, Node)]),
            io:format("Node size: ~p~n", [tuple_size(Node)]),
            ?assertEqual(variable_binding, element(1, Node)),
            ?assertEqual(3, tuple_size(Node));
        {error, Error} ->
            io:format("Parse error: ~p~n", [Error]),
            ?assert(false)
    end.

%% Test with file
working_file_test() ->
    Filename = "examples/test_simple.lx",
    case file:read_file(Filename) of
        {ok, Content} ->
            Source = binary_to_list(Content),
            io:format("=== SOURCE ===~n"),
            io:format("~s~n", [Source]),
            case lx2:parse(Source) of
                {ok, AST} ->
                    io:format("=== WORKING AST ===~n"),
                    io:format("AST: ~p~n", [AST]),
                    io:format("Length: ~p~n", [length(AST)]),
                    lists:foreach(fun(Node) ->
                        io:format("Node: ~p~n", [Node])
                    end, AST),
                    ?assert(length(AST) > 0);
                {error, Error} ->
                    io:format("Parse error: ~p~n", [Error]),
                    ?assert(false)
            end;
        {error, Reason} ->
            io:format("File read error: ~p~n", [Reason]),
            ?assert(false)
    end.

%% Test different types
different_types_test() ->
    Tests = [
        {"42", "integer literal"},
        {"\"hello\"", "string literal"},
        {":world", "atom literal"},
        {"true", "boolean literal"},
        {"nil", "nil literal"},
        {"x", "identifier"},
        {"_", "underscore"}
    ],

    lists:foreach(fun({Source, Description}) ->
        io:format("Testing ~s: ~s~n", [Description, Source]),
        case lx2:parse(Source) of
            {ok, AST} ->
                io:format("  Result: ~p~n", [AST]),
                ?assert(length(AST) > 0);
            {error, Error} ->
                io:format("  Error: ~p~n", [Error]),
                ?assert(false)
        end
    end, Tests).

%% Test variable binding
binding_test() ->
    Source = "x = 42",
    case lx2:parse(Source) of
        {ok, AST} ->
            [Node] = AST,
            {Type, VarName, Expr} = Node,
            ?assertEqual(variable_binding, Type),
            ?assertEqual(x, VarName),
            ?assertEqual({literal, integer, 42}, Expr);
        {error, Error} ->
            io:format("Parse error: ~p~n", [Error]),
            ?assert(false)
    end.

%% Test multiple statements
multiple_statements_test() ->
    Source = "x = 42; y = \"hello\"",
    case lx2:parse(Source) of
        {ok, AST} ->
            io:format("Multiple statements AST: ~p~n", [AST]),
            ?assertEqual(2, length(AST)),
            [Bind1, Bind2] = AST,
            ?assertEqual(variable_binding, element(1, Bind1)),
            ?assertEqual(variable_binding, element(1, Bind2));
        {error, Error} ->
            io:format("Parse error: ~p~n", [Error]),
            ?assert(false)
    end.