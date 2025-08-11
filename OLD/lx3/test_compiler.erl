-module(test_compiler).

-export([test/0]).

test() ->
    io:format("Testing LX Compiler~n"),

    % Test 1: Tokenize
    io:format("Test 1: Tokenizing 'x = 42'~n"),
    case lx_lexer:string("x = 42") of
        {ok, Tokens, _} ->
            io:format("Tokens: ~p~n", [Tokens]);
        {error, Error} ->
            io:format("Error: ~p~n", [Error])
    end,

    % Test 2: Parse
    io:format("~nTest 2: Parsing tokens~n"),
    case lx_lexer:string("x = 42") of
        {ok, Tokens2, _} ->
            case lx_parser:parse(Tokens2) of
                {ok, AST} ->
                    io:format("AST: ~p~n", [AST]);
                {error, Error2} ->
                    io:format("Parse Error: ~p~n", [Error2])
            end;
        {error, Error2} ->
            io:format("Lexer Error: ~p~n", [Error2])
    end,

    % Test 3: Full compilation
    io:format("~nTest 3: Full compilation~n"),
    case lx:compile("x = 42") of
        {ok, Result, Specs} ->
            io:format("Compilation successful~n"),
            io:format("Result: ~p~n", [Result]),
            io:format("Specs: ~p~n", [Specs]);
        {error, Error3} ->
            io:format("Compilation Error: ~p~n", [Error3])
    end,

    io:format("~nTests completed.~n").