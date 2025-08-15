-module(simple_ast_test).

-include_lib("eunit/include/eunit.hrl").

%% Test basic AST structure
basic_ast_test() ->
    % Test simple integer literal
    Source = "42",
    case lx2:parse(Source) of
        {ok, AST} ->
            ?assert(is_list(AST)),
            ?assert(length(AST) > 0);
        {error, Error} ->
            io:format("Parse error: ~p~n", [Error]),
            ?assert(false)
    end.

%% Test basic string literal
string_literal_test() ->
    Source = "\"hello\"",
    case lx2:parse(Source) of
        {ok, AST} ->
            ?assert(is_list(AST)),
            ?assert(length(AST) > 0);
        {error, Error} ->
            io:format("Parse error: ~p~n", [Error]),
            ?assert(false)
    end.

%% Test basic atom
atom_test() ->
    Source = ":hello",
    case lx2:parse(Source) of
        {ok, AST} ->
            ?assert(is_list(AST)),
            ?assert(length(AST) > 0);
        {error, Error} ->
            io:format("Parse error: ~p~n", [Error]),
            ?assert(false)
    end.

%% Test basic boolean
boolean_test() ->
    Source = "true",
    case lx2:parse(Source) of
        {ok, AST} ->
            ?assert(is_list(AST)),
            ?assert(length(AST) > 0);
        {error, Error} ->
            io:format("Parse error: ~p~n", [Error]),
            ?assert(false)
    end.

%% Test basic nil
nil_test() ->
    Source = "nil",
    case lx2:parse(Source) of
        {ok, AST} ->
            ?assert(is_list(AST)),
            ?assert(length(AST) > 0);
        {error, Error} ->
            io:format("Parse error: ~p~n", [Error]),
            ?assert(false)
    end.

%% Test basic identifier
identifier_test() ->
    Source = "x",
    case lx2:parse(Source) of
        {ok, AST} ->
            ?assert(is_list(AST)),
            ?assert(length(AST) > 0);
        {error, Error} ->
            io:format("Parse error: ~p~n", [Error]),
            ?assert(false)
    end.

%% Test basic underscore
underscore_test() ->
    Source = "_",
    case lx2:parse(Source) of
        {ok, AST} ->
            ?assert(is_list(AST)),
            ?assert(length(AST) > 0);
        {error, Error} ->
            io:format("Parse error: ~p~n", [Error]),
            ?assert(false)
    end.