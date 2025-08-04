-module(lx_lexer_tests).
-include_lib("eunit/include/eunit.hrl").

%% Test basic token recognition
integer_token_test() ->
    {ok, Tokens, _} = lx_lexer:string("42"),
    ?assertEqual([{integer, 1, 42}], Tokens).

float_token_test() ->
    {ok, Tokens, _} = lx_lexer:string("3.14"),
    ?assertEqual([{float, 1, 3.14}], Tokens).

atom_token_test() ->
    {ok, Tokens, _} = lx_lexer:string(":test"),
    ?assertEqual([{atom, 1, test}], Tokens).

atom_key_token_test() ->
    {ok, Tokens, _} = lx_lexer:string("test"),
    ?assertEqual([{atom, 1, test}], Tokens).

string_token_test() ->
    {ok, Tokens, _} = lx_lexer:string("\"hello\""),
    ?assertEqual([{string, 1, "hello"}], Tokens).

%% Test punctuation tokens
punctuation_tokens_test() ->
    {ok, Tokens, _} = lx_lexer:string("{},[]():"),
    Expected = [{'{', 1}, {'}', 1}, {',', 1}, {'[', 1}, {']', 1}, {'(', 1}, {')', 1}, {':', 1}],
    ?assertEqual(Expected, Tokens).

map_token_test() ->
    {ok, Tokens, _} = lx_lexer:string("%"),
    ?assertEqual([{'%', 1}], Tokens).

%% Test whitespace handling
whitespace_test() ->
    {ok, Tokens, _} = lx_lexer:string("  1  2  "),
    ?assertEqual([{integer, 1, 1}, {integer, 1, 2}], Tokens).

newline_test() ->
    {ok, Tokens, _} = lx_lexer:string("1\n2"),
    ?assertEqual([{integer, 1, 1}, {integer, 2, 2}], Tokens).

%% Test comments
comment_test() ->
    {ok, Tokens, _} = lx_lexer:string("# This is a comment\n1"),
    ?assertEqual([{integer, 2, 1}], Tokens).

comment_only_test() ->
    {ok, Tokens, _} = lx_lexer:string("# Only comment"),
    ?assertEqual([], Tokens).

%% Test complex expressions
complex_expression_test() ->
    {ok, Tokens, _} = lx_lexer:string("{:ok, 1, 2, 3}"),
    Expected = [{'{', 1}, {atom, 1, ok}, {',', 1}, {integer, 1, 1}, {',', 1}, {integer, 1, 2}, {',', 1}, {integer, 1, 3}, {'}', 1}],
    ?assertEqual(Expected, Tokens).

map_expression_test() ->
    {ok, Tokens, _} = lx_lexer:string("%{a: 1, b: 2}"),
    Expected = [{'%', 1}, {'{', 1}, {atom, 1, a}, {':', 1}, {integer, 1, 1}, {',', 1}, {atom, 1, b}, {':', 1}, {integer, 1, 2}, {'}', 1}],
    ?assertEqual(Expected, Tokens).

%% Test error cases
invalid_token_test() ->
    {error, {1, lx_lexer, {illegal, "!"}}, _} = lx_lexer:string("!").

invalid_float_test() ->
    {error, {1, lx_lexer, {illegal, "."}}, _} = lx_lexer:string(".").

%% Test multiple lines with mixed content
mixed_content_test() ->
    Source = "# Test file\n1\n2.0\n:atom\ntest\n\"string\"\n{1, 2}\n[3, 4]\n%{a: 1}",
    {ok, Tokens, _} = lx_lexer:string(Source),
    ExpectedTokens = [
        {integer, 2, 1},
        {float, 3, 2.0},
        {atom, 4, atom},
        {atom, 5, test},
        {string, 6, "string"},
        {'{', 7}, {integer, 7, 1}, {',', 7}, {integer, 7, 2}, {'}', 7},
        {'[', 8}, {integer, 8, 3}, {',', 8}, {integer, 8, 4}, {']', 8},
        {'%', 9}, {'{', 9}, {atom, 9, a}, {':', 9}, {integer, 9, 1}, {'}', 9}
    ],
    ?assertEqual(ExpectedTokens, Tokens).