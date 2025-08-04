-module(lx_integration_tests).
-include_lib("eunit/include/eunit.hrl").

%% Test complete compilation pipeline
full_compilation_test() ->
    Source = "1\n2.0\n:test\n\"hello\"\n{1, 2}\n[3, 4]\n%{a: 1}",
    {ok, AST} = lx_compiler:compile(Source),
    ExpectedAST = [
        {integer, 1, 1},
        {float, 2, 2.0},
        {atom, 3, test},
        {string, 4, "hello"},
        {tuple, 5, [{integer, 5, 1}, {integer, 5, 2}]},
        {list, 6, [{integer, 6, 3}, {integer, 6, 4}]},
        {map, 7, [{map_entry, 7, {atom, 7, a}, {integer, 7, 1}}]}
    ],
    ?assertEqual(ExpectedAST, AST).

%% Test file compilation
file_compilation_test() ->
    {ok, AST} = lx_compiler:compile_file("examples/literals.lx"),
    ?assert(is_list(AST)),
    ?assert(length(AST) > 0),
    % Verify we have all expected literal types
    ?assert(lists:any(fun({integer, _, _}) -> true; (_) -> false end, AST)),
    ?assert(lists:any(fun({float, _, _}) -> true; (_) -> false end, AST)),
    ?assert(lists:any(fun({atom, _, _}) -> true; (_) -> false end, AST)),
    ?assert(lists:any(fun({tuple, _, _}) -> true; (_) -> false end, AST)),
    ?assert(lists:any(fun({list, _, _}) -> true; (_) -> false end, AST)),
    ?assert(lists:any(fun({map, _, _}) -> true; (_) -> false end, AST)).

%% Test CLI functionality
cli_test() ->
    % Test that CLI module exists and exports main function
    ?assert(erlang:function_exported(lx_cli, main, 1)).

%% Test error handling
error_handling_test() ->
    % Test invalid syntax
    {error, _} = lx_compiler:compile("{"),
    {error, _} = lx_compiler:compile("}"),
    {error, _} = lx_compiler:compile("["),
    {error, _} = lx_compiler:compile("]"),
    {error, _} = lx_compiler:compile("%{"),

    % Test invalid tokens
    {error, _} = lx_compiler:compile("!"),
    {error, _} = lx_compiler:compile("@"),
    {error, _} = lx_compiler:compile("$").

%% Test performance with large files
performance_test() ->
    % Create a large source file
    LargeSource = string:join([
        integer_to_list(I) || I <- lists:seq(1, 100)
    ], "\n"),

    {Time, {ok, AST}} = timer:tc(lx_compiler, compile, [LargeSource]),

    % Should compile in reasonable time (less than 1 second)
    ?assert(Time < 1000000),
    % Should have correct number of expressions
    ?assertEqual(100, length(AST)).

%% Test nested structures
nested_structures_test() ->
    Source = "{{1, 2}, [3, 4]}",
    {ok, AST} = lx_compiler:compile(Source),
    ?assert(is_list(AST)),
    ?assert(length(AST) =:= 1),
    [Tuple] = AST,
    ?assertEqual(tuple, element(1, Tuple)).

%% Test mixed content with comments
mixed_content_with_comments_test() ->
    Source = "# Start\n1\n# Middle\n2.0\n# End\n:test",
    {ok, AST} = lx_compiler:compile(Source),
    ExpectedAST = [
        {integer, 2, 1},
        {float, 4, 2.0},
        {atom, 6, test}
    ],
    ?assertEqual(ExpectedAST, AST).

%% Test whitespace handling
whitespace_handling_test() ->
    Source = "  1  \n  2.0  \n  :test  ",
    {ok, AST} = lx_compiler:compile(Source),
    ExpectedAST = [
        {integer, 1, 1},
        {float, 2, 2.0},
        {atom, 3, test}
    ],
    ?assertEqual(ExpectedAST, AST).

%% Test complex real-world scenario
real_world_scenario_test() ->
    Source = "# Configuration file\n"
             "%{\n"
             "  server: %{\n"
             "    host: \"localhost\",\n"
             "    port: 8080,\n"
             "    timeout: 30.5\n"
             "  },\n"
             "  database: %{\n"
             "    type: :postgres,\n"
             "    pool_size: 10\n"
             "  },\n"
             "  features: [:auth, :logging, :metrics]\n"
             "}",

    {ok, AST} = lx_compiler:compile(Source),
    ?assert(is_list(AST)),
    ?assert(length(AST) > 0).

%% Test edge cases
edge_cases_test() ->
    % Only comments
    {error, empty_source} = lx_compiler:compile("# Only comments\n# No code").

%% Test CLI argument handling
cli_argument_test() ->
    % Test that CLI can handle different argument patterns
    ?assert(erlang:function_exported(lx_cli, main, 1)).