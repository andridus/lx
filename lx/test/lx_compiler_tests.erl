-module(lx_compiler_tests).
-include_lib("eunit/include/eunit.hrl").

%% Test literals compilation
literals_test() ->
    Source = "#integer\n1\n\n#float\n2.0\n\n#atoms\n:atom\n\n:ok\n\n#tuples\n{1}\n\n{1,2}\n\n{:ok, 1,2,3}\n\n\n#lists\n[1]\n\n[1,2]\n\n[1,2,3, :ok]\n\n# maps\n\n%{a: 1, b: 2}\n%{\"a\": 1, \"b\": 2, 123: 1}",
    {ok, AST} = lx_compiler:compile_ast(Source),

    % Test integer
    ?assert(lists:member({integer, 2, 1}, AST)),

    % Test float
    ?assert(lists:member({float, 5, 2.0}, AST)),

    % Test atoms
    ?assert(lists:member({atom, 8, atom}, AST)),
    ?assert(lists:member({atom, 10, ok}, AST)),

    % Test tuples
    ?assert(lists:member({tuple, 13, [{integer, 13, 1}]}, AST)),
    ?assert(lists:member({tuple, 15, [{integer, 15, 1}, {integer, 15, 2}]}, AST)),
    ?assert(lists:member({tuple, 17, [{atom, 17, ok}, {integer, 17, 1}, {integer, 17, 2}, {integer, 17, 3}]}, AST)),

    % Test lists
    ?assert(lists:member({list, 21, [{integer, 21, 1}]}, AST)),
    ?assert(lists:member({list, 23, [{integer, 23, 1}, {integer, 23, 2}]}, AST)),
    ?assert(lists:member({list, 25, [{integer, 25, 1}, {integer, 25, 2}, {integer, 25, 3}, {atom, 25, ok}]}, AST)),

    % Test maps
    ?assert(lists:member({map, 29, [{map_entry, 29, {atom, 29, a}, {integer, 29, 1}}, {map_entry, 29, {atom, 29, b}, {integer, 29, 2}}]}, AST)),
    ?assert(lists:member({map, 30, [{map_entry, 30, {string, 30, "a"}, {integer, 30, 1}}, {map_entry, 30, {string, 30, "b"}, {integer, 30, 2}}, {map_entry, 30, {integer, 30, 123}, {integer, 30, 1}}]}, AST)).

%% Test individual literal types
integer_test() ->
    {ok, AST} = lx_compiler:compile_ast("42"),
    ?assertEqual([{integer, 1, 42}], AST).

float_test() ->
    {ok, AST} = lx_compiler:compile_ast("3.14"),
    ?assertEqual([{float, 1, 3.14}], AST).

atom_test() ->
    {ok, AST} = lx_compiler:compile_ast(":test"),
    ?assertEqual([{atom, 1, test}], AST).

atom_key_test() ->
    {ok, AST} = lx_compiler:compile_ast("test"),
    ?assertEqual([{atom, 1, test}], AST).

string_test() ->
    {ok, AST} = lx_compiler:compile_ast("\"hello\""),
    ?assertEqual([{string, 1, "hello"}], AST).

%% Test data structures
empty_tuple_test() ->
    {ok, AST} = lx_compiler:compile_ast("{}"),
    ?assertEqual([{tuple, 1, []}], AST).

single_element_tuple_test() ->
    {ok, AST} = lx_compiler:compile_ast("{1}"),
    ?assertEqual([{tuple, 1, [{integer, 1, 1}]}], AST).

multi_element_tuple_test() ->
    {ok, AST} = lx_compiler:compile_ast("{1, 2, 3}"),
    ?assertEqual([{tuple, 1, [{integer, 1, 1}, {integer, 1, 2}, {integer, 1, 3}]}], AST).

empty_list_test() ->
    {ok, AST} = lx_compiler:compile_ast("[]"),
    ?assertEqual([{list, 1, []}], AST).

single_element_list_test() ->
    {ok, AST} = lx_compiler:compile_ast("[1]"),
    ?assertEqual([{list, 1, [{integer, 1, 1}]}], AST).

multi_element_list_test() ->
    {ok, AST} = lx_compiler:compile_ast("[1, 2, 3]"),
    ?assertEqual([{list, 1, [{integer, 1, 1}, {integer, 1, 2}, {integer, 1, 3}]}], AST).

empty_map_test() ->
    {ok, AST} = lx_compiler:compile_ast("%{}"),
    ?assertEqual([{map, 1, []}], AST).

simple_map_test() ->
    {ok, AST} = lx_compiler:compile_ast("%{a: 1}"),
    ?assertEqual([{map, 1, [{map_entry, 1, {atom, 1, a}, {integer, 1, 1}}]}], AST).

complex_map_test() ->
    {ok, AST} = lx_compiler:compile_ast("%{a: 1, b: 2, \"c\": 3}"),
    ExpectedMap = [{map_entry, 1, {atom, 1, a}, {integer, 1, 1}},
                   {map_entry, 1, {atom, 1, b}, {integer, 1, 2}},
                   {map_entry, 1, {string, 1, "c"}, {integer, 1, 3}}],
    ?assertEqual([{map, 1, ExpectedMap}], AST).

%% Test complex map with nested structures
complex_nested_map_test() ->
    Source = "%{\n  \"a\": 1,\n  \"b\": 2,\n  123: {tuple,1, 2, 3},\n  list: [1,2,3,4],\n  map: %{a: 1, b: 2}\n}",
    {ok, AST} = lx_compiler:compile_ast(Source),
    ?assertEqual([
        {map,1,[
            {map_entry,2,{string,2,"a"},{integer,2,1}},
            {map_entry,3,{string,3,"b"},{integer,3,2}},
            {map_entry,4,{integer,4,123},{tuple,4,[{atom,4,tuple},{integer,4,1},{integer,4,2},{integer,4,3}]}},
            {map_entry,5,{atom,5,list},{list,5,[{integer,5,1},{integer,5,2},{integer,5,3},{integer,5,4}]}},
            {map_entry,6,{atom,6,map},{map,6,[{map_entry,6,{atom,6,a},{integer,6,1}},{map_entry,6,{atom,6,b},{integer,6,2}}]}}
        ]}
    ], AST).

%% Test complex tuple
complex_tuple_test() ->
    Source = "{1, 2, 3, {4, 5}, [6, 7], %{a: 8}}",
    {ok, AST} = lx_compiler:compile_ast(Source),
    ?assertEqual([
        {tuple,1,[
            {integer,1,1},
            {integer,1,2},
            {integer,1,3},
            {tuple,1,[{integer,1,4},{integer,1,5}]},
            {list,1,[{integer,1,6},{integer,1,7}]},
            {map,1,[{map_entry,1,{atom,1,a},{integer,1,8}}]}
        ]}
    ], AST).

%% Test complex list
complex_list_test() ->
    Source = "[1, 2, {3, 4}, [5, 6], %{a: 7}]",
    {ok, AST} = lx_compiler:compile_ast(Source),
    ?assertEqual([
        {list,1,[
            {integer,1,1},
            {integer,1,2},
            {tuple,1,[{integer,1,3},{integer,1,4}]},
            {list,1,[{integer,1,5},{integer,1,6}]},
            {map,1,[{map_entry,1,{atom,1,a},{integer,1,7}}]}
        ]}
    ], AST).

%% Test comments
comments_test() ->
    {ok, AST} = lx_compiler:compile_ast("# This is a comment\n1\n# Another comment\n2"),
    ?assertEqual([{integer, 2, 1}, {integer, 4, 2}], AST).

%% Test whitespace handling
whitespace_test() ->
    {ok, AST} = lx_compiler:compile_ast("  1  \n  2  "),
    ?assertEqual([{integer, 1, 1}, {integer, 2, 2}], AST).

%% Test error cases
invalid_syntax_test() ->
    {error, _} = lx_compiler:compile("{"),
    {error, _} = lx_compiler:compile("}"),
    {error, _} = lx_compiler:compile("["),
    {error, _} = lx_compiler:compile("]"),
    {error, _} = lx_compiler:compile("%{").

%% Test file compilation
file_compilation_test() ->
    {ok, AST} = lx_compiler:compile_file_ast("examples/literals.lx"),
    ?assert(is_list(AST)),
    ?assert(length(AST) > 0).

%% Test CLI functionality
cli_usage_test() ->
    % This test verifies that the CLI module exports the main function
    ?assert(erlang:function_exported(lx_cli, main, 1)).

%% Performance test for large files
performance_test() ->
    % Create a large source with many literals
    LargeSource = string:join([integer_to_list(I) || I <- lists:seq(1, 1000)], "\n"),
    {Time, {ok, _AST}} = timer:tc(lx_compiler, compile_ast, [LargeSource]),
    % Should compile in less than 1 second
    ?assert(Time < 1000000).