-module(lx_parser_tests).
-include_lib("eunit/include/eunit.hrl").

%% Test basic literal parsing
parse_integer_test() ->
    Tokens = [{integer, 1, 42}],
    {ok, AST} = lx_parser:parse(Tokens),
    ?assertEqual([{integer, 1, 42}], AST).

parse_float_test() ->
    Tokens = [{float, 1, 3.14}],
    {ok, AST} = lx_parser:parse(Tokens),
    ?assertEqual([{float, 1, 3.14}], AST).

parse_atom_test() ->
    Tokens = [{atom, 1, test}],
    {ok, AST} = lx_parser:parse(Tokens),
    ?assertEqual([{atom, 1, test}], AST).

parse_string_test() ->
    Tokens = [{string, 1, "hello"}],
    {ok, AST} = lx_parser:parse(Tokens),
    ?assertEqual([{string, 1, "hello"}], AST).

%% Test tuple parsing
parse_empty_tuple_test() ->
    Tokens = [{'{', 1}, {'}', 1}],
    {ok, AST} = lx_parser:parse(Tokens),
    ?assertEqual([{tuple, 1, []}], AST).

parse_single_element_tuple_test() ->
    Tokens = [{'{', 1}, {integer, 1, 1}, {'}', 1}],
    {ok, AST} = lx_parser:parse(Tokens),
    ?assertEqual([{tuple, 1, [{integer, 1, 1}]}], AST).

parse_multi_element_tuple_test() ->
    Tokens = [{'{', 1}, {integer, 1, 1}, {',', 1}, {integer, 1, 2}, {',', 1}, {integer, 1, 3}, {'}', 1}],
    {ok, AST} = lx_parser:parse(Tokens),
    ?assertEqual([{tuple, 1, [{integer, 1, 1}, {integer, 1, 2}, {integer, 1, 3}]}], AST).

parse_mixed_tuple_test() ->
    Tokens = [{'{', 1}, {atom, 1, ok}, {',', 1}, {integer, 1, 1}, {',', 1}, {float, 1, 2.0}, {'}', 1}],
    {ok, AST} = lx_parser:parse(Tokens),
    ?assertEqual([{tuple, 1, [{atom, 1, ok}, {integer, 1, 1}, {float, 1, 2.0}]}], AST).

%% Test list parsing
parse_empty_list_test() ->
    Tokens = [{'[', 1}, {']', 1}],
    {ok, AST} = lx_parser:parse(Tokens),
    ?assertEqual([{list, 1, []}], AST).

parse_single_element_list_test() ->
    Tokens = [{'[', 1}, {integer, 1, 1}, {']', 1}],
    {ok, AST} = lx_parser:parse(Tokens),
    ?assertEqual([{list, 1, [{integer, 1, 1}]}], AST).

parse_multi_element_list_test() ->
    Tokens = [{'[', 1}, {integer, 1, 1}, {',', 1}, {integer, 1, 2}, {',', 1}, {integer, 1, 3}, {']', 1}],
    {ok, AST} = lx_parser:parse(Tokens),
    ?assertEqual([{list, 1, [{integer, 1, 1}, {integer, 1, 2}, {integer, 1, 3}]}], AST).

parse_mixed_list_test() ->
    Tokens = [{'[', 1}, {integer, 1, 1}, {',', 1}, {atom, 1, ok}, {',', 1}, {string, 1, "test"}, {']', 1}],
    {ok, AST} = lx_parser:parse(Tokens),
    ?assertEqual([{list, 1, [{integer, 1, 1}, {atom, 1, ok}, {string, 1, "test"}]}], AST).

%% Test map parsing
parse_empty_map_test() ->
    Tokens = [{'%', 1}, {'{', 1}, {'}', 1}],
    {ok, AST} = lx_parser:parse(Tokens),
    ?assertEqual([{map, 1, []}], AST).

parse_simple_map_test() ->
    Tokens = [{'%', 1}, {'{', 1}, {atom, 1, a}, {':', 1}, {integer, 1, 1}, {'}', 1}],
    {ok, AST} = lx_parser:parse(Tokens),
    ?assertEqual([{map, 1, [{map_entry, 1, {atom, 1, a}, {integer, 1, 1}}]}], AST).

parse_complex_map_test() ->
    Tokens = [{'%', 1}, {'{', 1},
              {atom, 1, a}, {':', 1}, {integer, 1, 1}, {',', 1},
              {string, 1, "b"}, {':', 1}, {integer, 1, 2}, {',', 1},
              {integer, 1, 123}, {':', 1}, {float, 1, 3.14},
              {'}', 1}],
    {ok, AST} = lx_parser:parse(Tokens),
    ExpectedMap = [{map_entry, 1, {atom, 1, a}, {integer, 1, 1}},
                   {map_entry, 1, {string, 1, "b"}, {integer, 1, 2}},
                   {map_entry, 1, {integer, 1, 123}, {float, 1, 3.14}}],
    ?assertEqual([{map, 1, ExpectedMap}], AST).

%% Test multiple expressions
parse_multiple_expressions_test() ->
    Tokens = [{integer, 1, 1}, {integer, 2, 2}, {integer, 3, 3}],
    {ok, AST} = lx_parser:parse(Tokens),
    ?assertEqual([{integer, 1, 1}, {integer, 2, 2}, {integer, 3, 3}], AST).

%% Test nested structures
parse_nested_tuple_test() ->
    Tokens = [{'{', 1}, {'{', 1}, {integer, 1, 1}, {'}', 1}, {',', 1}, {integer, 1, 2}, {'}', 1}],
    {ok, AST} = lx_parser:parse(Tokens),
    ?assertEqual([{tuple, 1, [{tuple, 1, [{integer, 1, 1}]}, {integer, 1, 2}]}], AST).

parse_nested_list_test() ->
    Tokens = [{'[', 1}, {'[', 1}, {integer, 1, 1}, {']', 1}, {',', 1}, {integer, 1, 2}, {']', 1}],
    {ok, AST} = lx_parser:parse(Tokens),
    ?assertEqual([{list, 1, [{list, 1, [{integer, 1, 1}]}, {integer, 1, 2}]}], AST).

%% Test error cases
parse_incomplete_tuple_test() ->
    Tokens = [{'{', 1}, {integer, 1, 1}],
    {error, _} = lx_parser:parse(Tokens).

parse_incomplete_list_test() ->
    Tokens = [{'[', 1}, {integer, 1, 1}],
    {error, _} = lx_parser:parse(Tokens).

parse_incomplete_map_test() ->
    Tokens = [{'%', 1}, {'{', 1}, {atom, 1, a}, {':', 1}, {integer, 1, 1}],
    {error, _} = lx_parser:parse(Tokens).

%% Test complex real-world example
parse_literals_file_test() ->
    % Simulate the tokens from the literals.lx file
    Tokens = [
        {integer, 2, 1},
        {float, 5, 2.0},
        {atom, 8, atom},
        {atom, 10, ok},
        {'{', 13}, {integer, 13, 1}, {'}', 13},
        {'{', 15}, {integer, 15, 1}, {',', 15}, {integer, 15, 2}, {'}', 15},
        {'{', 17}, {atom, 17, ok}, {',', 17}, {integer, 17, 1}, {',', 17}, {integer, 17, 2}, {',', 17}, {integer, 17, 3}, {'}', 17},
        {'[', 21}, {integer, 21, 1}, {']', 21},
        {'[', 23}, {integer, 23, 1}, {',', 23}, {integer, 23, 2}, {']', 23},
        {'[', 25}, {integer, 25, 1}, {',', 25}, {integer, 25, 2}, {',', 25}, {integer, 25, 3}, {',', 25}, {atom, 25, ok}, {']', 25},
        {'%', 29}, {'{', 29}, {atom, 29, a}, {':', 29}, {integer, 29, 1}, {',', 29}, {atom, 29, b}, {':', 29}, {integer, 29, 2}, {'}', 29}
    ],
    {ok, AST} = lx_parser:parse(Tokens),
    ?assert(is_list(AST)),
    ?assert(length(AST) > 0),
    % Verify we have the expected number of top-level expressions
    ?assertEqual(11, length(AST)).