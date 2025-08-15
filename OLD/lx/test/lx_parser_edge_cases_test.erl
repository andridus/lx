-module(lx_parser_edge_cases_test).
-include_lib("eunit/include/eunit.hrl").

%% Test suite for LX parser edge cases and additional validations
%% Tests edge cases and complex scenarios not covered in the main test suite

%% =============================================================================
%% Test setup and utilities
%% =============================================================================

setup() ->
    % Ensure parser is compiled
    case code:is_loaded(lx_parser) of
        false ->
            {ok, lx_parser} = compile:file("src/lx_parser.yrl");
        _ ->
            ok
    end,
    ok.

teardown(_) ->
    ok.

%% Helper function to parse LX code and return AST
parse_lx(Code) ->
    case lx_lexer:string(Code) of
        {ok, Tokens, _} ->
            case lx_parser:parse(Tokens) of
                {ok, AST} -> AST;
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason, _} ->
            {error, Reason}
    end.

%% =============================================================================
%% Deep nesting tests
%% =============================================================================

deep_nesting_test_() ->
    {setup, fun setup/0, fun teardown/1, [
        {"deeply nested tuples", fun test_deeply_nested_tuples/0},
        {"deeply nested lists", fun test_deeply_nested_lists/0},
        {"deeply nested maps", fun test_deeply_nested_maps/0},
        {"mixed deep nesting", fun test_mixed_deep_nesting/0}
    ]}.

test_deeply_nested_tuples() ->
    Code = "{{{1, 2}, {3, 4}}, {{5, 6}, {7, 8}}}",
    Expected = [{tuple, 1, [
        {tuple, 1, [
            {tuple, 1, [{integer, 1, 1}, {integer, 1, 2}]},
            {tuple, 1, [{integer, 1, 3}, {integer, 1, 4}]}
        ]},
        {tuple, 1, [
            {tuple, 1, [{integer, 1, 5}, {integer, 1, 6}]},
            {tuple, 1, [{integer, 1, 7}, {integer, 1, 8}]}
        ]}
    ]}],
    ?assertEqual(Expected, parse_lx(Code)).

test_deeply_nested_lists() ->
    Code = "[[[1, 2], [3, 4]], [[5, 6], [7, 8]]]",
    Expected = [{list, 1, [
        {list, 1, [
            {list, 1, [{integer, 1, 1}, {integer, 1, 2}]},
            {list, 1, [{integer, 1, 3}, {integer, 1, 4}]}
        ]},
        {list, 1, [
            {list, 1, [{integer, 1, 5}, {integer, 1, 6}]},
            {list, 1, [{integer, 1, 7}, {integer, 1, 8}]}
        ]}
    ]}],
    ?assertEqual(Expected, parse_lx(Code)).

test_deeply_nested_maps() ->
    Code = "%{outer: %{inner: %{deep: 42}}}",
    Expected = [{map, 1, [
        {map_entry, 1, {ident, 1, outer}, {map, 1, [
            {map_entry, 1, {ident, 1, inner}, {map, 1, [
                {map_entry, 1, {ident, 1, deep}, {integer, 1, 42}}
            ]}}
        ]}}
    ]}],
    ?assertEqual(Expected, parse_lx(Code)).

test_mixed_deep_nesting() ->
    Code = "%{data: [1, {nested: [2, 3]}, %{map: 4}]}",
    Expected = [{map, 1, [
        {map_entry, 1, {ident, 1, data}, {list, 1, [
            {integer, 1, 1},
            {tuple, 1, [{atom, 1, nested}, {list, 1, [{integer, 1, 2}, {integer, 1, 3}]}]},
            {map, 1, [{map_entry, 1, {ident, 1, map}, {integer, 1, 4}}]}
        ]}}
    ]}],
    ?assertEqual(Expected, parse_lx(Code)).

%% =============================================================================
%% Operator precedence tests
%% =============================================================================

operator_precedence_test_() ->
    {setup, fun setup/0, fun teardown/1, [
        {"arithmetic precedence", fun test_arithmetic_precedence/0},
        {"mixed operators", fun test_mixed_operators/0},
        {"parenthesized expressions", fun test_parenthesized_expressions/0}
    ]}.

test_arithmetic_precedence() ->
    Code = "1 + 2 * 3",
    Expected = [{operator, 1, [
        {integer, 1, 1},
        {operator, 1, [{integer, 1, 2}, {integer, 1, 3}]}
    ]}],
    ?assertEqual(Expected, parse_lx(Code)).

test_mixed_operators() ->
    Code = "a >= b and c < d",
    Expected = [{operator, 1, [
        {operator, 1, [{ident, 1, a}, {ident, 1, b}]},
        {operator, 1, [{ident, 1, c}, {ident, 1, d}]}
    ]}],
    ?assertEqual(Expected, parse_lx(Code)).

test_parenthesized_expressions() ->
    Code = "(1 + 2) * 3",
    Expected = [{operator, 1, [
        {tuple, 1, [{operator, 1, [{integer, 1, 1}, {integer, 1, 2}]}]},
        {integer, 1, 3}
    ]}],
    ?assertEqual(Expected, parse_lx(Code)).

%% =============================================================================
%% Function call complexity tests
%% =============================================================================

function_call_complexity_test_() ->
    {setup, fun setup/0, fun teardown/1, [
        {"nested function calls", fun test_nested_function_calls/0},
        {"function calls with complex args", fun test_function_calls_complex_args/0},
        {"chained function calls", fun test_chained_function_calls/0}
    ]}.

test_nested_function_calls() ->
    Code = "outer(inner(deep(42)))",
    Expected = [{call, 1, [outer, [{call, 1, [inner, [{call, 1, [deep, [{integer, 1, 42}]]}]]}]]}],
    ?assertEqual(Expected, parse_lx(Code)).

test_function_calls_complex_args() ->
    Code = "process(%{input: [1, 2, 3], config: {verbose: true, timeout: 5000}})",
    Expected = [{call, 1, [process, [{map, 1, [
        {map_entry, 1, {ident, 1, input}, {list, 1, [{integer, 1, 1}, {integer, 1, 2}, {integer, 1, 3}]}},
        {map_entry, 1, {ident, 1, config}, {tuple, 1, [
            {atom, 1, verbose}, {atom, 1, true},
            {atom, 1, timeout}, {integer, 1, 5000}
        ]}}
    ]}]]}],
    ?assertEqual(Expected, parse_lx(Code)).

test_chained_function_calls() ->
    Code = "data |> filter(fn) |> map(transform) |> reduce(accumulator)",
    Expected = [{operator, 1, [
        {operator, 1, [
            {operator, 1, [
                {ident, 1, data},
                {call, 1, [filter, [{ident, 1, fn}]]}
            ]},
            {call, 1, [map, [{ident, 1, transform}]]}
        ]},
        {call, 1, [reduce, [{ident, 1, accumulator}]]}
    ]}],
    ?assertEqual(Expected, parse_lx(Code)).

%% =============================================================================
%% Block expression complexity tests
%% =============================================================================

block_complexity_test_() ->
    {setup, fun setup/0, fun teardown/1, [
        {"nested blocks", fun test_nested_blocks/0},
        {"blocks with mixed expressions", fun test_blocks_mixed_expressions/0},
        {"blocks with function calls", fun test_blocks_function_calls/0}
    ]}.

test_nested_blocks() ->
    Code = "do\n  do\n    1 + 1\n  end\n  2 + 2\nend",
    Expected = [{block, 1, [
        {block, 2, [{operator, 3, [{integer, 3, 1}, {integer, 3, 1}]}]},
        {operator, 5, [{integer, 5, 2}, {integer, 5, 2}]}
    ]}],
    ?assertEqual(Expected, parse_lx(Code)).

test_blocks_mixed_expressions() ->
    Code = "do\n  config: %{timeout: 5000};\n  result = process(config);\n  {result, :ok}\nend",
    Expected = [{block, 1, [
        {tuple, 2, [{atom, 2, config}, {map, 2, [{map_entry, 2, {ident, 2, timeout}, {integer, 2, 5000}}]}]},
        {operator, 3, [{ident, 3, result}, {call, 3, [process, [{ident, 3, config}]]}]},
        {tuple, 4, [{ident, 4, result}, {atom, 4, ok}]}
    ]}],
    ?assertEqual(Expected, parse_lx(Code)).

test_blocks_function_calls() ->
    Code = "do\n  IO.puts(\"Starting\");\n  result = calculate(1, 2);\n  IO.puts(\"Result: \" <> to_string(result))\nend",
    Expected = [{block, 1, [
        {mfa, 2, [io, puts, [{string, 2, "Starting"}]]},
        {operator, 3, [{ident, 3, result}, {call, 3, [calculate, [{integer, 3, 1}, {integer, 3, 2}]]}]},
        {mfa, 4, [io, puts, [{operator, 4, [{string, 4, "Result: "}, {call, 4, [to_string, [{ident, 4, result}]]}]}]]}
    ]}],
    ?assertEqual(Expected, parse_lx(Code)).

%% =============================================================================
%% Whitespace and formatting tests
%% =============================================================================

whitespace_formatting_test_() ->
    {setup, fun setup/0, fun teardown/1, [
        {"extra whitespace", fun test_extra_whitespace/0},
        {"no whitespace", fun test_no_whitespace/0},
        {"mixed whitespace", fun test_mixed_whitespace/0}
    ]}.

test_extra_whitespace() ->
    Code = "  {  :ok  ,  42  }  ",
    Expected = [{tuple, 1, [{atom, 1, ok}, {integer, 1, 42}]}],
    ?assertEqual(Expected, parse_lx(Code)).

test_no_whitespace() ->
    Code = "{:ok,42}",
    Expected = [{tuple, 1, [{atom, 1, ok}, {integer, 1, 42}]}],
    ?assertEqual(Expected, parse_lx(Code)).

test_mixed_whitespace() ->
    Code = "%{a:1,b: 2,c :3}",
    Expected = [{map, 1, [
        {map_entry, 1, {ident, 1, a}, {integer, 1, 1}},
        {map_entry, 1, {ident, 1, b}, {integer, 1, 2}},
        {map_entry, 1, {ident, 1, c}, {integer, 1, 3}}
    ]}],
    ?assertEqual(Expected, parse_lx(Code)).

%% =============================================================================
%% Error recovery tests
%% =============================================================================

error_recovery_test_() ->
    {setup, fun setup/0, fun teardown/1, [
        {"malformed tuple", fun test_malformed_tuple/0},
        {"malformed list", fun test_malformed_list/0},
        {"malformed map", fun test_malformed_map/0},
        {"unexpected tokens", fun test_unexpected_tokens/0}
    ]}.

test_malformed_tuple() ->
    Code = "{1, 2,}",
    Result = parse_lx(Code),
    ?assertMatch({error, _}, Result).

test_malformed_list() ->
    Code = "[1, 2,]",
    Result = parse_lx(Code),
    ?assertMatch({error, _}, Result).

test_malformed_map() ->
    Code = "%{a: 1,}",
    Result = parse_lx(Code),
    ?assertMatch({error, _}, Result).

test_unexpected_tokens() ->
    Code = "1 + + 2",
    Result = parse_lx(Code),
    ?assertMatch({error, _}, Result).

%% =============================================================================
%% Performance stress tests
%% =============================================================================

performance_stress_test_() ->
    {setup, fun setup/0, fun teardown/1, [
        {"large tuple", fun test_large_tuple/0},
        {"large list", fun test_large_list/0},
        {"large map", fun test_large_map/0}
    ]}.

test_large_tuple() ->
    % Generate a tuple with 100 elements
    Elements = lists:seq(1, 100),
    TupleStr = "{" ++ string:join([integer_to_list(E) || E <- Elements], ", ") ++ "}",
    Result = parse_lx(TupleStr),
    ?assertMatch([{tuple, 1, _}], Result).

test_large_list() ->
    % Generate a list with 100 elements
    Elements = lists:seq(1, 100),
    ListStr = "[" ++ string:join([integer_to_list(E) || E <- Elements], ", ") ++ "]",
    Result = parse_lx(ListStr),
    ?assertMatch([{list, 1, _}], Result).

test_large_map() ->
    % Generate a map with 50 entries
    Entries = [{integer_to_list(I), I} || I <- lists:seq(1, 50)],
    MapStr = "%{" ++ string:join([K ++ ": " ++ integer_to_list(V) || {K, V} <- Entries], ", ") ++ "}",
    Result = parse_lx(MapStr),
    ?assertMatch([{map, 1, _}], Result).

%% =============================================================================
%% AST structure validation tests
%% =============================================================================

ast_structure_validation_test_() ->
    {setup, fun setup/0, fun teardown/1, [
        {"AST node types", fun test_ast_node_types/0},
        {"AST line numbers", fun test_ast_line_numbers/0},
        {"AST value consistency", fun test_ast_value_consistency/0}
    ]}.

test_ast_node_types() ->
    Code = "42",
    [AST] = parse_lx(Code),
    ?assertEqual(integer, element(1, AST)),
    ?assertEqual(1, element(2, AST)),
    ?assertEqual(42, element(3, AST)).

test_ast_line_numbers() ->
    Code = "1\n2\n3",
    AST = parse_lx(Code),
    ?assertEqual(3, length(AST)),
    ?assertEqual(1, element(2, lists:nth(1, AST))),
    ?assertEqual(2, element(2, lists:nth(2, AST))),
    ?assertEqual(3, element(2, lists:nth(3, AST))).

test_ast_value_consistency() ->
    Code = "{:ok, 42}",
    [AST] = parse_lx(Code),
    ?assertEqual(tuple, element(1, AST)),
    ?assertEqual(1, element(2, AST)),
    Elements = element(3, AST),
    ?assertEqual(2, length(Elements)),
    ?assertEqual({atom, 1, ok}, lists:nth(1, Elements)),
    ?assertEqual({integer, 1, 42}, lists:nth(2, Elements)).