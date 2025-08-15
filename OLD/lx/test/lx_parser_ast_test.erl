-module(lx_parser_ast_test).
-include_lib("eunit/include/eunit.hrl").

%% Test suite for LX parser AST generation
%% Validates that the parser correctly generates AST for all valid LX syntax examples

%% =============================================================================
%% Test setup and utilities
%% =============================================================================

setup() ->
    % Ensure parser is compiled
    case code:is_loaded(lx_parser) of
        false ->
            case compile:file("src/lx_parser.yrl") of
                {ok, lx_parser} -> ok;
                {error, _} ->
                    % Try to compile the generated .erl file
                    case compile:file("src/lx_parser.erl") of
                        {ok, lx_parser} -> ok;
                        {error, Reason} -> {error, Reason}
                    end
            end;
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
%% Literals tests
%% =============================================================================

literals_test_() ->
    {setup, fun setup/0, fun teardown/1, [
        {"atom literal", fun test_atom_literal/0},
        {"integer literal", fun test_integer_literal/0},
        {"float literal", fun test_float_literal/0},
        {"string literal", fun test_string_literal/0}
    ]}.

test_atom_literal() ->
    Code = ":something",
    Expected = [{atom, 1, something}],
    ?assertEqual(Expected, parse_lx(Code)).

test_integer_literal() ->
    Code = "123",
    Expected = [{integer, 1, 123}],
    ?assertEqual(Expected, parse_lx(Code)).

test_float_literal() ->
    Code = "3.14",
    Expected = [{float, 1, 3.14}],
    ?assertEqual(Expected, parse_lx(Code)).

test_string_literal() ->
    Code = "\"hello\"",
    Expected = [{string, 1, "hello"}],
    ?assertEqual(Expected, parse_lx(Code)).

%% =============================================================================
%% Tuples tests
%% =============================================================================

tuples_test_() ->
    {setup, fun setup/0, fun teardown/1, [
        {"empty tuple", fun test_empty_tuple/0},
        {"single element tuple", fun test_single_element_tuple/0},
        {"multiple elements tuple", fun test_multiple_elements_tuple/0},
        {"tuple with atom", fun test_tuple_with_atom/0}
    ]}.

test_empty_tuple() ->
    Code = "{}",
    Expected = [{tuple, 1, []}],
    ?assertEqual(Expected, parse_lx(Code)).

test_single_element_tuple() ->
    Code = "{1}",
    Expected = [{tuple, 1, [{integer, 1, 1}]}],
    ?assertEqual(Expected, parse_lx(Code)).

test_multiple_elements_tuple() ->
    Code = "{1, 2}",
    Expected = [{tuple, 1, [{integer, 1, 1}, {integer, 1, 2}]}],
    ?assertEqual(Expected, parse_lx(Code)).

test_tuple_with_atom() ->
    Code = "{:ok, 42}",
    Expected = [{tuple, 1, [{atom, 1, ok}, {integer, 1, 42}]}],
    ?assertEqual(Expected, parse_lx(Code)).

%% =============================================================================
%% Lists tests
%% =============================================================================

lists_test_() ->
    {setup, fun setup/0, fun teardown/1, [
        {"empty list", fun test_empty_list/0},
        {"single element list", fun test_single_element_list/0},
        {"multiple elements list", fun test_multiple_elements_list/0},
        {"list with atoms", fun test_list_with_atoms/0}
    ]}.

test_empty_list() ->
    Code = "[]",
    Expected = [{list, 1, []}],
    ?assertEqual(Expected, parse_lx(Code)).

test_single_element_list() ->
    Code = "[1]",
    Expected = [{list, 1, [{integer, 1, 1}]}],
    ?assertEqual(Expected, parse_lx(Code)).

test_multiple_elements_list() ->
    Code = "[1, 2, 3]",
    Expected = [{list, 1, [{integer, 1, 1}, {integer, 1, 2}, {integer, 1, 3}]}],
    ?assertEqual(Expected, parse_lx(Code)).

test_list_with_atoms() ->
    Code = "[:a, :b, :c]",
    Expected = [{list, 1, [{atom, 1, a}, {atom, 1, b}, {atom, 1, c}]}],
    ?assertEqual(Expected, parse_lx(Code)).

%% =============================================================================
%% Maps tests
%% =============================================================================

maps_test_() ->
    {setup, fun setup/0, fun teardown/1, [
        {"empty map", fun test_empty_map/0},
        {"single entry map", fun test_single_entry_map/0},
        {"multiple entries map", fun test_multiple_entries_map/0},
        {"map with atom keys", fun test_map_with_atom_keys/0}
    ]}.

test_empty_map() ->
    Code = "%{}",
    Expected = [{map, 1, []}],
    ?assertEqual(Expected, parse_lx(Code)).

test_single_entry_map() ->
    Code = "%{a: 1}",
    Expected = [{map, 1, [{map_entry, 1, {ident, 1, a}, {integer, 1, 1}}]}],
    ?assertEqual(Expected, parse_lx(Code)).

test_multiple_entries_map() ->
    Code = "%{a: 1, b: 2}",
    Expected = [{map, 1, [
        {map_entry, 1, {ident, 1, a}, {integer, 1, 1}},
        {map_entry, 1, {ident, 1, b}, {integer, 1, 2}}
    ]}],
    ?assertEqual(Expected, parse_lx(Code)).

test_map_with_atom_keys() ->
    Code = "%{:ok: \"done\", :error: \"fail\"}",
    Expected = [{map, 1, [
        {map_entry, 1, {atom, 1, ok}, {string, 1, "done"}},
        {map_entry, 1, {atom, 1, error}, {string, 1, "fail"}}
    ]}],
    ?assertEqual(Expected, parse_lx(Code)).

%% =============================================================================
%% Function calls tests
%% =============================================================================

function_calls_test_() ->
    {setup, fun setup/0, fun teardown/1, [
        {"simple function call", fun test_simple_function_call/0},
        {"function call with multiple args", fun test_function_call_multiple_args/0},
        {"module function call", fun test_module_function_call/0}
    ]}.

test_simple_function_call() ->
    Code = "sum(1, 2)",
    Expected = [{call, 1, [sum, [{integer, 1, 1}, {integer, 1, 2}]]}],
    ?assertEqual(Expected, parse_lx(Code)).

test_function_call_multiple_args() ->
    Code = "to_string(123)",
    Expected = [{call, 1, [to_string, [{integer, 1, 123}]]}],
    ?assertEqual(Expected, parse_lx(Code)).

test_module_function_call() ->
    Code = "Map.get(my_map, :key)",
    Expected = [{mfa, 1, [map, get, [{ident, 1, my_map}, {atom, 1, key}]]}],
    ?assertEqual(Expected, parse_lx(Code)).

%% =============================================================================
%% Operator expressions tests
%% =============================================================================

operator_expressions_test_() ->
    {setup, fun setup/0, fun teardown/1, [
        {"arithmetic operator", fun test_arithmetic_operator/0},
        {"comparison operator", fun test_comparison_operator/0},
        {"pipe operator", fun test_pipe_operator/0},
        {"concatenation operator", fun test_concatenation_operator/0}
    ]}.

test_arithmetic_operator() ->
    Code = "1 + 2",
    Expected = [{operator, 1, [{integer, 1, 1}, {integer, 1, 2}]}],
    ?assertEqual(Expected, parse_lx(Code)).

test_comparison_operator() ->
    Code = "4 >= 3",
    Expected = [{operator, 1, [{integer, 1, 4}, {integer, 1, 3}]}],
    ?assertEqual(Expected, parse_lx(Code)).

test_pipe_operator() ->
    Code = ":ok |> IO.inspect()",
    Expected = [{operator, 1, [{atom, 1, ok}, {mfa, 1, [io, inspect, []]}]}],
    ?assertEqual(Expected, parse_lx(Code)).

test_concatenation_operator() ->
    Code = "\"hello\" <> \"world\"",
    Expected = [{operator, 1, [{string, 1, "hello"}, {string, 1, "world"}]}],
    ?assertEqual(Expected, parse_lx(Code)).

%% =============================================================================
%% Keyword pairs tests
%% =============================================================================

keyword_pairs_test_() ->
    {setup, fun setup/0, fun teardown/1, [
        {"simple keyword pair", fun test_simple_keyword_pair/0},
        {"keyword pair with string", fun test_keyword_pair_with_string/0}
    ]}.

test_simple_keyword_pair() ->
    Code = "foo: 123",
    Expected = [{tuple, 1, [{atom, 1, foo}, {integer, 1, 123}]}],
    ?assertEqual(Expected, parse_lx(Code)).

test_keyword_pair_with_string() ->
    Code = "name: \"chatgpt\"",
    Expected = [{tuple, 1, [{atom, 1, name}, {string, 1, "chatgpt"}]}],
    ?assertEqual(Expected, parse_lx(Code)).

%% =============================================================================
%% Module function calls (MFA) tests
%% =============================================================================

mfa_calls_test_() ->
    {setup, fun setup/0, fun teardown/1, [
        {"empty MFA call", fun test_empty_mfa_call/0},
        {"MFA call with single arg", fun test_mfa_call_single_arg/0},
        {"MFA call with keyword args", fun test_mfa_call_keyword_args/0}
    ]}.

test_empty_mfa_call() ->
    Code = "IO.puts()",
    Expected = [{mfa, 1, [io, puts, []]}],
    ?assertEqual(Expected, parse_lx(Code)).

test_mfa_call_single_arg() ->
    Code = "IO.puts(\"hello\")",
    Expected = [{mfa, 1, [io, puts, [{string, 1, "hello"}]]}],
    ?assertEqual(Expected, parse_lx(Code)).

test_mfa_call_keyword_args() ->
    Code = "IO.inspect(value, label: \"value\")",
    Expected = [{mfa, 1, [io, inspect, [
        {ident, 1, value},
        {tuple, 1, [{atom, 1, label}, {string, 1, "value"}]}
    ]]}],
    ?assertEqual(Expected, parse_lx(Code)).

%% =============================================================================
%% Do/end blocks tests
%% =============================================================================

do_end_blocks_test_() ->
    {setup, fun setup/0, fun teardown/1, [
        {"empty do/end block", fun test_empty_do_end_block/0},
        {"single expression do/end block", fun test_single_expression_do_end_block/0},
        {"multiple expressions do/end block", fun test_multiple_expressions_do_end_block/0}
    ]}.

test_empty_do_end_block() ->
    Code = "do\nend",
    Expected = [{block, 1, []}],
    ?assertEqual(Expected, parse_lx(Code)).

test_single_expression_do_end_block() ->
    Code = "do\n  1 + 1\nend",
    Expected = [{block, 1, [{operator, 2, [{integer, 2, 1}, {integer, 2, 1}]}]}],
    ?assertEqual(Expected, parse_lx(Code)).

test_multiple_expressions_do_end_block() ->
    Code = "do\n  a: 1;\n  b: 2\nend",
    Expected = [{block, 1, [
        {tuple, 2, [{atom, 2, a}, {integer, 2, 1}]},
        {tuple, 3, [{atom, 3, b}, {integer, 3, 2}]}
    ]}],
    ?assertEqual(Expected, parse_lx(Code)).

%% =============================================================================
%% Complex expressions tests
%% =============================================================================

complex_expressions_test_() ->
    {setup, fun setup/0, fun teardown/1, [
        {"nested expressions", fun test_nested_expressions/0},
        {"mixed data structures", fun test_mixed_data_structures/0},
        {"complex function calls", fun test_complex_function_calls/0}
    ]}.

test_nested_expressions() ->
    Code = "{:ok, [1, 2, 3]}",
    Expected = [{tuple, 1, [
        {atom, 1, ok},
        {list, 1, [{integer, 1, 1}, {integer, 1, 2}, {integer, 1, 3}]}
    ]}],
    ?assertEqual(Expected, parse_lx(Code)).

test_mixed_data_structures() ->
    Code = "%{status: :ok, data: [1, 2], meta: {count: 2}}",
    Expected = [{map, 1, [
        {map_entry, 1, {ident, 1, status}, {atom, 1, ok}},
        {map_entry, 1, {ident, 1, data}, {list, 1, [{integer, 1, 1}, {integer, 1, 2}]}},
        {map_entry, 1, {ident, 1, meta}, {tuple, 1, [{atom, 1, count}, {integer, 1, 2}]}}
    ]}],
    ?assertEqual(Expected, parse_lx(Code)).

test_complex_function_calls() ->
    Code = "process_data(%{input: [1, 2, 3], options: {verbose: true}})",
    Expected = [{call, 1, [process_data, [{map, 1, [
        {map_entry, 1, {ident, 1, input}, {list, 1, [{integer, 1, 1}, {integer, 1, 2}, {integer, 1, 3}]}},
        {map_entry, 1, {ident, 1, options}, {tuple, 1, [{atom, 1, verbose}, {atom, 1, true}]}}
    ]}]]}],
    ?assertEqual(Expected, parse_lx(Code)).

%% =============================================================================
%% Error handling tests
%% =============================================================================

error_handling_test_() ->
    {setup, fun setup/0, fun teardown/1, [
        {"invalid syntax", fun test_invalid_syntax/0},
        {"unclosed bracket", fun test_unclosed_bracket/0},
        {"missing comma", fun test_missing_comma/0}
    ]}.

test_invalid_syntax() ->
    Code = "{1,}",
    Result = parse_lx(Code),
    ?assertMatch({error, _}, Result).

test_unclosed_bracket() ->
    Code = "[1, 2, 3",
    Result = parse_lx(Code),
    ?assertMatch({error, _}, Result).

test_missing_comma() ->
    Code = "{1 2}",
    Result = parse_lx(Code),
    ?assertMatch({error, _}, Result).

%% =============================================================================
%% Integration tests
%% =============================================================================

integration_test_() ->
    {setup, fun setup/0, fun teardown/1, [
        {"complete program with multiple expressions", fun test_complete_program/0},
        {"real-world example", fun test_real_world_example/0}
    ]}.

test_complete_program() ->
    Code = "result = calculate(1, 2)\nstatus = :ok\n{result, status}",
    Expected = [
        {operator, 1, [{ident, 1, result}, {call, 1, [calculate, [{integer, 1, 1}, {integer, 1, 2}]]}]},
        {operator, 2, [{ident, 2, status}, {atom, 2, ok}]},
        {tuple, 3, [{ident, 3, result}, {ident, 3, status}]}
    ],
    ?assertEqual(Expected, parse_lx(Code)).

test_real_world_example() ->
    Code = "config = %{api_key: \"abc123\", timeout: 5000}\nresponse = make_request(config)",
    Expected = [
        {operator, 1, [{ident, 1, config}, {map, 1, [
            {map_entry, 1, {ident, 1, api_key}, {string, 1, "abc123"}},
            {map_entry, 1, {ident, 1, timeout}, {integer, 1, 5000}}
        ]}]},
        {operator, 2, [{ident, 2, response}, {call, 2, [make_request, [{ident, 2, config}]]}]}
    ],
    ?assertEqual(Expected, parse_lx(Code)).