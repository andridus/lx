-module(lx_parser_documentation_examples_test).
-include_lib("eunit/include/eunit.hrl").

%% Test suite for LX parser documentation examples
%% Validates that the parser correctly handles all examples from the grammar documentation

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
%% Literals examples from documentation
%% =============================================================================

literals_documentation_test_() ->
    {setup, fun setup/0, fun teardown/1, [
        {"atom literal - :something", fun test_atom_something/0},
        {"integer literal - 123", fun test_integer_123/0},
        {"float literal - 3.14", fun test_float_314/0},
        {"string literal - \"hello\"", fun test_string_hello/0},
        {"quoted atom - 'abc'", fun test_quoted_atom_abc/0}
    ]}.

test_atom_something() ->
    Code = ":something",
    Expected = [{atom, 1, something}],
    ?assertEqual(Expected, parse_lx(Code)).

test_integer_123() ->
    Code = "123",
    Expected = [{integer, 1, 123}],
    ?assertEqual(Expected, parse_lx(Code)).

test_float_314() ->
    Code = "3.14",
    Expected = [{float, 1, 3.14}],
    ?assertEqual(Expected, parse_lx(Code)).

test_string_hello() ->
    Code = "\"hello\"",
    Expected = [{string, 1, "hello"}],
    ?assertEqual(Expected, parse_lx(Code)).

test_quoted_atom_abc() ->
    Code = "'abc'",
    Expected = [{atom, 1, abc}],
    ?assertEqual(Expected, parse_lx(Code)).

%% =============================================================================
%% Tuples examples from documentation
%% =============================================================================

tuples_documentation_test_() ->
    {setup, fun setup/0, fun teardown/1, [
        {"empty tuple - {}", fun test_empty_tuple_doc/0},
        {"single element - {1}", fun test_single_element_tuple_doc/0},
        {"multiple elements - {1, 2}", fun test_multiple_elements_tuple_doc/0},
        {"with atom - {:ok, 42}", fun test_tuple_with_atom_doc/0}
    ]}.

test_empty_tuple_doc() ->
    Code = "{}",
    Expected = [{tuple, 1, []}],
    ?assertEqual(Expected, parse_lx(Code)).

test_single_element_tuple_doc() ->
    Code = "{1}",
    Expected = [{tuple, 1, [{integer, 1, 1}]}],
    ?assertEqual(Expected, parse_lx(Code)).

test_multiple_elements_tuple_doc() ->
    Code = "{1, 2}",
    Expected = [{tuple, 1, [{integer, 1, 1}, {integer, 1, 2}]}],
    ?assertEqual(Expected, parse_lx(Code)).

test_tuple_with_atom_doc() ->
    Code = "{:ok, 42}",
    Expected = [{tuple, 1, [{atom, 1, ok}, {integer, 1, 42}]}],
    ?assertEqual(Expected, parse_lx(Code)).

%% =============================================================================
%% Lists examples from documentation
%% =============================================================================

lists_documentation_test_() ->
    {setup, fun setup/0, fun teardown/1, [
        {"empty list - []", fun test_empty_list_doc/0},
        {"single element - [1]", fun test_single_element_list_doc/0},
        {"multiple elements - [1, 2, 3]", fun test_multiple_elements_list_doc/0},
        {"with atoms - [:a, :b, :c]", fun test_list_with_atoms_doc/0}
    ]}.

test_empty_list_doc() ->
    Code = "[]",
    Expected = [{list, 1, []}],
    ?assertEqual(Expected, parse_lx(Code)).

test_single_element_list_doc() ->
    Code = "[1]",
    Expected = [{list, 1, [{integer, 1, 1}]}],
    ?assertEqual(Expected, parse_lx(Code)).

test_multiple_elements_list_doc() ->
    Code = "[1, 2, 3]",
    Expected = [{list, 1, [{integer, 1, 1}, {integer, 1, 2}, {integer, 1, 3}]}],
    ?assertEqual(Expected, parse_lx(Code)).

test_list_with_atoms_doc() ->
    Code = "[:a, :b, :c]",
    Expected = [{list, 1, [{atom, 1, a}, {atom, 1, b}, {atom, 1, c}]}],
    ?assertEqual(Expected, parse_lx(Code)).

%% =============================================================================
%% Maps examples from documentation
%% =============================================================================

maps_documentation_test_() ->
    {setup, fun setup/0, fun teardown/1, [
        {"empty map - %{}", fun test_empty_map_doc/0},
        {"single entry - %{a: 1}", fun test_single_entry_map_doc/0},
        {"multiple entries - %{a: 1, b: 2}", fun test_multiple_entries_map_doc/0},
        {"with atom keys - %{:ok: \"done\", :error: \"fail\"}", fun test_map_with_atom_keys_doc/0}
    ]}.

test_empty_map_doc() ->
    Code = "%{}",
    Expected = [{map, 1, []}],
    ?assertEqual(Expected, parse_lx(Code)).

test_single_entry_map_doc() ->
    Code = "%{a: 1}",
    Expected = [{map, 1, [{map_entry, 1, {ident, 1, a}, {integer, 1, 1}}]}],
    ?assertEqual(Expected, parse_lx(Code)).

test_multiple_entries_map_doc() ->
    Code = "%{a: 1, b: 2}",
    Expected = [{map, 1, [
        {map_entry, 1, {ident, 1, a}, {integer, 1, 1}},
        {map_entry, 1, {ident, 1, b}, {integer, 1, 2}}
    ]}],
    ?assertEqual(Expected, parse_lx(Code)).

test_map_with_atom_keys_doc() ->
    Code = "%{:ok: \"done\", :error: \"fail\"}",
    Expected = [{map, 1, [
        {map_entry, 1, {atom, 1, ok}, {string, 1, "done"}},
        {map_entry, 1, {atom, 1, error}, {string, 1, "fail"}}
    ]}],
    ?assertEqual(Expected, parse_lx(Code)).

%% =============================================================================
%% Function calls examples from documentation
%% =============================================================================

function_calls_documentation_test_() ->
    {setup, fun setup/0, fun teardown/1, [
        {"simple call - sum(1, 2)", fun test_simple_call_doc/0},
        {"string conversion - to_string(123)", fun test_to_string_call_doc/0},
        {"module function - Map.get(my_map, :key)", fun test_module_function_call_doc/0}
    ]}.

test_simple_call_doc() ->
    Code = "sum(1, 2)",
    Expected = [{call, 1, [sum, [{integer, 1, 1}, {integer, 1, 2}]]}],
    ?assertEqual(Expected, parse_lx(Code)).

test_to_string_call_doc() ->
    Code = "to_string(123)",
    Expected = [{call, 1, [to_string, [{integer, 1, 123}]]}],
    ?assertEqual(Expected, parse_lx(Code)).

test_module_function_call_doc() ->
    Code = "Map.get(my_map, :key)",
    Expected = [{mfa, 1, [map, get, [{ident, 1, my_map}, {atom, 1, key}]]}],
    ?assertEqual(Expected, parse_lx(Code)).

%% =============================================================================
%% Operator expressions examples from documentation
%% =============================================================================

operator_expressions_documentation_test_() ->
    {setup, fun setup/0, fun teardown/1, [
        {"arithmetic - 1 + 2", fun test_arithmetic_doc/0},
        {"comparison - 4 >= 3", fun test_comparison_doc/0},
        {"pipe operator - :ok |> IO.inspect()", fun test_pipe_operator_doc/0},
        {"concatenation - \"hello\" <> \"world\"", fun test_concatenation_doc/0}
    ]}.

test_arithmetic_doc() ->
    Code = "1 + 2",
    Expected = [{operator, 1, [{integer, 1, 1}, {integer, 1, 2}]}],
    ?assertEqual(Expected, parse_lx(Code)).

test_comparison_doc() ->
    Code = "4 >= 3",
    Expected = [{operator, 1, [{integer, 1, 4}, {integer, 1, 3}]}],
    ?assertEqual(Expected, parse_lx(Code)).

test_pipe_operator_doc() ->
    Code = ":ok |> IO.inspect()",
    Expected = [{operator, 1, [{atom, 1, ok}, {mfa, 1, [io, inspect, []]}]}],
    ?assertEqual(Expected, parse_lx(Code)).

test_concatenation_doc() ->
    Code = "\"hello\" <> \"world\"",
    Expected = [{operator, 1, [{string, 1, "hello"}, {string, 1, "world"}]}],
    ?assertEqual(Expected, parse_lx(Code)).

%% =============================================================================
%% Keyword pairs examples from documentation
%% =============================================================================

keyword_pairs_documentation_test_() ->
    {setup, fun setup/0, fun teardown/1, [
        {"simple pair - foo: 123", fun test_simple_keyword_pair_doc/0},
        {"string value - name: \"chatgpt\"", fun test_keyword_pair_string_doc/0}
    ]}.

test_simple_keyword_pair_doc() ->
    Code = "foo: 123",
    Expected = [{tuple, 1, [{atom, 1, foo}, {integer, 1, 123}]}],
    ?assertEqual(Expected, parse_lx(Code)).

test_keyword_pair_string_doc() ->
    Code = "name: \"chatgpt\"",
    Expected = [{tuple, 1, [{atom, 1, name}, {string, 1, "chatgpt"}]}],
    ?assertEqual(Expected, parse_lx(Code)).

%% =============================================================================
%% MFA calls examples from documentation
%% =============================================================================

mfa_calls_documentation_test_() ->
    {setup, fun setup/0, fun teardown/1, [
        {"empty call - IO.puts()", fun test_empty_mfa_call_doc/0},
        {"with arg - IO.puts(\"hello\")", fun test_mfa_call_with_arg_doc/0},
        {"with keyword args - IO.inspect(value, label: \"value\")", fun test_mfa_call_keyword_args_doc/0}
    ]}.

test_empty_mfa_call_doc() ->
    Code = "IO.puts()",
    Expected = [{mfa, 1, [io, puts, []]}],
    ?assertEqual(Expected, parse_lx(Code)).

test_mfa_call_with_arg_doc() ->
    Code = "IO.puts(\"hello\")",
    Expected = [{mfa, 1, [io, puts, [{string, 1, "hello"}]]}],
    ?assertEqual(Expected, parse_lx(Code)).

test_mfa_call_keyword_args_doc() ->
    Code = "IO.inspect(value, label: \"value\")",
    Expected = [{mfa, 1, [io, inspect, [
        {ident, 1, value},
        {tuple, 1, [{atom, 1, label}, {string, 1, "value"}]}
    ]]}],
    ?assertEqual(Expected, parse_lx(Code)).

%% =============================================================================
%% Do/end blocks examples from documentation
%% =============================================================================

do_end_blocks_documentation_test_() ->
    {setup, fun setup/0, fun teardown/1, [
        {"simple block - do 1 + 1 end", fun test_simple_block_doc/0},
        {"multiple expressions - do a: 1; b: 2 end", fun test_multiple_expressions_block_doc/0}
    ]}.

test_simple_block_doc() ->
    Code = "do\n  1 + 1\nend",
    Expected = [{block, 1, [{operator, 2, [{integer, 2, 1}, {integer, 2, 1}]}]}],
    ?assertEqual(Expected, parse_lx(Code)).

test_multiple_expressions_block_doc() ->
    Code = "do\n  a: 1;\n  b: 2\nend",
    Expected = [{block, 1, [
        {tuple, 2, [{atom, 2, a}, {integer, 2, 1}]},
        {tuple, 3, [{atom, 3, b}, {integer, 3, 2}]}
    ]}],
    ?assertEqual(Expected, parse_lx(Code)).