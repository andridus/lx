-module(lx_macro_tests).
-include_lib("eunit/include/eunit.hrl").

%% Test basic macro definition and usage
basic_macro_test() ->
    Source = "defmacro a(body) do\n    {:a, %{}, [body]}\nend\n\na [1,2,3]",
    {ok, AST} = lx_compiler:compile_ast(Source),
    ?assert(is_list(AST)),
    ?assert(length(AST) > 0).

%% Test infix macro definition and usage
infix_macro_test() ->
    Source = "defmacro infix +(left, right) do\n    {'+', %{}, [left, right]}\nend\n\n1 + 2",
    {ok, AST} = lx_compiler:compile_ast(Source),
    ?assert(is_list(AST)),
    ?assert(length(AST) > 0).

%% Test macro with do block
macro_with_do_block_test() ->
    Source = "defmacro b(body) do\n    {:b, %{}, [body]}\nend\n\nb do\n  [1,2,3]\nend",
    {ok, AST} = lx_compiler:compile_ast(Source),
    ?assert(is_list(AST)),
    ?assert(length(AST) > 0).

%% Test multiple macros in same file
multiple_macros_test() ->
    Source = "defmacro a(body) do\n    {:a, %{}, [body]}\nend\n\ndefmacro infix +(left, right) do\n    {'+', %{}, [left, right]}\nend\n\ndefmacro tuple(left, right) do\n    {:tuple, %{}, [left, right]}\nend\n\na [1,2,3]\n1 + 2\ntuple 1 2",
    {ok, AST} = lx_compiler:compile_ast(Source),
    ?assert(is_list(AST)),
    ?assert(length(AST) > 0).

%% Test macro that generates Erlang code
macro_generating_erl_test() ->
    Source = "defmacro infix +(left, right) do\n    {:__erl__, {:op, 1, :'+', left, right}}\nend\n\n1 + 2",
    {ok, AST} = lx_compiler:compile_ast(Source),
    ?assert(is_list(AST)),
    ?assert(length(AST) > 0).

%% Test macro with list concatenation
list_concat_macro_test() ->
    Source = "defmacro infix ++(left, right) do\n    {:__erl__, {:op, 1, :'++', left, right}}\nend\n\n[1,2] ++ [3,4]",
    {ok, AST} = lx_compiler:compile_ast(Source),
    ?assert(is_list(AST)),
    ?assert(length(AST) > 0).

%% Test macro with multiple operators
multiple_operators_test() ->
    Source = "defmacro infix +(left, right) do\n    {:__erl__, {:op, 1, :'+', left, right}}\nend\n\ndefmacro infix -(left, right) do\n    {:__erl__, {:op, 1, :'-', left, right}}\nend\n\ndefmacro infix *(left, right) do\n    {:__erl__, {:op, 1, :'*', left, right}}\nend\n\n1 + 2\n3 - 1\n4 * 5",
    {ok, AST} = lx_compiler:compile_ast(Source),
    ?assert(is_list(AST)),
    ?assert(length(AST) > 0).

%% Test macro with complex body
complex_macro_body_test() ->
    Source = "defmacro a(body) do\n    {:a, %{}, [body]}\nend\n\na [1,2,3,4,5]",
    {ok, AST} = lx_compiler:compile_ast(Source),
    ?assert(is_list(AST)),
    ?assert(length(AST) > 0).

%% Test macro with nested structures
nested_macro_test() ->
    Source = "defmacro list(elements) do\n    {:list, %{}, [elements]}\nend\n\nlist [1,2,3,4,5]",
    {ok, AST} = lx_compiler:compile_ast(Source),
    ?assert(is_list(AST)),
    ?assert(length(AST) > 0).

%% Test macro with tuple generation
tuple_macro_test() ->
    Source = "defmacro tuple(left, right) do\n    {:tuple, %{}, [left, right]}\nend\n\ntuple 1 2",
    {ok, AST} = lx_compiler:compile_ast(Source),
    ?assert(is_list(AST)),
    ?assert(length(AST) > 0).

%% Test macro with comments
macro_with_comments_test() ->
    Source = "# Macro definition\ndefmacro a(body) do\n    {:a, %{}, [body]}\nend\n\n# Macro usage\na [1,2,3]",
    {ok, AST} = lx_compiler:compile_ast(Source),
    ?assert(is_list(AST)),
    ?assert(length(AST) > 0).

%% Test macro with whitespace handling
macro_whitespace_test() ->
    Source = "defmacro a(body) do\n    {:a, %{}, [body]}\nend\n\n  a  [1,2,3]  ",
    {ok, AST} = lx_compiler:compile_ast(Source),
    ?assert(is_list(AST)),
    ?assert(length(AST) > 0).

%% Test macro with multiple lines
multiline_macro_test() ->
    Source = "defmacro a(body) do\n    {:a, %{}, [body]}\nend\n\na [1,\n   2,\n   3]",
    {ok, AST} = lx_compiler:compile_ast(Source),
    ?assert(is_list(AST)),
    ?assert(length(AST) > 0).

%% Test macro with atom parameters
macro_atom_params_test() ->
    Source = "defmacro a(body) do\n    {:a, %{}, [body]}\nend\n\na :hello",
    {ok, AST} = lx_compiler:compile_ast(Source),
    ?assert(is_list(AST)),
    ?assert(length(AST) > 0).

%% Test macro with string parameters
macro_string_params_test() ->
    Source = "defmacro a(body) do\n    {:a, %{}, [body]}\nend\n\na \"hello\"",
    {ok, AST} = lx_compiler:compile_ast(Source),
    ?assert(is_list(AST)),
    ?assert(length(AST) > 0).

%% Test macro with tuple parameters
macro_tuple_params_test() ->
    Source = "defmacro a(body) do\n    {:a, %{}, [body]}\nend\n\na {1, 2, 3}",
    {ok, AST} = lx_compiler:compile_ast(Source),
    ?assert(is_list(AST)),
    ?assert(length(AST) > 0).

%% Test macro with map parameters
macro_map_params_test() ->
    Source = "defmacro a(body) do\n    {:a, %{}, [body]}\nend\n\na %{a: 1, b: 2}",
    {ok, AST} = lx_compiler:compile_ast(Source),
    ?assert(is_list(AST)),
    ?assert(length(AST) > 0).

%% Test macro with mixed parameters
macro_mixed_params_test() ->
    Source = "defmacro a(body) do\n    {:a, %{}, [body]}\nend\n\na [1, :hello, \"world\", {1, 2}, %{a: 1}]",
    {ok, AST} = lx_compiler:compile_ast(Source),
    ?assert(is_list(AST)),
    ?assert(length(AST) > 0).

%% Test macro with nested macro calls
nested_macro_calls_test() ->
    Source = "defmacro a(body) do\n    {:a, %{}, [body]}\nend\n\ndefmacro b(body) do\n    {:b, %{}, [body]}\nend\n\na b [1,2,3]",
    {ok, AST} = lx_compiler:compile_ast(Source),
    ?assert(is_list(AST)),
    ?assert(length(AST) > 0).

%% Test macro with operator precedence
operator_precedence_test() ->
    Source = "defmacro infix +(left, right) do\n    {:__erl__, {:op, 1, :'+', left, right}}\nend\n\ndefmacro infix *(left, right) do\n    {:__erl__, {:op, 1, :'*', left, right}}\nend\n\n1 + 2 * 3",
    {ok, AST} = lx_compiler:compile_ast(Source),
    ?assert(is_list(AST)),
    ?assert(length(AST) > 0).

%% Test macro with complex expressions
complex_expressions_test() ->
    Source = "defmacro infix +(left, right) do\n    {:__erl__, {:op, 1, :'+', left, right}}\nend\n\ndefmacro infix ++(left, right) do\n    {:__erl__, {:op, 1, :'++', left, right}}\nend\n\n[1,2] ++ [3,4] + [5,6]",
    {ok, AST} = lx_compiler:compile_ast(Source),
    ?assert(is_list(AST)),
    ?assert(length(AST) > 0).

%% Test macro with empty body
macro_empty_body_test() ->
    Source = "defmacro a(body) do\n    {:a, %{}, [body]}\nend\n\na []",
    {ok, AST} = lx_compiler:compile_ast(Source),
    ?assert(is_list(AST)),
    ?assert(length(AST) > 0).

%% Test macro with single element
macro_single_element_test() ->
    Source = "defmacro a(body) do\n    {:a, %{}, [body]}\nend\n\na [1]",
    {ok, AST} = lx_compiler:compile_ast(Source),
    ?assert(is_list(AST)),
    ?assert(length(AST) > 0).

%% Test macro with large list
macro_large_list_test() ->
    Source = "defmacro a(body) do\n    {:a, %{}, [body]}\nend\n\na [1,2,3,4,5,6,7,8,9,10]",
    {ok, AST} = lx_compiler:compile_ast(Source),
    ?assert(is_list(AST)),
    ?assert(length(AST) > 0).

%% Test macro with nested lists
macro_nested_lists_test() ->
    Source = "defmacro a(body) do\n    {:a, %{}, [body]}\nend\n\na [[1,2], [3,4], [5,6]]",
    {ok, AST} = lx_compiler:compile_ast(Source),
    ?assert(is_list(AST)),
    ?assert(length(AST) > 0).

%% Test macro with nested tuples
macro_nested_tuples_test() ->
    Source = "defmacro a(body) do\n    {:a, %{}, [body]}\nend\n\na [{1,2}, {3,4}, {5,6}]",
    {ok, AST} = lx_compiler:compile_ast(Source),
    ?assert(is_list(AST)),
    ?assert(length(AST) > 0).

%% Test macro with nested maps
macro_nested_maps_test() ->
    Source = "defmacro a(body) do\n    {:a, %{}, [body]}\nend\n\na [%{a: 1}, %{b: 2}, %{c: 3}]",
    {ok, AST} = lx_compiler:compile_ast(Source),
    ?assert(is_list(AST)),
    ?assert(length(AST) > 0).

%% Test macro with mixed nested structures
macro_mixed_nested_test() ->
    Source = "defmacro a(body) do\n    {:a, %{}, [body]}\nend\n\na [%{list: [1,2,3], tuple: {1,2}, atom: :hello}]",
    {ok, AST} = lx_compiler:compile_ast(Source),
    ?assert(is_list(AST)),
    ?assert(length(AST) > 0).

%% Test macro with multiple infix operators
multiple_infix_operators_test() ->
    Source = "defmacro infix +(left, right) do\n    {:__erl__, {:op, 1, :'+', left, right}}\nend\n\ndefmacro infix -(left, right) do\n    {:__erl__, {:op, 1, :'-', left, right}}\nend\n\ndefmacro infix *(left, right) do\n    {:__erl__, {:op, 1, :'*', left, right}}\nend\n\ndefmacro infix /(left, right) do\n    {:__erl__, {:op, 1, :'/', left, right}}\nend\n\n1 + 2 - 3 * 4 / 5",
    {ok, AST} = lx_compiler:compile_ast(Source),
    ?assert(is_list(AST)),
    ?assert(length(AST) > 0).

%% Test macro with list concatenation and arithmetic
list_and_arithmetic_test() ->
    Source = "defmacro infix +(left, right) do\n    {:__erl__, {:op, 1, :'+', left, right}}\nend\n\ndefmacro infix ++(left, right) do\n    {:__erl__, {:op, 1, :'++', left, right}}\nend\n\n[1,2] ++ [3,4] + [5,6]",
    {ok, AST} = lx_compiler:compile_ast(Source),
    ?assert(is_list(AST)),
    ?assert(length(AST) > 0).

%% Test macro with complex do block
complex_do_block_test() ->
    Source = "defmacro b(body) do\n    {:b, %{}, [body]}\nend\n\nb do\n  [1,2,3]\n  {4,5,6}\n  %{a: 1, b: 2}\nend",
    {ok, AST} = lx_compiler:compile_ast(Source),
    ?assert(is_list(AST)),
    ?assert(length(AST) > 0).

%% Test macro with empty do block
empty_do_block_test() ->
    Source = "defmacro b(body) do\n    {:b, %{}, [body]}\nend\n\nb do\nend",
    {ok, AST} = lx_compiler:compile_ast(Source),
    ?assert(is_list(AST)),
    ?assert(length(AST) > 0).

%% Test macro with single expression do block
single_do_block_test() ->
    Source = "defmacro b(body) do\n    {:b, %{}, [body]}\nend\n\nb do\n  42\nend",
    {ok, AST} = lx_compiler:compile_ast(Source),
    ?assert(is_list(AST)),
    ?assert(length(AST) > 0).

%% Test macro with nested do blocks
nested_do_blocks_test() ->
    Source = "defmacro b(body) do\n    {:b, %{}, [body]}\nend\n\nb do\n  b do\n    [1,2,3]\n  end\nend",
    {ok, AST} = lx_compiler:compile_ast(Source),
    ?assert(is_list(AST)),
    ?assert(length(AST) > 0).

%% Test macro with comments in do block
do_block_with_comments_test() ->
    Source = "defmacro b(body) do\n    {:b, %{}, [body]}\nend\n\nb do\n  # First expression\n  [1,2,3]\n  # Second expression\n  {4,5,6}\nend",
    {ok, AST} = lx_compiler:compile_ast(Source),
    ?assert(is_list(AST)),
    ?assert(length(AST) > 0).

%% Test macro with whitespace in do block
do_block_whitespace_test() ->
    Source = "defmacro b(body) do\n    {:b, %{}, [body]}\nend\n\nb do\n  \n  [1,2,3]\n  \n  {4,5,6}\n  \nend",
    {ok, AST} = lx_compiler:compile_ast(Source),
    ?assert(is_list(AST)),
    ?assert(length(AST) > 0).

%% Test macro with multiple lines in do block
multiline_do_block_test() ->
    Source = "defmacro b(body) do\n    {:b, %{}, [body]}\nend\n\nb do\n  [1,\n   2,\n   3]\nend",
    {ok, AST} = lx_compiler:compile_ast(Source),
    ?assert(is_list(AST)),
    ?assert(length(AST) > 0).

%% Test macro with complex nested structures in do block
complex_nested_do_block_test() ->
    Source = "defmacro b(body) do\n    {:b, %{}, [body]}\nend\n\nb do\n  [%{a: [1,2,3], b: {4,5,6}}, %{c: :hello, d: \"world\"}]\nend",
    {ok, AST} = lx_compiler:compile_ast(Source),
    ?assert(is_list(AST)),
    ?assert(length(AST) > 0).

%% Test macro with operators in do block
operators_in_do_block_test() ->
    Source = "defmacro infix +(left, right) do\n    {:__erl__, {:op, 1, :'+', left, right}}\nend\n\ndefmacro b(body) do\n    {:b, %{}, [body]}\nend\n\nb do\n  1 + 2\n  3 + 4\nend",
    {ok, AST} = lx_compiler:compile_ast(Source),
    ?assert(is_list(AST)),
    ?assert(length(AST) > 0).

%% Test macro with macro calls in do block
macro_calls_in_do_block_test() ->
    Source = "defmacro a(body) do\n    {:a, %{}, [body]}\nend\n\ndefmacro b(body) do\n    {:b, %{}, [body]}\nend\n\nb do\n  a [1,2,3]\n  a [4,5,6]\nend",
    {ok, AST} = lx_compiler:compile_ast(Source),
    ?assert(is_list(AST)),
    ?assert(length(AST) > 0).

%% Test macro with mixed content in do block
mixed_do_block_test() ->
    Source = "defmacro infix +(left, right) do\n    {:__erl__, {:op, 1, :'+', left, right}}\nend\n\ndefmacro a(body) do\n    {:a, %{}, [body]}\nend\n\ndefmacro b(body) do\n    {:b, %{}, [body]}\nend\n\nb do\n  1 + 2\n  a [3,4,5]\n  %{result: 42}\nend",
    {ok, AST} = lx_compiler:compile_ast(Source),
    ?assert(is_list(AST)),
    ?assert(length(AST) > 0).

%% Test macro with large do block
large_do_block_test() ->
    Source = "defmacro b(body) do\n    {:b, %{}, [body]}\nend\n\nb do\n  1\n  2\n  3\n  4\n  5\n  6\n  7\n  8\n  9\n  10\nend",
    {ok, AST} = lx_compiler:compile_ast(Source),
    ?assert(is_list(AST)),
    ?assert(length(AST) > 0).

%% Test macro with deep nesting
deep_nesting_test() ->
    Source = "defmacro a(body) do\n    {:a, %{}, [body]}\nend\n\ndefmacro b(body) do\n    {:b, %{}, [body]}\nend\n\ndefmacro c(body) do\n    {:c, %{}, [body]}\nend\n\na b c [1,2,3]",
    {ok, AST} = lx_compiler:compile_ast(Source),
    ?assert(is_list(AST)),
    ?assert(length(AST) > 0).

%% Test macro with complex operator expressions
complex_operator_expressions_test() ->
    Source = "defmacro infix +(left, right) do\n    {:__erl__, {:op, 1, :'+', left, right}}\nend\n\ndefmacro infix *(left, right) do\n    {:__erl__, {:op, 1, :'*', left, right}}\nend\n\ndefmacro infix ++(left, right) do\n    {:__erl__, {:op, 1, :'++', left, right}}\nend\n\n[1,2] ++ [3,4] + [5,6] * [7,8]",
    {ok, AST} = lx_compiler:compile_ast(Source),
    ?assert(is_list(AST)),
    ?assert(length(AST) > 0).

%% Test macro with all literal types
all_literal_types_test() ->
    Source = "defmacro a(body) do\n    {:a, %{}, [body]}\nend\n\na [1, 2.0, :hello, \"world\", {1,2}, [3,4], %{a: 1}]",
    {ok, AST} = lx_compiler:compile_ast(Source),
    ?assert(is_list(AST)),
    ?assert(length(AST) > 0).

%% Test macro with performance (large number of expressions)
performance_test() ->
    % Create a simple source with macro calls
    Source = "defmacro a(body) do\n    {:a, %{}, [body]}\nend\n\na [1]\na [2]\na [3]",
    {ok, AST} = lx_compiler:compile_ast(Source),
    ?assert(is_list(AST)),
    ?assert(length(AST) > 0).

%% Test macro with error handling (undefined macro)
undefined_macro_test() ->
    Source = "undefined_macro [1,2,3]",
    {error, _} = lx_compiler:compile_ast(Source).

%% Test macro with error handling (invalid syntax)
invalid_macro_syntax_test() ->
    Source = "defmacro a(body) do\n    {:a, %{}, [body]}\nend\n\na [1,2,3",
    {error, _} = lx_compiler:compile_ast(Source).

%% Test macro with error handling (unclosed do block)
unclosed_do_block_test() ->
    Source = "defmacro b(body) do\n    {:b, %{}, [body]}\nend\n\nb do\n  [1,2,3]",
    {error, _} = lx_compiler:compile_ast(Source).

%% Test macro with error handling (invalid operator)
invalid_operator_test() ->
    Source = "defmacro infix @(left, right) do\n    {:__erl__, {:op, 1, :'@', left, right}}\nend\n\n1 @ 2",
    {error, _} = lx_compiler:compile_ast(Source).