module main

// Test advanced type inference features

fn test_union_type_complex_concatenation() {
	// Test that complex union types are correctly unified in multiple concatenations
	lx_code := 'def concat_multiple_unions() do
[1, 2, 3] ++ ["hello", :ok] ++ [3.14, true] ++ [:atom, 42]
end'

	expected := '-module(test).
-export([concat_multiple_unions/0]).

-spec concat_multiple_unions() -> [integer() | binary() | atom() | float() | boolean()].
concat_multiple_unions() ->
    [1, 2, 3] ++ [<<"hello"/utf8>>, ok] ++ [3.14, true] ++ [atom, 42].
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_nested_union_concatenation() {
	// Test that nested concatenations preserve all union types
	lx_code := 'def nested_concat() do
([1, "hello"] ++ [3.14]) ++ ([:atom] ++ [true])
end'

	expected := '-module(test).
-export([nested_concat/0]).

-spec nested_concat() -> [integer() | binary() | float() | atom() | boolean()].
nested_concat() ->
    ([1, <<"hello"/utf8>>] ++ [3.14]) ++ ([atom] ++ [true]).
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_pattern_variable_binding_in_recursive_function() {
	// Test that variables from [head | tail] patterns are correctly bound and typed
	lx_code := 'def sum_list do
([]) -> 0
([(head :: integer) | tail]) -> head + sum_list(tail)
end'

	expected := '-module(test).
-export([sum_list/1]).

-spec sum_list(list()) -> integer().
sum_list([]) ->
    0;
sum_list([HEAD_1 | TAIL_2]) ->
    HEAD_1 + sum_list(TAIL_2).
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_pattern_variable_binding_with_type_annotations() {
	// Test that type annotations in list patterns are correctly handled
	lx_code := 'def process_typed_list do
([]) -> "empty"
([(head :: integer) | _tail]) -> head * 2
end'

	expected := '-module(test).
-export([process_typed_list/1]).

-spec process_typed_list([integer()]) -> binary() | integer().
process_typed_list([]) ->
    <<"empty"/utf8>>;
process_typed_list([HEAD_1 | _TAIL_2]) ->
    HEAD_1 * 2.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_recursive_function_type_inference() {
	// Test that recursive functions correctly infer return types from base cases
	lx_code := 'def countdown :: string do
(0) -> "done"
(n :: integer) -> countdown(n - 1)
end'

	expected := '-module(test).
-export([countdown/1]).

-spec countdown(any()) -> any().
countdown(0) ->
    <<"done"/utf8>>;
countdown(N_1) when is_integer(N_1) ->
    countdown(N_1 - 1).
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_list_element_type_annotations() {
	// Test that type annotations are rejected in invalid contexts (expression lists)
	lx_code := 'def typed_elements() do
[head :: integer] ++ [value :: string]
end'

	result := compile_lx(lx_code)
	// Should fail compilation due to invalid type annotation in expression context
	assert result == 'falha'
}

fn test_complex_pattern_matching_with_unions() {
	// Test that complex patterns with union types work correctly
	lx_code := 'def process_mixed_args do
(x :: integer) -> x * 2
(s :: string) -> s
(:ok) -> "success"
end'

	expected := '-module(test).
-export([process_mixed_args/1]).

-spec process_mixed_args((integer() | binary() | atom())) -> integer() | binary().
process_mixed_args(X_1) when is_integer(X_1) ->
    X_1 * 2;
process_mixed_args(S_2) when is_binary(S_2) ->
    S_2;
process_mixed_args(ok) ->
    <<"success"/utf8>>.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_empty_list_type_unification() {
	// Test that empty lists correctly unify with typed lists
	lx_code := 'def mix_with_empty() do
[] ++ [1, 2, 3] ++ ["hello"]
end'

	expected := '-module(test).
-export([mix_with_empty/0]).

-spec mix_with_empty() -> [any()].
mix_with_empty() ->
    [] ++ [1, 2, 3] ++ [<<"hello"/utf8>>].
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_list_cons_pattern_variable_scope() {
	// Test that variables in list cons patterns are correctly scoped
	lx_code := 'def first_element do
([head | _tail]) -> head
([]) -> :empty
end'

	expected := '-module(test).
-export([first_element/1]).

-spec first_element(list()) -> any().
first_element([HEAD_1 | _TAIL_2]) ->
    HEAD_1;
first_element([]) ->
    empty.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_multiple_union_operations() {
	// Test multiple operations that create union types
	lx_code := 'def complex_operations() do
result1 = [1] ++ ["hello"]
result2 = [3.14] ++ [:atom]
result1 ++ result2
end'

	expected := '-module(test).
-export([complex_operations/0]).

-spec complex_operations() -> [integer() | binary() | float() | atom()].
complex_operations() ->
    RESULT1_1 = [1] ++ [<<"hello"/utf8>>],
    RESULT2_2 = [3.14] ++ [atom],
    RESULT1_1 ++ RESULT2_2.
'

	result := compile_lx(lx_code)
	assert result == expected
}
