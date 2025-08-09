module main

// Test edge cases and regression tests

fn test_any_type_propagation_in_unification() {
	// Test that 'any' types are handled correctly in unification
	lx_code := 'def check_status do
(:ok) -> "success"
(:error) -> "failure"
(code) -> "unknown"
end'

	expected := '-module(test).
-export([check_status/1]).

-spec check_status(any()) -> binary().
check_status(ok) ->
    <<"success"/utf8>>;
check_status(error) ->
    <<"failure"/utf8>>;
check_status(CODE_1) ->
    <<"unknown"/utf8>>.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_process_value_union_types() {
	// Test that function heads with different types create proper union
	lx_code := 'def process_value do
(0) -> "zero"
(n :: integer) -> n * 2
end'

	expected := '-module(test).
-export([process_value/1]).

-spec process_value(integer()) -> binary() | integer().
process_value(0) ->
    <<"zero"/utf8>>;
process_value(N_1) ->
    N_1 * 2.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_list_pattern_matching_empty_vs_cons() {
	// Test that empty list and cons patterns work together
	lx_code := 'def process_list do
([]) -> "empty"
([head | tail]) -> "non_empty"
end'

	expected := '-module(test).
-export([process_list/1]).

-spec process_list(any()) -> binary().
process_list([]) ->
    <<"empty"/utf8>>;
process_list([HEAD_1 | TAIL_2]) ->
    <<"non_empty"/utf8>>.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_recursive_function_base_case_priority() {
	// Test that base cases drive return type inference in recursive functions
	lx_code := 'def countdown :: string do
(0) -> "done"
(n :: integer) -> countdown(n - 1)
end'

	expected := '-module(test).
-export([countdown/1]).

-spec countdown(integer()) -> binary().
countdown(0) ->
    <<"done"/utf8>>;
countdown(N_1) ->
    countdown(N_1 - 1).
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_list_element_type_annotation_extraction() {
	// Test that type annotations are correctly extracted from list elements
	lx_code := 'def process_list do
([]) -> "empty"
([head :: integer]) -> "non_empty"
end'

	expected := '-module(test).
-export([process_list/1]).

-spec process_list([integer()]) -> binary().
process_list([]) ->
    <<"empty"/utf8>>;
process_list([HEAD_1]) ->
    <<"non_empty"/utf8>>.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_multiple_concatenation_preserves_all_types() {
	// Test the specific case that was failing: multiple concatenations
	lx_code := 'def concat_mixed() do
[1, 2, 3] ++ ["hello", :ok] ++ [3.14, true]
end'

	expected := '-module(test).
-export([concat_mixed/0]).

-spec concat_mixed() -> [integer() | binary() | atom() | float() | boolean()].
concat_mixed() ->
    [1, 2, 3] ++ [<<"hello"/utf8>>, ok] ++ [3.14, true].
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_variable_binding_in_patterns() {
	// Test that variables are correctly bound from complex patterns
	lx_code := 'def sum_list do
([]) -> 0
([head | tail]) -> head + sum_list(tail)
end'

	expected := '-module(test).
-export([sum_list/1]).

-spec sum_list(any()) -> integer().
sum_list([]) ->
    0;
sum_list([HEAD_1 | TAIL_2]) ->
    HEAD_1 + sum_list(TAIL_2).
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_union_type_deduplication() {
	// Test that union types correctly deduplicate repeated types
	lx_code := 'def mixed_types() do
[1, "hello", 1, "world", :atom]
end'

	expected := '-module(test).
-export([mixed_types/0]).

-spec mixed_types() -> [integer() | binary() | atom()].
mixed_types() ->
    [1, <<"hello"/utf8>>, 1, <<"world"/utf8>>, atom].
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_list_unification_with_any() {
	// Test that list(any) unifies correctly with list(specific)
	lx_code := 'def test_any_list() do
empty_list = []
typed_list = [1, 2, 3]
empty_list ++ typed_list
end'

	expected := '-module(test).
-export([test_any_list/0]).

-spec test_any_list() -> [any()].
test_any_list() ->
    EMPTY_LIST_1 = [],
    TYPED_LIST_2 = [1, 2, 3],
    EMPTY_LIST_1 ++ TYPED_LIST_2.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_pattern_without_type_annotation() {
	// Test that patterns without type annotations work correctly
	lx_code := 'def check_pattern do
([head]) -> head
([a, b]) -> a + b
end'

	expected := '-module(test).
-export([check_pattern/1]).

-spec check_pattern(any()) -> integer().
check_pattern([HEAD_1]) ->
    HEAD_1;
check_pattern([A_2, B_3]) ->
    A_2 + B_3.
'

	result := compile_lx(lx_code)
	assert result == expected
}