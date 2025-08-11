module main

// Test type annotation context validation

fn test_invalid_type_annotation_in_list_literal() {
	// Type annotations should not be allowed in list literals
	lx_code := 'def invalid_list() do
[x :: integer, y :: string]
end'

	result := compile_lx(lx_code)
	// Should fail compilation
	assert result == 'falha'
}

fn test_invalid_type_annotation_in_expression() {
	// Type annotations should not be allowed in general expressions
	lx_code := 'def invalid_expr() do
x :: integer = 42
x
end'

	result := compile_lx(lx_code)
	// Should fail compilation
	assert result == 'falha'
}

fn test_invalid_type_annotation_in_function_call() {
	// Type annotations should not be allowed in function call arguments
	lx_code := 'def invalid_call() do
func(x :: integer)
end'

	result := compile_lx(lx_code)
	// Should fail compilation
	assert result == 'falha'
}

fn test_valid_type_annotation_in_function_parameter() {
	// Type annotations should be allowed in function parameters
	lx_code := 'def valid_param(x :: integer) do
x * 2
end'

	expected := '-module(test).
-export([valid_param/1]).

-spec valid_param(integer()) -> integer().
valid_param(X_1) ->
    X_1 * 2.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_valid_type_annotation_in_pattern_matching() {
	// Type annotations should be allowed in pattern matching contexts
	lx_code := 'def valid_pattern do
([head :: integer | tail]) -> head * 2
([]) -> 0
end'

	expected := '-module(test).
-export([valid_pattern/1]).

-spec valid_pattern([integer()]) -> integer().
valid_pattern([HEAD_1 | TAIL_2]) ->
    HEAD_1 * 2;
valid_pattern([]) ->
    0.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_valid_type_annotation_in_function_head() {
	// Type annotations should be allowed in function heads
	lx_code := 'def valid_head do
(x :: integer) -> x + 1
(y :: string) -> y
end'

	expected := '-module(test).
-export([valid_head/1]).

-spec valid_head(integer() | binary()) -> integer() | binary().
valid_head(X_1) ->
    X_1 + 1;
valid_head(Y_2) ->
    Y_2.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_invalid_type_annotation_in_map_literal() {
	// Type annotations should not be allowed in map literals
	lx_code := 'def invalid_map() do
%{key :: atom => value :: string}
end'

	result := compile_lx(lx_code)
	// Should fail compilation
	assert result == 'falha'
}

fn test_invalid_type_annotation_in_tuple_literal() {
	// Type annotations should not be allowed in tuple literals
	lx_code := 'def invalid_tuple() do
{x :: integer, y :: string}
end'

	result := compile_lx(lx_code)
	// Should fail compilation
	assert result == 'falha'
}
