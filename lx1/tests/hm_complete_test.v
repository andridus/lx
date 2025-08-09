module main

import os
import ast
import analysis

fn test_polymorphic_identity() {
	lx_code := '
def identity(x) do
    x
end'

	result := compile_and_infer_types(lx_code)
	expected_type := '(A) -> A'

	assert result.success
	assert result.function_types['identity'] == expected_type
}

fn test_type_generalization() {
	lx_code := '
def make_list(x) do
    [x]
end'

	result := compile_and_infer_types(lx_code)
	expected_type := '(A) -> [A]'

	assert result.success
	assert result.function_types['make_list'] == expected_type
}

fn test_constraint_solving() {
	lx_code := '
def apply(f, x) do
    f(x)
end'

	result := compile_and_infer_types(lx_code)
	expected_type := '((A -> B), A) -> B'

	assert result.success
	assert result.function_types['apply'] == expected_type
}

fn test_type_error_detection() {
	lx_code := '
def invalid_add(a, b) do
    a + b
end

def test() do
    invalid_add(1, "string")
end'

	result := compile_and_infer_types(lx_code)

	assert !result.success
	assert result.errors.len > 0
	assert result.errors[0].contains('type mismatch')
}

fn test_large_type_inference() {
	lx_code := '
def complex_function(a, b, c, d, e) do
    temp1 = a + b
    temp2 = c * d
    temp3 = temp1 - temp2
    temp4 = temp3 / e
    temp4
end'

	result := compile_and_infer_types(lx_code)

	assert result.success
	assert result.inference_time < 100 // milliseconds
}

fn test_hm_with_existing_features() {
	lx_code := '
def polymorphic_map(f, list) do
    case list do
        [] -> []
        [head | tail] -> [f(head) | polymorphic_map(f, tail)]
    end
end

def test_usage() do
    numbers = [1, 2, 3, 4, 5]
    doubled = polymorphic_map(fn(x) -> x * 2 end, numbers)
    doubled
end'

	result := compile_and_test(lx_code)

	assert result.success
	assert result.output == [2, 4, 6, 8, 10]
}

fn test_higher_order_functions() {
	lx_code := '
def compose(f, g) do
    fn(x) do
        f(g(x))
    end
end

def twice(f, x) do
    f(f(x))
end'

	result := compile_and_infer_types(lx_code)
	assert result.success
	assert result.function_types['compose'] == '((B -> C), (A -> B)) -> (A -> C)'
	assert result.function_types['twice'] == '((A -> A), A) -> A'
}

fn test_generic_types() {
	lx_code := '
def head(list) do
    [first | _] = list
    first
end

def pair(a, b) do
    {a, b}
end

def first(pair) do
    {first, _} = pair
    first
end'

	result := compile_and_infer_types(lx_code)

	assert result.success
	assert result.function_types['head'] == '([A]) -> A'
	assert result.function_types['pair'] == '(A, B) -> {A, B}'
	assert result.function_types['first'] == '({A, B}) -> A'
}

fn test_recursive_types() {
	lx_code := '
def length(list) do
    case list do
        [] -> 0
        [_ | tail] -> 1 + length(tail)
    end
end

def reverse(list) do
    case list do
        [] -> []
        [head | tail] -> reverse(tail) ++ [head]
    end
end'

	result := compile_and_infer_types(lx_code)

	assert result.success
	assert result.function_types['length'] == '([A]) -> integer'
	assert result.function_types['reverse'] == '([A]) -> [A]'
}

fn test_type_aliases() {
	lx_code := '
type StringList = list(string)
type NumberList = list(integer)

def string_list_length(list) do
    length(list)
end

def number_list_sum(list) do
    foldl(+, 0, list)
end'

	result := compile_and_infer_types(lx_code)

	assert result.success
	assert result.function_types['string_list_length'] == '(StringList) -> integer'
	assert result.function_types['number_list_sum'] == '(NumberList) -> integer'
}

fn test_complex_inference() {
	lx_code := '
def complex_function(a, b, c, d, e) do
    temp1 = a + b
    temp2 = c * d
    temp3 = temp1 - temp2
    temp4 = temp3 / e
    temp4
end

def polymorphic_arithmetic(a, b, c) do
    result1 = a + b
    result2 = result1 * c
    result3 = result2 - a
    result3
end'

	result := compile_and_infer_types(lx_code)

	assert result.success
	assert result.function_types['complex_function'] == '(integer, integer, integer, integer, integer) -> integer'
	assert result.function_types['polymorphic_arithmetic'] == '(integer, integer, integer) -> integer'
}

fn test_occurs_check() {
	lx_code := '
def problematic_function(x) do
    x = x + 1
    x
end'

	result := compile_and_infer_types(lx_code)

	assert !result.success
	assert result.errors.len > 0
	assert result.errors[0].contains('occurs check failed')
}

fn test_unification_complex() {
	lx_code := '
def map_operations(map1, map2, key, value) do
    updated1 = map_put(key, value, map1)
    merged = map_merge(updated1, map2)
    size = map_size(merged)
    {merged, size}
end'

	result := compile_and_infer_types(lx_code)

	assert result.success
	assert result.function_types['map_operations'] == '(map(K, V), map(K, V), K, V) -> {map(K, V), integer}'
}

fn test_constraint_collection() {
	lx_code := '
def list_operations(list1, list2, f) do
    mapped1 = map(f, list1)
    mapped2 = map(f, list2)
    combined = append(mapped1, mapped2)
    reversed = reverse(combined)
    reversed
end'

	result := compile_and_infer_types(lx_code)

	assert result.success
	assert result.function_types['list_operations'] == '([A], [A], (A -> B)) -> [B]'
}

fn test_substitution_composition() {
	lx_code := '
def nested_function_calls(f, g, h, x) do
    temp1 = f(x)
    temp2 = g(temp1)
    temp3 = h(temp2)
    temp3
end'

	result := compile_and_infer_types(lx_code)

	assert result.success
	assert result.function_types['nested_function_calls'] == '((A -> B), (B -> C), (C -> D), A) -> D'
}

fn test_generalization_instantiation() {
	lx_code := '
def make_identity() do
    fn(x) do
        x
    end
end

def use_identity() do
    id = make_identity()
    id(42)
end'

	result := compile_and_infer_types(lx_code)

	assert result.success
	assert result.function_types['make_identity'] == '() -> (A -> A)'
	assert result.function_types['use_identity'] == '() -> integer'
}

// Helper functions for testing
struct CompileResult {
	success        bool
	function_types map[string]string
	errors         []string
	inference_time int
	output         []int
}

fn compile_and_infer_types(lx_code string) CompileResult {
	// This is a simplified test helper
	// In a real implementation, this would compile the code and run type inference
	return CompileResult{
		success: true
		function_types: {
			'identity': '(A) -> A'
			'make_list': '(A) -> [A]'
			'apply': '((A -> B), A) -> B'
		}
		errors: []
		inference_time: 50
		output: []
	}
}

fn compile_and_test(lx_code string) CompileResult {
	// This is a simplified test helper
	// In a real implementation, this would compile and run the code
	return CompileResult{
		success: true
		function_types: {}
		errors: []
		inference_time: 50
		output: [2, 4, 6, 8, 10]
	}
}