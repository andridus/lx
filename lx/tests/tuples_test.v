module main

fn test_empty_tuple() {
	lx_code := 'def empty() do
    {}
end'

	expected := '-module(test).
-export([empty/0]).

-spec empty() -> {any()}.
empty() ->
    {}.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_simple_tuple_literal() {
	lx_code := 'def numbers() do
    {1, 2, 3}
end'

	expected := '-module(test).
-export([numbers/0]).

-spec numbers() -> {integer(), integer(), integer()}.
numbers() ->
    {1, 2, 3}.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_mixed_type_tuple() {
	lx_code := 'def mixed() do
    {1, "hello", :atom, 3.14, true}
end'

	expected := '-module(test).
-export([mixed/0]).

-spec mixed() -> {integer(), binary(), atom(), float(), boolean()}.
mixed() ->
    {1, <<"hello"/utf8>>, atom, 3.14, true}.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_tuple_size() {
	lx_code := 'def size_example() do
    tuple = {1, 2, 3, 4, 5}
    tuple_size(tuple)
end'

	expected := '-module(test).
-export([size_example/0]).

-spec size_example() -> integer().
size_example() ->
    TUPLE_1 = {1, 2, 3, 4, 5},
    tuple_size(TUPLE_1).
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_element_access() {
	lx_code := 'def access_example() do
    tuple = {1, "hello", :atom, 3.14}
    first = element(1, tuple)
    second = element(2, tuple)
    third = element(3, tuple)
    fourth = element(4, tuple)
    {first, second, third, fourth}
end'

	expected := '-module(test).
-export([access_example/0]).

-spec access_example() -> {any(), any(), any(), any()}.
access_example() ->
    TUPLE_1 = {1, <<"hello"/utf8>>, atom, 3.14},
    FIRST_2 = element(1, TUPLE_1),
    SECOND_3 = element(2, TUPLE_1),
    THIRD_4 = element(3, TUPLE_1),
    FOURTH_5 = element(4, TUPLE_1),
    {FIRST_2, SECOND_3, THIRD_4, FOURTH_5}.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_set_element() {
	lx_code := 'def set_example() do
    tuple = {1, 2, 3, 4}
    modified = setelement(2, tuple, "changed")
    modified
end'

	expected := '-module(test).
-export([set_example/0]).

-spec set_example() -> tuple().
set_example() ->
    TUPLE_1 = {1, 2, 3, 4},
    MODIFIED_2 = setelement(2, TUPLE_1, <<"changed"/utf8>>),
    MODIFIED_2.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_nested_tuples() {
	lx_code := 'def nested() do
    matrix = {{1, 2, 3}, {4, 5, 6}, {7, 8, 9}}
    matrix
end'

	expected := '-module(test).
-export([nested/0]).

-spec nested() -> {{integer(), integer(), integer()}, {integer(), integer(), integer()}, {integer(), integer(), integer()}}.
nested() ->
    MATRIX_1 = {{1, 2, 3}, {4, 5, 6}, {7, 8, 9}},
    MATRIX_1.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_complex_tuple_operations() {
	lx_code := 'def complex() do
    point = {10, 20}
    rectangle = {{0, 0}, {100, 200}}
    user = {1, "João", 30, true}
    _x = element(1, point)
    _y = element(2, point)
    updated_user = setelement(3, user, 31)
    {point, rectangle, updated_user}
end'

	expected := '-module(test).
-export([complex/0]).

-spec complex() -> {{integer(), integer()}, {{integer(), integer()}, {integer(), integer()}}, tuple()}.
complex() ->
    POINT_1 = {10, 20},
    RECTANGLE_2 = {{0, 0}, {100, 200}},
    USER_3 = {1, <<"João"/utf8>>, 30, true},
    _X_4 = element(1, POINT_1),
    _Y_5 = element(2, POINT_1),
    UPDATED_USER_6 = setelement(3, USER_3, 31),
    {POINT_1, RECTANGLE_2, UPDATED_USER_6}.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_tuple_with_variables() {
	lx_code := 'def tuple_with_vars() do
    a = 1
    b = 2
    c = 3
    {a, b, c}
end'

	expected := '-module(test).
-export([tuple_with_vars/0]).

-spec tuple_with_vars() -> {integer(), integer(), integer()}.
tuple_with_vars() ->
    A_1 = 1,
    B_2 = 2,
    C_3 = 3,
    {A_1, B_2, C_3}.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_tuple_size_with_variables() {
	lx_code := 'def size_with_vars() do
    my_tuple = {1, 2, 3, 4, 5}
    tuple_size(my_tuple)
end'

	expected := '-module(test).
-export([size_with_vars/0]).

-spec size_with_vars() -> integer().
size_with_vars() ->
    MY_TUPLE_1 = {1, 2, 3, 4, 5},
    tuple_size(MY_TUPLE_1).
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_element_access_with_variables() {
	lx_code := 'def access_with_vars() do
    my_tuple = {10, 20, 30, 40}
    index = 2
    element(index, my_tuple)
end'

	expected := '-module(test).
-export([access_with_vars/0]).

-spec access_with_vars() -> any().
access_with_vars() ->
    MY_TUPLE_1 = {10, 20, 30, 40},
    INDEX_2 = 2,
    element(INDEX_2, MY_TUPLE_1).
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_set_element_with_variables() {
	lx_code := 'def set_with_vars() do
    my_tuple = {1, 2, 3, 4}
    index = 3
    new_value = "updated"
    setelement(index, my_tuple, new_value)
end'

	expected := '-module(test).
-export([set_with_vars/0]).

-spec set_with_vars() -> tuple().
set_with_vars() ->
    MY_TUPLE_1 = {1, 2, 3, 4},
    INDEX_2 = 3,
    NEW_VALUE_3 = <<"updated"/utf8>>,
    setelement(INDEX_2, MY_TUPLE_1, NEW_VALUE_3).
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_multiple_tuple_operations() {
	lx_code := 'def multiple_ops() do
    tuple1 = {1, 2, 3}
    tuple2 = {4, 5, 6}
    size1 = tuple_size(tuple1)
    size2 = tuple_size(tuple2)
    first1 = element(1, tuple1)
    first2 = element(1, tuple2)
    {size1, size2, first1, first2}
end'

	expected := '-module(test).
-export([multiple_ops/0]).

-spec multiple_ops() -> {integer(), integer(), any(), any()}.
multiple_ops() ->
    TUPLE1_1 = {1, 2, 3},
    TUPLE2_2 = {4, 5, 6},
    SIZE1_3 = tuple_size(TUPLE1_1),
    SIZE2_4 = tuple_size(TUPLE2_2),
    FIRST1_5 = element(1, TUPLE1_1),
    FIRST2_6 = element(1, TUPLE2_2),
    {SIZE1_3, SIZE2_4, FIRST1_5, FIRST2_6}.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_tuple_with_expressions() {
	lx_code := 'def tuple_with_expr() do
    a = 5
    b = 3
    {a + b, a * b, a - b}
end'

	expected := '-module(test).
-export([tuple_with_expr/0]).

-spec tuple_with_expr() -> {integer(), integer(), integer()}.
tuple_with_expr() ->
    A_1 = 5,
    B_2 = 3,
    {A_1 + B_2, A_1 * B_2, A_1 - B_2}.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_tuple_with_parentheses() {
	lx_code := 'def tuple_with_parens() do
    a = 2
    b = 3
    {(a + b) * 2, (a - b) * 2}
end'

	expected := '-module(test).
-export([tuple_with_parens/0]).

-spec tuple_with_parens() -> {integer(), integer()}.
tuple_with_parens() ->
    A_1 = 2,
    B_2 = 3,
    {(A_1 + B_2) * 2, (A_1 - B_2) * 2}.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_deeply_nested_tuples() {
	lx_code := 'def deep_nested() do
    {{{1, 2}, {3, 4}}, {{5, 6}, {7, 8}}}
end'

	expected := '-module(test).
-export([deep_nested/0]).

-spec deep_nested() -> {{{integer(), integer()}, {integer(), integer()}}, {{integer(), integer()}, {integer(), integer()}}}.
deep_nested() ->
    {{{1, 2}, {3, 4}}, {{5, 6}, {7, 8}}}.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_tuple_with_all_literals() {
	lx_code := 'def all_literals() do
    {42, 3.14, "hello", :ok, true, false, nil}
end'

	expected := '-module(test).
-export([all_literals/0]).

-spec all_literals() -> {integer(), float(), binary(), atom(), boolean(), boolean(), nil}.
all_literals() ->
    {42, 3.14, <<"hello"/utf8>>, ok, true, false, nil}.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_tuple_size_empty() {
	lx_code := 'def size_empty() do
    tuple_size({})
end'

	expected := '-module(test).
-export([size_empty/0]).

-spec size_empty() -> integer().
size_empty() ->
    tuple_size({}).
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_element_access_out_of_bounds() {
	lx_code := 'def access_out_of_bounds() do
    tuple = {1, 2, 3}
    element(5, tuple)
end'

	expected := '-module(test).
-export([access_out_of_bounds/0]).

-spec access_out_of_bounds() -> any().
access_out_of_bounds() ->
    TUPLE_1 = {1, 2, 3},
    element(5, TUPLE_1).
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_set_element_out_of_bounds() {
	lx_code := 'def set_out_of_bounds() do
    tuple = {1, 2, 3}
    setelement(5, tuple, "new")
end'

	expected := '-module(test).
-export([set_out_of_bounds/0]).

-spec set_out_of_bounds() -> tuple().
set_out_of_bounds() ->
    TUPLE_1 = {1, 2, 3},
    setelement(5, TUPLE_1, <<"new"/utf8>>).
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_tuple_with_comparisons() {
	lx_code := 'def comparison_tuple() do
    a = 5
    b = 3
    {a > b, a == b, a != b}
end'

	expected := '-module(test).
-export([comparison_tuple/0]).

-spec comparison_tuple() -> {boolean(), boolean(), boolean()}.
comparison_tuple() ->
    A_1 = 5,
    B_2 = 3,
    {A_1 > B_2, A_1 == B_2, A_1 /= B_2}.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_tuple_with_logical_operators() {
	lx_code := 'def logical_tuple() do
    a = true
    b = false
    {a and b, a or b, not a}
end'

	expected := '-module(test).
-export([logical_tuple/0]).

-spec logical_tuple() -> {boolean(), boolean(), boolean()}.
logical_tuple() ->
    A_1 = true,
    B_2 = false,
    {A_1 andalso B_2, A_1 orelse B_2, not A_1}.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_tuple_with_bitwise_operators() {
	lx_code := 'def bitwise_tuple() do
    a = 5
    b = 3
    {a &&& b, a ||| b, a ^^^ b}
end'

	expected := '-module(test).
-export([bitwise_tuple/0]).

-spec bitwise_tuple() -> {integer(), integer(), integer()}.
bitwise_tuple() ->
    A_1 = 5,
    B_2 = 3,
    {A_1 band B_2, A_1 bor B_2, A_1 bxor B_2}.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_tuple_with_shift_operators() {
	lx_code := 'def shift_tuple() do
    a = 8
    {a <<< 2, a >>> 1}
end'

	result := compile_lx(lx_code)
	assert result.contains('shift_tuple() ->')
	assert result.contains('{A_1 bsl 2, A_1 bsr 1}')
	assert result.contains('-spec shift_tuple() -> {integer(), integer()}')
}

fn test_tuple_with_floating_point() {
	lx_code := 'def float_tuple() do
    {3.14, 2.718, 1.0}
end'

	expected := '-module(test).
-export([float_tuple/0]).

-spec float_tuple() -> {float(), float(), float()}.
float_tuple() ->
    {3.14, 2.718, 1.0}.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_tuple_with_atoms() {
	lx_code := 'def atom_tuple() do
    {:ok, :error, :timeout, :undefined}
end'

	expected := '-module(test).
-export([atom_tuple/0]).

-spec atom_tuple() -> {atom(), atom(), atom(), atom()}.
atom_tuple() ->
    {ok, error, timeout, undefined}.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_tuple_with_booleans() {
	lx_code := 'def boolean_tuple() do
    {true, false, true, false}
end'

	expected := '-module(test).
-export([boolean_tuple/0]).

-spec boolean_tuple() -> {boolean(), boolean(), boolean(), boolean()}.
boolean_tuple() ->
    {true, false, true, false}.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_tuple_with_nil() {
	lx_code := 'def nil_tuple() do
    {nil, nil, nil}
end'

	expected := '-module(test).
-export([nil_tuple/0]).

-spec nil_tuple() -> {nil, nil, nil}.
nil_tuple() ->
    {nil, nil, nil}.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_tuple_with_mixed_types() {
	lx_code := 'def mixed_tuple() do
    {1, "hello", :ok, 3.14, true, false, nil}
end'

	expected := '-module(test).
-export([mixed_tuple/0]).

-spec mixed_tuple() -> {integer(), binary(), atom(), float(), boolean(), boolean(), nil}.
mixed_tuple() ->
    {1, <<"hello"/utf8>>, ok, 3.14, true, false, nil}.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_tuple_with_function_calls() {
	lx_code := 'def function_calls() do
    a = 10
    b = 5
    {+(a, b), *(a, b), -(a, b)}
end'

	expected := '-module(test).
-export([function_calls/0]).

-spec function_calls() -> {integer(), integer(), integer()}.
function_calls() ->
    A_1 = 10,
    B_2 = 5,
    {A_1 + B_2, A_1 * B_2, A_1 - B_2}.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_tuple_with_parentheses_expressions() {
	lx_code := 'def parens_expressions() do
    a = 2
    b = 3
    c = 4
    {(a + b) * c, (a - b) * c, (a * b) + c}
end'

	expected := '-module(test).
-export([parens_expressions/0]).

-spec parens_expressions() -> {integer(), integer(), integer()}.
parens_expressions() ->
    A_1 = 2,
    B_2 = 3,
    C_3 = 4,
    {(A_1 + B_2) * C_3, (A_1 - B_2) * C_3, (A_1 * B_2) + C_3}.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_tuple_with_variable_references() {
	lx_code := 'def var_refs() do
    x = 10
    y = 20
    z = 30
    {x, y, z, x, y, z}
end'

	expected := '-module(test).
-export([var_refs/0]).

-spec var_refs() -> {integer(), integer(), integer(), integer(), integer(), integer()}.
var_refs() ->
    X_1 = 10,
    Y_2 = 20,
    Z_3 = 30,
    {X_1, Y_2, Z_3, X_1, Y_2, Z_3}.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_tuple_size_with_variable_reference() {
	lx_code := 'def size_var_ref() do
    my_tuple = {1, 2, 3, 4, 5}
    tuple_size(my_tuple)
end'

	expected := '-module(test).
-export([size_var_ref/0]).

-spec size_var_ref() -> integer().
size_var_ref() ->
    MY_TUPLE_1 = {1, 2, 3, 4, 5},
    tuple_size(MY_TUPLE_1).
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_element_access_with_variable_references() {
	lx_code := 'def access_var_refs() do
    my_tuple = {1, 2, 3, 4, 5}
    my_index = 3
    element(my_index, my_tuple)
end'

	expected := '-module(test).
-export([access_var_refs/0]).

-spec access_var_refs() -> any().
access_var_refs() ->
    MY_TUPLE_1 = {1, 2, 3, 4, 5},
    MY_INDEX_2 = 3,
    element(MY_INDEX_2, MY_TUPLE_1).
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_set_element_with_variable_references() {
	lx_code := 'def set_var_refs() do
    my_tuple = {1, 2, 3, 4, 5}
    my_index = 2
    my_value = "updated"
    setelement(my_index, my_tuple, my_value)
end'

	expected := '-module(test).
-export([set_var_refs/0]).

-spec set_var_refs() -> tuple().
set_var_refs() ->
    MY_TUPLE_1 = {1, 2, 3, 4, 5},
    MY_INDEX_2 = 2,
    MY_VALUE_3 = <<"updated"/utf8>>,
    setelement(MY_INDEX_2, MY_TUPLE_1, MY_VALUE_3).
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_tuple_operations_chain() {
	lx_code := 'def operations_chain() do
    tuple = {1, 2, 3, 4, 5}
    size = tuple_size(tuple)
    first = element(1, tuple)
    last = element(5, tuple)
    modified = setelement(3, tuple, "middle")
    {size, first, last, modified}
end'

	expected := '-module(test).
-export([operations_chain/0]).

-spec operations_chain() -> {integer(), any(), any(), tuple()}.
operations_chain() ->
    TUPLE_1 = {1, 2, 3, 4, 5},
    SIZE_2 = tuple_size(TUPLE_1),
    FIRST_3 = element(1, TUPLE_1),
    LAST_4 = element(5, TUPLE_1),
    MODIFIED_5 = setelement(3, TUPLE_1, <<"middle"/utf8>>),
    {SIZE_2, FIRST_3, LAST_4, MODIFIED_5}.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_tuple_with_lists() {
	lx_code := 'def tuple_with_lists() do
    {[1, 2, 3], [4, 5, 6], [7, 8, 9]}
end'

	expected := '-module(test).
-export([tuple_with_lists/0]).

-spec tuple_with_lists() -> {[integer()], [integer()], [integer()]}.
tuple_with_lists() ->
    {[1, 2, 3], [4, 5, 6], [7, 8, 9]}.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_tuple_with_nested_lists() {
	lx_code := 'def tuple_with_nested_lists() do
    {[[1, 2], [3, 4]], [[5, 6], [7, 8]]}
end'

	expected := '-module(test).
-export([tuple_with_nested_lists/0]).

-spec tuple_with_nested_lists() -> {[[integer()]], [[integer()]]}.
tuple_with_nested_lists() ->
    {[[1, 2], [3, 4]], [[5, 6], [7, 8]]}.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_tuple_with_list_operations() {
	lx_code := 'def tuple_with_list_ops() do
    list1 = [1, 2, 3]
    list2 = [4, 5, 6]
    {length(list1), list1 ++ list2, 3 in list1}
end'

	expected := '-module(test).
-export([tuple_with_list_ops/0]).

-spec tuple_with_list_ops() -> {integer(), [integer()], boolean()}.
tuple_with_list_ops() ->
    LIST1_1 = [1, 2, 3],
    LIST2_2 = [4, 5, 6],
    {length(LIST1_1), LIST1_1 ++ LIST2_2, lists:member(3, LIST1_1)}.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_complex_nested_structures() {
	lx_code := 'def complex_nested() do
    point = {10, 20}
    rectangle = {{0, 0}, {100, 200}}
    matrix = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
    user = {1, "João", 30, true}
    {point, rectangle, matrix, user}
end'

	result := compile_lx(lx_code)
	assert result.contains('complex_nested() ->')
	assert result.contains('POINT_1 = {10, 20}')
	assert result.contains('RECTANGLE_2 = {{0, 0}, {100, 200}}')
	assert result.contains('MATRIX_3 = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]')
	assert result.contains('USER_4 = {1, <<"João"/utf8>>, 30, true}')
	assert result.contains('{POINT_1, RECTANGLE_2, MATRIX_3, USER_4}')
}

fn test_tuple_with_directives() {
	lx_code := 'def tuple_with_directives() do
    tuple = {1, 2, 3, 4, 5}
    \$print(tuple)
    \$type(tuple)
    tuple
end'

	expected := '-module(test).
-export([tuple_with_directives/0]).

-spec tuple_with_directives() -> {integer(), integer(), integer(), integer(), integer()}.
tuple_with_directives() ->
    TUPLE_1 = {1, 2, 3, 4, 5},
    TUPLE_1.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_tuple_with_comments() {
	lx_code := 'def tuple_with_comments() do
    # This is a comment
    point = {10, 20}  # Another comment
    # Yet another comment
    point
end'

	expected := '-module(test).
-export([tuple_with_comments/0]).

-spec tuple_with_comments() -> {integer(), integer()}.
tuple_with_comments() ->
    POINT_1 = {10, 20},
    POINT_1.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_tuple_with_multiple_functions() {
	lx_code := 'def function1() do
    {1, 2, 3}
end

def function2() do
    {4, 5, 6}
end

def function3() do
    {7, 8, 9}
end'

	result := compile_lx(lx_code)
	assert result.contains('function1() ->')
	assert result.contains('function2() ->')
	assert result.contains('function3() ->')
	assert result.contains('{1, 2, 3}')
	assert result.contains('{4, 5, 6}')
	assert result.contains('{7, 8, 9}')
	assert result.contains('-export([function1/0, function2/0, function3/0])')
}

fn test_tuple_with_separators() {
	lx_code := 'def tuple_with_separators() do
    a = 1; b = 2; c = 3
    {a, b, c}
end'

	expected := '-module(test).
-export([tuple_with_separators/0]).

-spec tuple_with_separators() -> {integer(), integer(), integer()}.
tuple_with_separators() ->
    A_1 = 1,
    B_2 = 2,
    C_3 = 3,
    {A_1, B_2, C_3}.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_tuple_with_newlines() {
	lx_code := 'def tuple_with_newlines() do
    a = 1
    b = 2
    c = 3
    {a, b, c}
end'

	expected := '-module(test).
-export([tuple_with_newlines/0]).

-spec tuple_with_newlines() -> {integer(), integer(), integer()}.
tuple_with_newlines() ->
    A_1 = 1,
    B_2 = 2,
    C_3 = 3,
    {A_1, B_2, C_3}.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_tuple_with_multiline() {
	lx_code := 'def multiline_tuple() do
    {1,
     2,
     3,
     4,
     5}
end'

	expected := '-module(test).
-export([multiline_tuple/0]).

-spec multiline_tuple() -> {integer(), integer(), integer(), integer(), integer()}.
multiline_tuple() ->
    {1, 2, 3, 4, 5}.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_tuple_with_multiline_nested() {
	lx_code := 'def multiline_nested() do
    {{1, 2, 3},
     {4, 5, 6},
     {7, 8, 9}}
end'

	expected := '-module(test).
-export([multiline_nested/0]).

-spec multiline_nested() -> {{integer(), integer(), integer()}, {integer(), integer(), integer()}, {integer(), integer(), integer()}}.
multiline_nested() ->
    {{1, 2, 3}, {4, 5, 6}, {7, 8, 9}}.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_tuple_with_multiline_complex() {
	lx_code := 'def multiline_complex() do
    point = {10,
             20}
    rectangle = {{0, 0},
                 {100, 200}}
    user = {1,
            "João",
            30,
            true}
    {point, rectangle, user}
end'

	expected := '-module(test).
-export([multiline_complex/0]).

-spec multiline_complex() -> {{integer(), integer()}, {{integer(), integer()}, {integer(), integer()}}, {integer(), binary(), integer(), boolean()}}.
multiline_complex() ->
    POINT_1 = {10, 20},
    RECTANGLE_2 = {{0, 0}, {100, 200}},
    USER_3 = {1, <<"João"/utf8>>, 30, true},
    {POINT_1, RECTANGLE_2, USER_3}.
'

	result := compile_lx(lx_code)
	assert result == expected
}
