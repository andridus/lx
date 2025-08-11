module main

fn test_empty_list() {
	lx_code := 'def empty() do
    []
end'

	expected := '-module(test).
-export([empty/0]).

-spec empty() -> [any()].
empty() ->
    [].
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_simple_list_literal() {
	lx_code := 'def numbers() do
    [1, 2, 3]
end'

	expected := '-module(test).
-export([numbers/0]).

-spec numbers() -> [integer()].
numbers() ->
    [1, 2, 3].
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_mixed_type_list() {
	lx_code := 'def mixed() do
    [1, "hello", :atom, 3.14, true]
end'

	expected := '-module(test).
-export([mixed/0]).

-spec mixed() -> [integer() | binary() | atom() | float() | boolean()].
mixed() ->
    [1, <<"hello"/utf8>>, atom, 3.14, true].
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_list_cons() {
	lx_code := 'def cons_example() do
    head = 1
    tail = [2, 3, 4]
    [head | tail]
end'

	expected := '-module(test).
-export([cons_example/0]).

-spec cons_example() -> [integer()].
cons_example() ->
    HEAD_1 = 1,
    TAIL_2 = [2, 3, 4],
    [HEAD_1 | TAIL_2].
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_list_cons_direct() {
	lx_code := 'def cons_direct() do
    [1 | [2, 3]]
end'

	expected := '-module(test).
-export([cons_direct/0]).

-spec cons_direct() -> [integer()].
cons_direct() ->
    [1 | [2, 3]].
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_list_concatenation() {
	lx_code := 'def concat() do
    list1 = [1, 2, 3]
    list2 = [4, 5, 6]
    list1 ++ list2
end'

	expected := '-module(test).
-export([concat/0]).

-spec concat() -> [integer()].
concat() ->
    LIST1_1 = [1, 2, 3],
    LIST2_2 = [4, 5, 6],
    LIST1_1 ++ LIST2_2.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_list_length() {
	lx_code := 'def length_example() do
    list = [1, 2, 3, 4, 5]
    length(list)
end'

	expected := '-module(test).
-export([length_example/0]).

-spec length_example() -> integer().
length_example() ->
    LIST_1 = [1, 2, 3, 4, 5],
    length(LIST_1).
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_list_membership() {
	lx_code := 'def membership() do
    list = [1, 2, 3, 4, 5]
    3 in list
end'

	expected := '-module(test).
-export([membership/0]).

-spec membership() -> boolean().
membership() ->
    LIST_1 = [1, 2, 3, 4, 5],
    lists:member(3, LIST_1).
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_nested_lists() {
	lx_code := 'def nested() do
    matrix = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
    matrix
end'

	expected := '-module(test).
-export([nested/0]).

-spec nested() -> [[integer()]].
nested() ->
    MATRIX_1 = [[1, 2, 3], [4, 5, 6], [7, 8, 9]],
    MATRIX_1.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_complex_list_operations() {
	lx_code := 'def complex() do
    numbers = [1, 2, 3]
    extended = [0 | numbers]
    combined = [1, 2] ++ [3, 4] ++ [5, 6]
    matrix = [[1, 2], [3, 4], [5, 6]]
    extended
end'

	result := compile_lx(lx_code)
	assert result.contains('complex() ->')
	assert result.contains('NUMBERS_1 = [1, 2, 3]')
	assert result.contains('EXTENDED_2 = [0 | NUMBERS_1]')
	assert result.contains('COMBINED_3 = [1, 2] ++ [3, 4] ++ [5, 6]')
	assert result.contains('MATRIX_4 = [[1, 2], [3, 4], [5, 6]]')
	assert result.contains('EXTENDED_2')
	assert result.contains('-spec complex() -> [integer()]')
}

fn test_list_with_variables() {
	lx_code := 'def list_with_vars() do
    a = 1
    b = 2
    c = 3
    [a, b, c]
end'

	expected := '-module(test).
-export([list_with_vars/0]).

-spec list_with_vars() -> [integer()].
list_with_vars() ->
    A_1 = 1,
    B_2 = 2,
    C_3 = 3,
    [A_1, B_2, C_3].
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_list_cons_with_variables() {
	lx_code := 'def cons_with_vars() do
    head = 10
    tail = [20, 30]
    [head | tail]
end'

	expected := '-module(test).
-export([cons_with_vars/0]).

-spec cons_with_vars() -> [integer()].
cons_with_vars() ->
    HEAD_1 = 10,
    TAIL_2 = [20, 30],
    [HEAD_1 | TAIL_2].
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_multiple_list_operations() {
	lx_code := 'def multiple_ops() do
    list1 = [1, 2]
    list2 = [3, 4]
    list3 = [5, 6]
    combined = list1 ++ list2 ++ list3
    len = length(combined)
    has_five = 5 in combined
    len
end'

	expected := '-module(test).
-export([multiple_ops/0]).

-spec multiple_ops() -> integer().
multiple_ops() ->
    LIST1_1 = [1, 2],
    LIST2_2 = [3, 4],
    LIST3_3 = [5, 6],
    COMBINED_4 = LIST1_1 ++ LIST2_2 ++ LIST3_3,
    LEN_5 = length(COMBINED_4),
    HAS_FIVE_6 = lists:member(5, COMBINED_4),
    LEN_5.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_list_with_expressions() {
	lx_code := 'def list_with_expr() do
    a = 5
    b = 3
    [a + b, a * b, a - b]
end'

	expected := '-module(test).
-export([list_with_expr/0]).

-spec list_with_expr() -> [integer()].
list_with_expr() ->
    A_1 = 5,
    B_2 = 3,
    [A_1 + B_2, A_1 * B_2, A_1 - B_2].
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_list_cons_with_expressions() {
	lx_code := 'def cons_with_expr() do
    x = 10
    y = 5
    [x + y | [x * y, x - y]]
end'

	expected := '-module(test).
-export([cons_with_expr/0]).

-spec cons_with_expr() -> [integer()].
cons_with_expr() ->
    X_1 = 10,
    Y_2 = 5,
    [X_1 + Y_2 | [X_1 * Y_2, X_1 - Y_2]].
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_list_with_parentheses() {
	lx_code := 'def list_with_parens() do
    a = 2
    b = 3
    [(a + b) * 2, (a - b) * 2]
end'

	expected := '-module(test).
-export([list_with_parens/0]).

-spec list_with_parens() -> [integer()].
list_with_parens() ->
    A_1 = 2,
    B_3 = 3,
    [(A_1 + B_3) * 2, (A_1 - B_3) * 2].
'

	result := compile_lx(lx_code)
	assert result.contains('list_with_parens() ->')
	assert result.contains('[(A_1 + B_2) * 2, (A_1 - B_2) * 2]')
	assert result.contains('-spec list_with_parens() -> [integer()]')
}

fn test_deeply_nested_lists() {
	lx_code := 'def deep_nested() do
    [[[1, 2], [3, 4]], [[5, 6], [7, 8]]]
end'

	expected := '-module(test).
-export([deep_nested/0]).

-spec deep_nested() -> [[[integer()]]].
deep_nested() ->
    [[[1, 2], [3, 4]], [[5, 6], [7, 8]]].
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_list_with_all_literals() {
	lx_code := 'def all_literals() do
    [42, 3.14, "hello", :ok, true, false, nil]
end'

	expected := '-module(test).
-export([all_literals/0]).

-spec all_literals() -> [integer() | float() | binary() | atom() | boolean() | nil].
all_literals() ->
    [42, 3.14, <<"hello"/utf8>>, ok, true, false, nil].
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_list_cons_empty_tail() {
	lx_code := 'def cons_empty() do
    [1 | []]
end'

	expected := '-module(test).
-export([cons_empty/0]).

-spec cons_empty() -> [integer()].
cons_empty() ->
    [1 | []].
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_list_cons_nested() {
	lx_code := 'def cons_nested() do
    [1 | [2 | [3 | []]]]
end'

	expected := '-module(test).
-export([cons_nested/0]).

-spec cons_nested() -> [integer()].
cons_nested() ->
    [1 | [2 | [3 | []]]].
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_list_concatenation_multiple() {
	lx_code := 'def concat_multiple() do
    [1] ++ [2] ++ [3] ++ [4]
end'

	expected := '-module(test).
-export([concat_multiple/0]).

-spec concat_multiple() -> [integer()].
concat_multiple() ->
    [1] ++ [2] ++ [3] ++ [4].
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_list_operations_chain() {
	lx_code := 'def operations_chain() do
    list = [1, 2, 3, 4, 5]
    extended = [0 | list]
    doubled = extended ++ extended
    length(doubled)
end'

	expected := '-module(test).
-export([operations_chain/0]).

-spec operations_chain() -> integer().
operations_chain() ->
    LIST_1 = [1, 2, 3, 4, 5],
    EXTENDED_2 = [0 | LIST_1],
    DOUBLED_3 = EXTENDED_2 ++ EXTENDED_2,
    length(DOUBLED_3).
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_list_membership_negative() {
	lx_code := 'def membership_negative() do
    list = [1, 2, 3, 4, 5]
    10 in list
end'

	expected := '-module(test).
-export([membership_negative/0]).

-spec membership_negative() -> boolean().
membership_negative() ->
    LIST_1 = [1, 2, 3, 4, 5],
    lists:member(10, LIST_1).
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_list_with_comparisons() {
	lx_code := 'def comparison_list() do
    a = 5
    b = 3
    [a > b, a == b, a != b]
end'

	expected := '-module(test).
-export([comparison_list/0]).

-spec comparison_list() -> [boolean()].
comparison_list() ->
    A_1 = 5,
    B_2 = 3,
    [A_1 > B_2, A_1 == B_2, A_1 /= B_2].
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_list_with_logical_operators() {
	lx_code := 'def logical_list() do
    a = true
    b = false
    [a and b, a or b, not a]
end'

	expected := '-module(test).
-export([logical_list/0]).

-spec logical_list() -> [boolean()].
logical_list() ->
    A_1 = true,
    B_2 = false,
    [A_1 andalso B_2, A_1 orelse B_2, not A_1].
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_list_with_bitwise_operators() {
	lx_code := 'def bitwise_list() do
    a = 5
    b = 3
    [a &&& b, a ||| b, a ^^^ b]
end'

	expected := '-module(test).
-export([bitwise_list/0]).

-spec bitwise_list() -> [integer()].
bitwise_list() ->
    A_1 = 5,
    B_2 = 3,
    [A_1 band B_2, A_1 bor B_2, A_1 bxor B_2].
'
	result := compile_lx(lx_code)
	assert result == expected
}

fn test_list_with_shift_operators() {
	lx_code := 'def shift_list() do
    a = 8
    [a <<< 2, a >>> 1]
end'

	result := compile_lx(lx_code)
	assert result.contains('shift_list() ->')
	assert result.contains('[A_1 bsl 2, A_1 bsr 1]')
	assert result.contains('-spec shift_list() -> [integer()]')
}

fn test_list_with_floating_point() {
	lx_code := 'def float_list() do
    [3.14, 2.718, 1.0]
end'

	expected := '-module(test).
-export([float_list/0]).

-spec float_list() -> [float()].
float_list() ->
    [3.14, 2.718, 1.0].
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_list_with_atoms() {
	lx_code := 'def atom_list() do
    [:ok, :error, :timeout, :undefined]
end'

	expected := '-module(test).
-export([atom_list/0]).

-spec atom_list() -> [atom()].
atom_list() ->
    [ok, error, timeout, undefined].
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_list_with_booleans() {
	lx_code := 'def boolean_list() do
    [true, false, true, false]
end'

	expected := '-module(test).
-export([boolean_list/0]).

-spec boolean_list() -> [boolean()].
boolean_list() ->
    [true, false, true, false].
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_list_with_nil() {
	lx_code := 'def nil_list() do
    [nil, nil, nil]
end'

	expected := '-module(test).
-export([nil_list/0]).

-spec nil_list() -> [nil].
nil_list() ->
    [nil, nil, nil].
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_list_cons_with_mixed_types() {
	lx_code := 'def cons_mixed() do
    [1 | ["hello", :ok, 3.14]]
end'

	expected := '-module(test).
-export([cons_mixed/0]).

-spec cons_mixed() -> [integer() | binary() | atom() | float()].
cons_mixed() ->
    [1 | [<<"hello"/utf8>>, ok, 3.14]].
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_list_concatenation_with_mixed_types() {
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

fn test_list_length_empty() {
	lx_code := 'def length_empty() do
    length([])
end'

	expected := '-module(test).
-export([length_empty/0]).

-spec length_empty() -> integer().
length_empty() ->
    length([]).
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_list_membership_empty() {
	lx_code := 'def membership_empty() do
    1 in []
end'

	expected := '-module(test).
-export([membership_empty/0]).

-spec membership_empty() -> boolean().
membership_empty() ->
    lists:member(1, []).
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_list_with_function_calls() {
	lx_code := 'def function_calls() do
    a = 10
    b = 5
    [+(a, b), *(a, b), -(a, b)]
end'

	expected := '-module(test).
-export([function_calls/0]).

-spec function_calls() -> [integer()].
function_calls() ->
    A_1 = 10,
    B_2 = 5,
    [A_1 + B_2, A_1 * B_2, A_1 - B_2].
'
	result := compile_lx(lx_code)
	assert result == expected
}

fn test_list_with_parentheses_expressions() {
	lx_code := 'def parens_expressions() do
    a = 2
    b = 3
    c = 4
    [(a + b) * c, (a - b) * c, (a * b) + c]
end'

	expected := '-module(test).
-export([parens_expressions/0]).

-spec parens_expressions() -> [integer()].
parens_expressions() ->
    A_1 = 2,
    B_2 = 3,
    C_3 = 4,
    [(A_1 + B_2) * C_3, (A_1 - B_2) * C_3, (A_1 * B_2) + C_3].
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_list_cons_with_parentheses() {
	lx_code := 'def cons_parens() do
    a = 5
    b = 3
    [(a + b) | [(a - b), (a * b)]]
end'

	expected := '-module(test).
-export([cons_parens/0]).

-spec cons_parens() -> [integer()].
cons_parens() ->
    A_1 = 5,
    B_2 = 3,
    [(A_1 + B_2) | [(A_1 - B_2), (A_1 * B_2)]].
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_list_concatenation_with_parentheses() {
	lx_code := 'def concat_parens() do
    a = 2
    b = 3
    [(a + b)] ++ [(a - b)] ++ [(a * b)]
end'

	expected := '-module(test).
-export([concat_parens/0]).

-spec concat_parens() -> [integer()].
concat_parens() ->
    A_1 = 2,
    B_2 = 3,
    [(A_1 + B_2)] ++ [(A_1 - B_2)] ++ [(A_1 * B_2)].
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_list_with_variable_references() {
	lx_code := 'def var_refs() do
    x = 10
    y = 20
    z = 30
    [x, y, z, x, y, z]
end'

	expected := '-module(test).
-export([var_refs/0]).

-spec var_refs() -> [integer()].
var_refs() ->
    X_1 = 10,
    Y_2 = 20,
    Z_3 = 30,
    [X_1, Y_2, Z_3, X_1, Y_2, Z_3].
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_list_cons_with_variable_references() {
	lx_code := 'def cons_var_refs() do
    head = 100
    tail = [200, 300]
    [head | tail]
end'

	expected := '-module(test).
-export([cons_var_refs/0]).

-spec cons_var_refs() -> [integer()].
cons_var_refs() ->
    HEAD_1 = 100,
    TAIL_2 = [200, 300],
    [HEAD_1 | TAIL_2].
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_list_concatenation_with_variable_references() {
	lx_code := 'def concat_var_refs() do
    list1 = [1, 2]
    list2 = [3, 4]
    list1 ++ list2
end'

	expected := '-module(test).
-export([concat_var_refs/0]).

-spec concat_var_refs() -> [integer()].
concat_var_refs() ->
    LIST1_1 = [1, 2],
    LIST2_2 = [3, 4],
    LIST1_1 ++ LIST2_2.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_list_length_with_variable_reference() {
	lx_code := 'def length_var_ref() do
    my_list = [1, 2, 3, 4, 5]
    length(my_list)
end'

	expected := '-module(test).
-export([length_var_ref/0]).

-spec length_var_ref() -> integer().
length_var_ref() ->
    MY_LIST_1 = [1, 2, 3, 4, 5],
    length(MY_LIST_1).
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_list_membership_with_variable_references() {
	lx_code := 'def membership_var_refs() do
    my_list = [1, 2, 3, 4, 5]
    my_element = 3
    my_element in my_list
end'

	expected := '-module(test).
-export([membership_var_refs/0]).

-spec membership_var_refs() -> boolean().
membership_var_refs() ->
    MY_LIST_1 = [1, 2, 3, 4, 5],
    MY_ELEMENT_2 = 3,
    lists:member(MY_ELEMENT_2, MY_LIST_1).
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_list_with_multiline() {
	lx_code := 'def multiline_list() do
    [1,
     2,
     3,
     4,
     5]
end'

	expected := '-module(test).
-export([multiline_list/0]).

-spec multiline_list() -> [integer()].
multiline_list() ->
    [1, 2, 3, 4, 5].
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_list_with_multiline_nested() {
	lx_code := 'def multiline_nested() do
    [[1, 2, 3],
     [4, 5, 6],
     [7, 8, 9]]
end'

	expected := '-module(test).
-export([multiline_nested/0]).

-spec multiline_nested() -> [[integer()]].
multiline_nested() ->
    [[1, 2, 3], [4, 5, 6], [7, 8, 9]].
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_list_cons_with_multiline() {
	lx_code := 'def cons_multiline() do
    head = 1
    tail = [2,
            3,
            4]
    [head | tail]
end'

	expected := '-module(test).
-export([cons_multiline/0]).

-spec cons_multiline() -> [integer()].
cons_multiline() ->
    HEAD_1 = 1,
    TAIL_2 = [2, 3, 4],
    [HEAD_1 | TAIL_2].
'

	result := compile_lx(lx_code)
	assert result == expected
}
