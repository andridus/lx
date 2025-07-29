module main

fn test_simple_arithmetic() {
	lx_code := 'def test() do
        10 + 5
    end'
	expected := '-module(test).
-export([test/0]).

-spec test() -> integer().
test() ->
    10 + 5.
'
	result := compile_lx(lx_code)
	assert result == expected
}

fn test_arithmetic_precedence() {
	lx_code := 'def test() do
        2 + 3 * 4
    end'
	expected := '-module(test).
-export([test/0]).

-spec test() -> integer().
test() ->
    2 + 3 * 4.
'
	result := compile_lx(lx_code)
	assert result == expected
}

fn test_parentheses() {
	lx_code := 'def test() do
        (2 + 3) * 4
    end'
	expected := '-module(test).
-export([test/0]).

-spec test() -> integer().
test() ->
    (2 + 3) * 4.
'
	result := compile_lx(lx_code)
	assert result == expected
}

fn test_comparison_operators() {
	lx_code := 'def test() do
        10 > 5
    end'
	expected := '-module(test).
-export([test/0]).

-spec test() -> boolean().
test() ->
    10 > 5.
'
	result := compile_lx(lx_code)
	assert result == expected
}

fn test_bitwise_operators() {
	lx_code := 'def test() do
        a = 5
        b = 3
        a &&& b
    end'
	expected := '-module(test).
-export([test/0]).

-spec test() -> integer().
test() ->
    A_1 = 5,
    B_2 = 3,
    A_1 band B_2.
'
	result := compile_lx(lx_code)
	assert result == expected
}

fn test_complex_expression() {
	lx_code := 'def test() do
        a = 10
        b = 5
        c = 3
        (a + b) * c - 2
    end'
	expected := '-module(test).
-export([test/0]).

-spec test() -> integer().
test() ->
    A_1 = 10,
    B_2 = 5,
    C_3 = 3,
    (A_1 + B_2) * C_3 - 2.
'
	result := compile_lx(lx_code)
	assert result == expected
}

fn test_type_mismatch_error() {
	lx_code := 'def test() do
        42 + "hello"
    end'
	result := compile_lx_with_error(lx_code)
	assert result.contains('Invalid operator: +(integer, string)')
}

fn test_incompatible_numeric_types() {
	lx_code := 'def test() do
        a = 10
        b = 3.14
        a + b
    end'
	result := compile_lx_with_error(lx_code)
	assert result.contains('Invalid operator: +(integer, float)')
}

fn test_bitwise_with_non_integer() {
	lx_code := 'def test() do
        a = 5
        b = 3.14
        a &&& b
    end'
	result := compile_lx_with_error(lx_code)
	assert result.contains('Invalid operator: &&&(integer, float)')
}

fn test_multiple_operators() {
	lx_code := 'def test() do
        a = true
        b = false
        a and b
    end'
	expected := '-module(test).
-export([test/0]).

-spec test() -> boolean().
test() ->
    A_1 = true,
    B_2 = false,
    A_1 andalso B_2.
'
	result := compile_lx(lx_code)
	assert result == expected
}

fn test_float_arithmetic() {
	lx_code := 'def test() do
        a = 3.14
        b = 2.86
        a + b
    end'
	expected := '-module(test).
-export([test/0]).

-spec test() -> float().
test() ->
    A_1 = 3.14,
    B_2 = 2.86,
    A_1 + B_2.
'
	result := compile_lx(lx_code)
	assert result == expected
}

fn test_float_comparison() {
	lx_code := 'def test() do
        a = 3.14
        b = 2.86
        a > b
    end'
	expected := '-module(test).
-export([test/0]).

-spec test() -> boolean().
test() ->
    A_1 = 3.14,
    B_2 = 2.86,
    A_1 > B_2.
'
	result := compile_lx(lx_code)
	assert result == expected
}

fn test_all_bitwise_operators() {
	lx_code := 'def test() do
        a = 5
        b = 3
        a &&& b
        a ||| b
        a ^^^ b
        a <<< 2
        a >>> 1
    end'
	expected := '-module(test).
-export([test/0]).

-spec test() -> integer().
test() ->
    A_1 = 5,
    B_2 = 3,
    A_1 band B_2,
    A_1 bor B_2,
    A_1 bxor B_2,
    A_1 bsl 2,
    A_1 bsr 1.
'
	result := compile_lx(lx_code)
	assert result == expected
}

fn test_all_comparison_operators() {
	lx_code := 'def test() do
        a = 10
        b = 5
        a == b
        a != b
        a < b
        a <= b
        a > b
        a >= b
    end'
	expected := '-module(test).
-export([test/0]).

-spec test() -> boolean().
test() ->
    A_1 = 10,
    B_2 = 5,
    A_1 == B_2,
    A_1 != B_2,
    A_1 < B_2,
    A_1 <= B_2,
    A_1 > B_2,
    A_1 >= B_2.
'
	result := compile_lx(lx_code)
	assert result == expected
}
