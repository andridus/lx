module main

fn test_simple_binding() {
	lx_code := 'def test_function() do
        x = 42
        x
    end'
	expected := '-module(test).
-export([test_function/0]).

-spec test_function() -> integer().
test_function() ->
    X_1 = 42,
    X_1.
'
	result := compile_lx(lx_code)
	assert result == expected
}

fn test_multiple_bindings() {
	lx_code := 'def test_function() do
        a = 10
        b = 20
        a
    end'
	result := compile_lx(lx_code)
	expected := '-module(test).
-export([test_function/0]).

-spec test_function() -> integer().
test_function() ->
    A_1 = 10,
    B_2 = 20,
    A_1.
'
	assert result == expected
}

fn test_explicit_separators() {
	lx_code := 'def test_function() do
        a = 1; b = 2; c = 3
        a
    end'
	result := compile_lx(lx_code)
	expected := '-module(test).
-export([test_function/0]).

-spec test_function() -> integer().
test_function() ->
    A_1 = 1,
    B_2 = 2,
    C_3 = 3,
    A_1.
'
	assert result == expected
}

fn test_different_types() {
	lx_code := 'def test_function() do
        number = 42
        text = "Hello"
        flag = true
        number
    end'
	result := compile_lx(lx_code)
	expected := '-module(test).
-export([test_function/0]).

-spec test_function() -> integer().
test_function() ->
    NUMBER_1 = 42,
    TEXT_2 = <<"Hello"/utf8>>,
    FLAG_3 = true,
    NUMBER_1.
'
	assert result == expected
}

fn test_isolated_scope() {
	lx_code := 'def func1() do
        x = 42
        x
    end

    def func2() do
        x = "hello"
        x
    end'
	result := compile_lx(lx_code)
	expected := '-module(test).
-export([func1/0, func2/0]).

-spec func1() -> integer().
func1() ->
    X_1 = 42,
    X_1.
-spec func2() -> binary().
func2() ->
    X_1 = <<"hello"/utf8>>,
    X_1.
'
	assert result == expected
}

fn test_undefined_variable() {
	lx_code := 'def test_function() do
        x
    end'
	result := compile_lx_with_error(lx_code)
	assert result.contains('Undefined variable: x')
}

fn test_variable_reuse_error() {
	lx_code := 'def test_function() do
        x = 5
        x = 10
        x
    end'
	result := compile_lx_with_error(lx_code)
	assert result.contains('Variable "x" cannot be reassigned')
}
