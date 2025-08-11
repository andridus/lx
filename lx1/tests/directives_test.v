module main

fn test_directive_print_simple() {
	lx_code := 'def test_function() do
    x = 42
    \$print(x)
    x
end'

	result := compile_lx(lx_code)

	// Verifica se o código Erlang foi gerado corretamente (sem diretivas)
	assert result.contains('-module(test).')
	assert result.contains('-export([test_function/0]).')
	assert result.contains('test_function() ->')
	assert result.contains('X_1 = 42')
	assert result.contains('X_1')

	// Verifica se não há referências às diretivas no código final
	assert !result.contains('${print}')
	assert !result.contains('print')
}

fn test_directive_type_simple() {
	lx_code := 'def test_function() do
    x = 42
    \$type(x)
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

fn test_directive_print_expression() {
	lx_code := 'def test_function() do
    a = 10
    b = 5
    \$print(a + b)
    a + b
end'
	expected := '-module(test).
-export([test_function/0]).

-spec test_function() -> integer().
test_function() ->
    A_1 = 10,
    B_2 = 5,
    A_1 + B_2.
'
	result := compile_lx(lx_code)
	assert result == expected
}

fn test_directive_type_expression() {
	lx_code := 'def test_function() do
    a = 10
    b = 5
    \$type(a + b)
    a + b
end'
	expected := '-module(test).
-export([test_function/0]).

-spec test_function() -> integer().
test_function() ->
    A_1 = 10,
    B_2 = 5,
    A_1 + B_2.
'
	result := compile_lx(lx_code)
	assert result == expected
}

fn test_multiple_directives() {
	lx_code := 'def test_function() do
    x = 42
    y = "hello"
    \$print(x)
    \$type(x)
    \$print(y)
    \$type(y)
    x
end'
	expected := '-module(test).
-export([test_function/0]).

-spec test_function() -> integer().
test_function() ->
    X_1 = 42,
    Y_2 = <<"hello"/utf8>>,
    X_1.
'
	result := compile_lx(lx_code)
	assert result == expected
}

fn test_directive_complex_expression() {
	lx_code := 'def test_function() do
    a = 10
    b = 5
    c = 2
    \$print((a + b) * c)
    \$type((a + b) * c)
    (a + b) * c
end'
	expected := '-module(test).
-export([test_function/0]).

-spec test_function() -> integer().
test_function() ->
    A_1 = 10,
    B_2 = 5,
    C_3 = 2,
    (A_1 + B_2) * C_3.
'
	result := compile_lx(lx_code)
	assert result == expected
}

fn test_directive_error_unknown() {
	lx_code := 'def test_function() do
    x = 42
    \$unknown(x)
    x
end'

	result := compile_lx_with_error(lx_code)
	assert result.contains('Unknown directive: \$unknown')
}

fn test_directive_error_no_parentheses() {
	lx_code := 'def test_function() do
    x = 42
    \$print x
    x
end'

	result := compile_lx_with_error(lx_code)
	assert result.contains('Directive \$print requires parentheses')
}

fn test_directive_error_wrong_argument_count() {
	lx_code := 'def test_function() do
    x = 42
    y = 10
    \$print(x, y)
    x
end'
	result := compile_lx_with_error(lx_code)
	assert result.contains('Directive \$print requires 1 arguments, got 2')
}

fn test_directive_transparency() {
	lx_code := 'def test_function() do
    x = 42
    \$print(x)
    \$type(x)
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
