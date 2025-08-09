module main

// ============ Lambda Functions Tests ============

fn test_lambda_single_line() {
	lx_code := 'def test_lambda() do
    lambda = fn(x :: integer, y :: integer) -> x + y
    lambda.(3, 4)
end'
	expected := '-module(test).
-export([test_lambda/0]).

-spec test_lambda() -> any().
test_lambda() ->
    LAMBDA_1 = fun(X_2, Y_3) ->
        X_2 + Y_3
    end,
    LAMBDA_1(3, 4).
'
	result := compile_lx(lx_code)
	assert result == expected
}

fn test_lambda_multiline_with_end() {
	lx_code := 'def test_lambda_multiline() do
    lambda = fn(x :: integer, y :: integer) ->
        result = x + y
        result * 2
    end
    lambda.(5, 6)
end'
	expected := '-module(test).
-export([test_lambda_multiline/0]).

-spec test_lambda_multiline() -> any().
test_lambda_multiline() ->
    LAMBDA_1 = fun(X_2, Y_3) ->
        RESULT_4 = X_2 + Y_3,
    RESULT_4 * 2
    end,
    LAMBDA_1(5, 6).
'
	result := compile_lx(lx_code)
	assert result == expected
}

fn test_lambda_do_block() {
	lx_code := 'def test_lambda_do() do
    lambda = fn(x :: integer) do
        doubled = x * 2
        doubled + 1
    end
    lambda.(10)
end'
	expected := '-module(test).
-export([test_lambda_do/0]).

-spec test_lambda_do() -> any().
test_lambda_do() ->
    LAMBDA_1 = fun(X_2) ->
        DOUBLED_3 = X_2 * 2,
    DOUBLED_3 + 1
    end,
    LAMBDA_1(10).
'
	result := compile_lx(lx_code)
	assert result == expected
}

fn test_lambda_multihead() {
	lx_code := 'def test_lambda_multihead() do
    lambda = fn do
        (:ok) -> "success"
        (:error) -> "failure"
        (_) -> "unknown"
    end
    lambda.(:ok)
end'
	expected := '-module(test).
-export([test_lambda_multihead/0]).

-spec test_lambda_multihead() -> any().
test_lambda_multihead() ->
    LAMBDA_1 = fun
        (ok) ->
            <<"success"/utf8>>;
        (error) ->
            <<"failure"/utf8>>;
        (__2) ->
            <<"unknown"/utf8>>
    end,
    LAMBDA_1(ok).
'
	result := compile_lx(lx_code)
	assert result == expected
}

fn test_lambda_without_type_annotations() {
	lx_code := 'def test_simple_lambda() do
    lambda = fn(x, y) -> x + y
    lambda.(1, 2)
end'
	expected := '-module(test).
-export([test_simple_lambda/0]).

-spec test_simple_lambda() -> any().
test_simple_lambda() ->
    LAMBDA_1 = fun(X_2, Y_3) ->
        X_2 + Y_3
    end,
    LAMBDA_1(1, 2).
'
	result := compile_lx(lx_code)
	assert result == expected
}

fn test_lambda_no_parameters() {
	lx_code := 'def test_no_params() do
    lambda = fn() -> 42
    lambda.()
end'
	expected := '-module(test).
-export([test_no_params/0]).

-spec test_no_params() -> any().
test_no_params() ->
    LAMBDA_1 = fun() ->
        42
    end,
    LAMBDA_1().
'
	result := compile_lx(lx_code)
	assert result == expected
}

fn test_lambda_single_parameter() {
	lx_code := 'def test_single_param() do
    lambda = fn(x :: string) -> x
    lambda.("hello")
end'
	expected := '-module(test).
-export([test_single_param/0]).

-spec test_single_param() -> any().
test_single_param() ->
    LAMBDA_1 = fun(X_2) ->
        X_2
    end,
    LAMBDA_1(<<"hello"/utf8>>).
'
	result := compile_lx(lx_code)
	assert result == expected
}

fn test_lambda_complex_multihead() {
	lx_code := 'def test_complex_multihead() do
    lambda = fn do
        (0) -> "zero"
        (1) -> "one"
        (n) -> "many"
    end
    lambda.(5)
end'
	expected := '-module(test).
-export([test_complex_multihead/0]).

-spec test_complex_multihead() -> any().
test_complex_multihead() ->
    LAMBDA_1 = fun
        (0) ->
            <<"zero"/utf8>>;
        (1) ->
            <<"one"/utf8>>;
        (N_2) ->
            <<"many"/utf8>>
    end,
    LAMBDA_1(5).
'
	result := compile_lx(lx_code)
	assert result == expected
}

fn test_lambda_nested_calls() {
	lx_code := 'def test_nested() do
    add = fn(x :: integer, y :: integer) -> x + y
    multiply = fn(x :: integer, y :: integer) -> x * y
    result = multiply.(add.(2, 3), 4)
    result
end'
	expected := '-module(test).
-export([test_nested/0]).

-spec test_nested() -> any().
test_nested() ->
    ADD_1 = fun(X_2, Y_3) ->
        X_2 + Y_3
    end,
    MULTIPLY_4 = fun(X_5, Y_6) ->
        X_5 * Y_6
    end,
    RESULT_7 = MULTIPLY_4(ADD_1(2, 3), 4),
    RESULT_7.
'
	result := compile_lx(lx_code)
	assert result == expected
}

fn test_lambda_mixed_types() {
	lx_code := 'def test_mixed_types() do
    processor = fn do
        (x :: integer) -> x * 2
        (s :: string) -> s
        (_) -> :unknown
    end
    processor.(42)
end'
	expected := '-module(test).
-export([test_mixed_types/0]).

-spec test_mixed_types() -> any().
test_mixed_types() ->
    PROCESSOR_1 = fun
        (X_2) ->
            X_2 * 2;
        (S_3) ->
            S_3;
        (__4) ->
            unknown
    end,
    PROCESSOR_1(42).
'
	result := compile_lx(lx_code)
	assert result == expected
}

// ============ Lambda Error Cases Tests ============

fn test_lambda_error_missing_end() {
	lx_code := 'def test_error() do
    lambda = fn(x :: integer) ->
        x + 1
    lambda.(5)
end'
	result := compile_lx_with_error(lx_code)
	assert result.contains('Expected "end" to close lambda body after line break')
}

fn test_lambda_error_invalid_syntax() {
	lx_code := 'def test_error() do
    lambda = fn(x :: integer)
    lambda.(5)
end'
	result := compile_lx_with_error(lx_code)
	assert result.contains('Expected "->" or "do" after lambda parameters')
}

fn test_lambda_error_missing_params() {
	lx_code := 'def test_error() do
    lambda = fn -> x + 1
    lambda.()
end'
	result := compile_lx_with_error(lx_code)
	assert result.contains('Expected "(" after fn or "do" for multi-head lambda')
}

// ============ Lambda Integration Tests ============

fn test_lambda_with_records() {
	lx_code := 'record Person {
    name :: string
    age :: integer
}

def test_lambda_records() do
    creator = fn(name :: string, age :: integer) -> Person{name: name, age: age}
    person = creator.("John", 30)
    person.name
end'
	expected := '-module(test).
-export([test_lambda_records/0]).

-record(person, {name, age}).

-spec test_lambda_records() -> any().
test_lambda_records() ->
    CREATOR_1 = fun(NAME_2, AGE_3) ->
        #person{name = NAME_2, age = AGE_3}
    end,
    PERSON_4 = CREATOR_1(<<"John"/utf8>>, 30),
    PERSON_4#person.name.
'
	result := compile_lx(lx_code)
	assert result == expected
}

fn test_lambda_with_lists() {
	lx_code := 'def test_lambda_lists() do
    mapper = fn(f, list) do
        case list do
            [] -> []
            [h | t] -> [f.(h) | mapper.(f, t)]
        end
    end
    double = fn(x) -> x * 2
    mapper.(double, [1, 2, 3])
end'
	expected := '-module(test).
-export([test_lambda_lists/0]).

-spec test_lambda_lists() -> any().
test_lambda_lists() ->
    MAPPER_1 = fun(F_2, LIST_3) ->
        case LIST_3 of
            [] -> [];
            [H_4 | T_5] -> [F_2(H_4) | MAPPER_1(F_2, T_5)]
        end
    end,
    DOUBLE_6 = fun(X_7) ->
        X_7 * 2
    end,
    MAPPER_1(DOUBLE_6, [1, 2, 3]).
'
	result := compile_lx(lx_code)
	assert result == expected
}