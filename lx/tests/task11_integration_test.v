module main

// ============ Task 11 Integration Tests ============

// Test that binaries compile correctly
fn test_binaries_basic() {
	lx_code := 'def test_binary() do
    binary = <<1, 2, 3>>
    binary
end'
	expected := '-module(test).
-export([test_binary/0]).

-spec test_binary() -> binary().
test_binary() ->
    BINARY_1 = <<1, 2, 3>>,
    BINARY_1.
'
	result := compile_lx(lx_code)
	assert result == expected
}

fn test_binaries_with_options() {
	lx_code := 'def test_binary() do
    version = 1
    binary = <<version:8, 255:16/big>>
    binary
end'
	expected := '-module(test).
-export([test_binary/0]).

-spec test_binary() -> binary().
test_binary() ->
    VERSION_1 = 1,
    BINARY_2 = <<VERSION_1:8, 255:16/big>>,
    BINARY_2.
'
	result := compile_lx(lx_code)
	assert result == expected
}

// Test that custom types compile correctly
fn test_custom_types_basic() {
	lx_code := 'type my_type :: integer

def test_function(x :: my_type) do
    x
end'
	expected := '-module(test).
-export([test_function/1]).

-type my_type() :: integer().
-spec test_function(my_type()) -> my_type().
test_function(X_1) ->
    X_1.
'
	result := compile_lx(lx_code)
	assert result == expected
}

fn test_opaque_types() {
	lx_code := 'type opaque user_id :: integer

def create_user_id(id :: user_id) do
    id
end'
	expected := '-module(test).
-export([create_user_id/1]).

-opaque user_id() :: integer().
-spec create_user_id(user_id()) -> user_id().
create_user_id(ID_1) ->
    ID_1.
'
	result := compile_lx(lx_code)
	assert result == expected
}

// Test that control flow compiles correctly
fn test_if_expressions() {
	lx_code := 'def test_if(x) do
    if x > 0 do
        "positive"
    else
        "negative"
    end
end'
	expected := '-module(test).
-export([test_if/1]).

-spec test_if(any()) -> binary().
test_if(X_1) ->
    case X_1 > 0 of
        true -> <<"positive"/utf8>>;
        false -> <<"negative"/utf8>>
    end.
'
	result := compile_lx(lx_code)
	assert result == expected
}

fn test_case_expressions() {
	lx_code := 'def test_case(x) do
    case x do
        1 -> "one"
        2 -> "two"
        _ -> "other"
    end
end'
	expected := '-module(test).
-export([test_case/1]).

-spec test_case(any()) -> binary().
test_case(X_1) ->
    case X_1 of
        1 ->
            <<"one"/utf8>>;
        2 ->
            <<"two"/utf8>>;
        _ ->
            <<"other"/utf8>>
    end.
'
	result := compile_lx(lx_code)
	assert result == expected
}

fn test_case_with_tuples() {
	lx_code := 'def test_case(result) do
    case result do
        {:success, data} -> data
        {:error, reason} -> reason
        _ -> "unknown"
    end
end'
	expected := '-module(test).
-export([test_case/1]).

-spec test_case(any()) -> any().
test_case(RESULT_1) ->
    case RESULT_1 of
        {success, DATA_2} ->
            DATA_2;
        {error, REASON_3} ->
            REASON_3;
        _ ->
            <<"unknown"/utf8>>
    end.
'
	result := compile_lx(lx_code)
	assert result == expected
}

fn test_with_expressions() {
	lx_code := 'def test_with() do
    result = {:ok, "data"}
    with {:ok, data} <- result do
        data
    end
end'
	expected := '-module(test).
-export([test_with/0]).

-spec test_with() -> any().
test_with() ->
    RESULT_1 = {ok, <<"data"/utf8>>},
    case RESULT_1 of
        {ok, DATA_2} ->
            DATA_2;
        Error -> Error
    end.
'
	result := compile_lx(lx_code)
	assert result == expected
}

// Test lambda functions (already well tested)
fn test_lambda_basic() {
	lx_code := 'def test_lambda() do
    lambda = fn(x, y) -> x + y
    lambda.(1, 2)
end'
	expected := '-module(test).
-export([test_lambda/0]).

-spec test_lambda() -> integer().
test_lambda() ->
    LAMBDA_1 = fun(X_2, Y_3) ->
        X_2 + Y_3
    end,
    LAMBDA_1(1, 2).
'
	result := compile_lx(lx_code)
	assert result == expected
}

// Test error cases
fn test_syntax_errors() {
	lx_code := 'def test_binary() do
    binary = <<1, 2, 3
    binary
end'
	expected := '
	-module(test).
	-export([test_binary/0]).

	test_binary() ->
	    Binary = <<1, 2, 3>>,
	    Binary.
	'
	result := compile_lx_with_error(lx_code)
	assert result.contains('Expected ">>"')
}

// Test complex integration
fn test_complex_integration() {
	lx_code := 'type status :: {:string, binary}

def process_data(input) :: status do
    binary_data = <<1, 2, 3>>

    result = case input do
        {:user, name, age} ->
            if age > 18 do
                {:ok, name, binary_data}
            else
                {:error, "minor", binary_data}
            end
        _ -> {:error, "invalid", binary_data}
    end

    with {:ok, n, data} <- result do
        {n, data}
    end
end'
	expected := '-module(test).
-export([process_data/1]).

-type status() :: {atom(), binary()}.
-spec process_data(any()) -> status().
process_data(INPUT_1) ->
    BINARY_DATA_2 = <<1, 2, 3>>,
    RESULT_3 = case INPUT_1 of
        {user, NAME_4, AGE_5} ->
            case AGE_5 > 18 of
        true -> {ok, NAME_4, BINARY_DATA_2};
        false -> {error, <<"minor"/utf8>>, BINARY_DATA_2}
    end;
        _ ->
            {error, <<"invalid"/utf8>>, BINARY_DATA_2}
    end,
    case RESULT_3 of
        {ok, N_6, DATA_7} ->
            {N_6, DATA_7};
        Error -> Error
    end.
'
	result := compile_lx(lx_code)
	assert result == expected
}
