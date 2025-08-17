module main

// ============ Control Flow Tests ============

// Test basic if expressions
fn test_if_basic() {
	lx_code := 'def test_if(x) do
    if x > 0 do
        "positive"
    else
        "not positive"
    end
end'
	expected := '-module(test).
-export([test_if/1]).

-spec test_if(any()) -> binary().
test_if(X_1) ->
    case X_1 > 0 of
        true -> <<"positive"/utf8>>;
        false -> <<"not positive"/utf8>>
    end.
'
	result := compile_lx(lx_code)
	assert result == expected
}

// Test if without else
fn test_if_without_else() {
	lx_code := 'def test_if(x) do
    if x > 0 do
        "positive"
    end
end'
	expected := '-module(test).
-export([test_if/1]).

-spec test_if(any()) -> binary().
test_if(X_1) ->
    case X_1 > 0 of
        true -> <<"positive"/utf8>>;
        false -> nil
    end.
'
	result := compile_lx(lx_code)
	assert result == expected
}

// Test nested if expressions
fn test_if_nested() {
	lx_code := 'def test_nested(x, y) do
    if x > 0 do
        if y > 0 do
            "both positive"
        else
            "x positive, y not"
        end
    else
        "x not positive"
    end
end'
	expected := '-module(test).
-export([test_nested/2]).

-spec test_nested(any(), any()) -> binary().
test_nested(X_1, Y_2) ->
    case X_1 > 0 of
        true -> case Y_2 > 0 of
        true -> <<"both positive"/utf8>>;
        false -> <<"x positive, y not"/utf8>>
    end;
        false -> <<"x not positive"/utf8>>
    end.
'
	result := compile_lx(lx_code)
	assert result == expected
}

// Test basic case expressions
fn test_case_basic() {
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

// Test case with tuple patterns
fn test_case_tuple_patterns() {
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

// Test case with guards (if supported)
fn test_case_with_guards() {
	lx_code := 'def test_case(x) do
    case x do
        n when n > 0 -> "positive"
        n when n < 0 -> "negative"
        _ -> "zero"
    end
end'
	expected := '-module(test).
-export([test_case/1]).

-spec test_case(any()) -> binary().
test_case(X_1) ->
    case X_1 of
        N_2 when N_2 > 0 ->
            <<"positive"/utf8>>;
        N_2 when N_2 < 0 ->
            <<"negative"/utf8>>;
        _ ->
            <<"zero"/utf8>>
    end.
'
	result := compile_lx(lx_code)
	assert result == expected
}

// Test case with list patterns
fn test_case_list_patterns() {
	lx_code := 'def test_case(list) do
    case list do
        [] -> "empty"
        [h] -> h
        [h | t] -> h
    end
end'
	expected := '-module(test).
-export([test_case/1]).

-spec test_case(any()) -> any().
test_case(LIST_1) ->
    case LIST_1 of
        [] ->
            <<"empty"/utf8>>;
        [H_2] ->
            H_2;
        [H_2 | T_3] ->
            H_2
    end.
'
	result := compile_lx(lx_code)
	assert result == expected
}

// Test nested case expressions
fn test_case_nested() {
	lx_code := 'def test_nested(x, y) do
    case x do
        1 -> case y do
            a -> "1a"
            b -> "1b"
            _ -> "1other"
        end
        2 -> "two"
        _ -> "other"
    end
end'
	expected := '-module(test).
-export([test_nested/2]).

-spec test_nested(any(), any()) -> binary().
test_nested(X_1, Y_2) ->
    case X_1 of
        1 ->
            case Y_2 of
        A_3 ->
            <<"1a"/utf8>>;
        B_4 ->
            <<"1b"/utf8>>;
        _ ->
            <<"1other"/utf8>>
    end;
        2 ->
            <<"two"/utf8>>;
        _ ->
            <<"other"/utf8>>
    end.
'
	result := compile_lx(lx_code)
	assert result == expected
}

// Test basic with expressions
fn test_with_basic() {
	lx_code := 'def test_with() do
    result = {:success, "data"}
    with {:success, data} <- result do
        data
    end
end'
	expected := '-module(test).
-export([test_with/0]).

-spec test_with() -> any().
test_with() ->
    RESULT_1 = {success, <<"data"/utf8>>},
    case RESULT_1 of
        {success, DATA_2} ->
            DATA_2;
        Error -> Error
    end.
'
	result := compile_lx(lx_code)
	assert result == expected
}

// Test with multiple clauses
fn test_with_multiple_clauses() {
	lx_code := 'def test_with() do
    result1 = {:success, 10}
    result2 = {:success, 20}
    with {:success, x} <- result1, {:success, y} <- result2 do
        x + y
    end
end'
	expected := '-module(test).
-export([test_with/0]).

-spec test_with() -> integer().
test_with() ->
    RESULT1_1 = {success, 10},
    RESULT2_2 = {success, 20},
    case RESULT1_1 of
        {success, X_3} ->
            case RESULT2_2 of
        {success, Y_4} ->
            X_3 + Y_4;
        Error -> Error
    end;
        Error -> Error
    end.
'
	result := compile_lx(lx_code)
	assert result == expected
}

// Test with expression with else clause
fn test_with_else() {
	lx_code := 'def test_with(maybe_data) do
    with {:success, data} <- maybe_data do
        data
    else
        _ -> "failed"
    end
end'
	expected := '-module(test).
-export([test_with/1]).

-spec test_with(any()) -> any().
test_with(MAYBE_DATA_1) ->
    case MAYBE_DATA_1 of
        {success, DATA_2} ->
            DATA_2;
        _ ->
            <<"failed"/utf8>>
    end.
'
	result := compile_lx(lx_code)
	assert result == expected
}

// Test complex control flow combinations
fn test_complex_control_flow() {
	lx_code := 'def process_data(input) do
    if input != nil do
        case input do
            {data, _} ->
                with {success, result} <- data do
                    result
                end
            _ -> "invalid"
        end
    else
        "nil input"
    end
end'
	expected := '-module(test).
-export([process_data/1]).

-spec process_data(any()) -> any().
process_data(INPUT_1) ->
    case INPUT_1 /= nil of
        true -> case INPUT_1 of
        {DATA_2, _} ->
            case DATA_2 of
        {SUCCESS_3, RESULT_4} ->
            RESULT_4;
        Error -> Error
    end;
        _ ->
            <<"invalid"/utf8>>
    end;
        false -> <<"nil input"/utf8>>
    end.
'
	result := compile_lx(lx_code)
	assert result == expected
}

// Test control flow with records
fn test_control_flow_records() {
	lx_code := 'record user { name :: string, age :: integer }

def process_user(u) do
    case u do
        user{name: name, age: age} when age > 18 ->
            if name != "" do
                "adult user"
            else
                "unnamed adult"
            end
        user{age: age} when age <= 18 -> "minor"
        _ -> "invalid user"
    end
end'
	expected := '-module(test).
-export([process_user/1]).

-include("test.hrl").

-spec process_user(any()) -> binary().
process_user(U_1) ->
    case U_1 of
        #user{name = NAME_2, age = AGE_3} when AGE_3 > 18 ->
            case NAME_2 /= <<""/utf8>> of
        true -> <<"adult user"/utf8>>;
        false -> <<"unnamed adult"/utf8>>
    end;
        #user{age = AGE_3} when AGE_3 =< 18 ->
            <<"minor"/utf8>>;
        _ ->
            <<"invalid user"/utf8>>
    end.
'
	result := compile_lx(lx_code)
	assert result == expected
}

// Test control flow error cases
fn test_if_syntax_error() {
	lx_code := 'def test_function() do
    if x > 0
        "positive"
    end
end'

	result := compile_lx_with_error(lx_code)
	assert result.contains('Expected "do" after if condition')
}

fn test_case_syntax_error() {
	lx_code := 'def test_function() do
    case x
        1 -> "one"
    end
end'
	result := compile_lx_with_error(lx_code)
	assert result.contains('Expected "do" after case expression')
}

fn test_with_syntax_error() {
	lx_code := 'def test_function() do
    with pattern result do
        "ok"
    end
end'
	result := compile_lx_with_error(lx_code)
	assert result.contains('Expected "<-" after with pattern')
}

// Test if with complex conditions
fn test_if_complex_conditions() {
	lx_code := 'def test_complex(x, y, z) do
    if (x > 0) and (y < 10) or (z == 0) do
        "complex condition true"
    else
        "complex condition false"
    end
end'
	expected := '-module(test).
-export([test_complex/3]).

-spec test_complex(any(), any(), any()) -> binary().
test_complex(X_1, Y_2, Z_3) ->
    case (X_1 > 0) andalso (Y_2 < 10) orelse (Z_3 == 0) of
        true -> <<"complex condition true"/utf8>>;
        false -> <<"complex condition false"/utf8>>
    end.
'
	result := compile_lx(lx_code)
	assert result == expected
}

// Test case with multiple patterns per clause
fn test_case_multiple_patterns() {
	lx_code := 'def test_multiple(x) do
    case x do
        1 -> "one"
        2 -> "two"
        3 -> "three"
        n when n > 10 -> "big"
        _ -> "other"
    end
end'
	expected := '-module(test).
-export([test_multiple/1]).

-spec test_multiple(any()) -> binary().
test_multiple(X_1) ->
    case X_1 of
        1 ->
            <<"one"/utf8>>;
        2 ->
            <<"two"/utf8>>;
        3 ->
            <<"three"/utf8>>;
        N_2 when N_2 > 10 ->
            <<"big"/utf8>>;
        _ ->
            <<"other"/utf8>>
    end.
'
	result := compile_lx(lx_code)
	assert result == expected
}

// ============ Match Expression Tests ============

// Test simple match expression
fn test_match_simple() {
	lx_code := 'def test_simple_match() do
    data = {:ok, "success"}
    match {:ok, result} <- data
    result
end'

	expected := '-module(test).
-export([test_simple_match/0]).

-spec test_simple_match() -> any().
test_simple_match() ->
    DATA_1 = {ok, <<"success"/utf8>>},
    case DATA_1 of
        {ok, RESULT_2} ->
            RESULT_2;
        Otherwise ->
            Otherwise
    end.
'

	result := compile_lx(lx_code)
	assert result == expected
}

// Test match with single variable
fn test_match_single_variable() {
	lx_code := 'def test_single_variable() do
    data = "hello"
    match value <- data
    value
end'

	expected := '-module(test).
-export([test_single_variable/0]).

-spec test_single_variable() -> any().
test_single_variable() ->
    DATA_1 = <<"hello"/utf8>>,
    case DATA_1 of
        VALUE_2 ->
            VALUE_2;
        Otherwise ->
            Otherwise
    end.
'

	result := compile_lx(lx_code)
	assert result == expected
}

// Test nested match expressions
fn test_match_nested() {
	lx_code := 'def test_complex_match(value) do
    match {:ok, value1} <- value
    match {:ok, value2} <- value1
    :done
end'

	expected := '-module(test).
-export([test_complex_match/1]).

-spec test_complex_match(any()) -> atom().
test_complex_match(VALUE_1) ->
    case VALUE_1 of
        {ok, VALUE1_2} ->
            case VALUE1_2 of
        {ok, VALUE2_3} ->
            done;
        Otherwise ->
            Otherwise
    end;
        Otherwise ->
            Otherwise
    end.
'

	result := compile_lx(lx_code)
	assert result == expected
}

// Test match with list pattern
fn test_match_list_pattern() {
	lx_code := 'def test_list_match() do
    data = [1, 2, 3]
    match [head | tail] <- data
    {head, tail}
end'

	expected := '-module(test).
-export([test_list_match/0]).

-spec test_list_match() -> {any(), [any()]}.
test_list_match() ->
    DATA_1 = [1, 2, 3],
    case DATA_1 of
        [HEAD_2 | TAIL_3] ->
            {HEAD_2, TAIL_3};
        Otherwise ->
            Otherwise
    end.
'

	result := compile_lx(lx_code)
	assert result == expected
}

// Test match with tuple destructuring
fn test_match_tuple_destructuring() {
	lx_code := 'def test_tuple_match() do
    data = {1, "hello", :atom}
    match {num, str, atom_val} <- data
    {atom_val, str, num}
end'

	expected := '-module(test).
-export([test_tuple_match/0]).

-spec test_tuple_match() -> {any(), any(), any()}.
test_tuple_match() ->
    DATA_1 = {1, <<"hello"/utf8>>, atom},
    case DATA_1 of
        {NUM_2, STR_3, ATOM_VAL_4} ->
            {ATOM_VAL_4, STR_3, NUM_2};
        Otherwise ->
            Otherwise
    end.
'

	result := compile_lx(lx_code)
	assert result == expected
}

// Test match with rescue clause
fn test_match_with_rescue() {
	lx_code := 'def test_match_rescue() do
    data = {:error, "failed"}
    match {:ok, result} <- data rescue error do
        {:failed, error}
    end
    :done
end'

	expected := '-module(test).
-export([test_match_rescue/0]).

-spec test_match_rescue() -> {atom(), any()}.
test_match_rescue() ->
    DATA_1 = {error, <<"failed"/utf8>>},
    case DATA_1 of
        {ok, RESULT_2} ->
            done;
        ERROR_3 ->
            {failed, ERROR_3}
    end.
'

	result := compile_lx(lx_code)
	assert result == expected
}

// Test match with atom patterns
fn test_match_atom_patterns() {
	lx_code := 'def test_atom_match(status) do
    match :ok <- status
    "success"
end'

	expected := '-module(test).
-export([test_atom_match/1]).

-spec test_atom_match(any()) -> binary().
test_atom_match(STATUS_1) ->
    case STATUS_1 of
        ok ->
            <<"success"/utf8>>;
        Otherwise ->
            Otherwise
    end.
'

	result := compile_lx(lx_code)
	assert result == expected
}

// Test case with line breaks after arrow - basic case
fn test_case_with_line_breaks_basic() {
	lx_code := 'def test_case(x) do
    case x do
        1 ->
            "one"
        2 ->
            "two"
        _ ->
            "other"
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

// Test case with line breaks - record patterns
fn test_case_with_line_breaks_records() {
	lx_code := 'record User{ name :: string, age :: integer }

def process_user(u) do
    case u do
        User{name: _name, age: age} when age > 18 ->
            "adult user"
        User{age: age} when age <= 18 ->
            "minor"
        _ ->
            "invalid user"
    end
end'
	expected := '-module(test).
-export([process_user/1]).

-record(user, {name = nil :: binary(), age = nil :: integer()}).
-spec process_user(any()) -> binary().
process_user(U_1) ->
    case U_1 of
        #user{name = _NAME_2, age = AGE_3} when AGE_3 > 18 ->
            <<"adult user"/utf8>>;
        #user{age = AGE_3} when AGE_3 =< 18 ->
            <<"minor"/utf8>>;
        _ ->
            <<"invalid user"/utf8>>
    end.
'
	result := compile_lx(lx_code)
	assert result == expected
}

// Test case with line breaks - mixed inline and multiline
fn test_case_with_line_breaks_mixed() {
	lx_code := 'def test_mixed(x) do
    case x do
        1 -> "one"
        2 ->
            "two"
        3 ->
            "three"
        _ -> "other"
    end
end'
	expected := '-module(test).
-export([test_mixed/1]).

-spec test_mixed(any()) -> binary().
test_mixed(X_1) ->
    case X_1 of
        1 ->
            <<"one"/utf8>>;
        2 ->
            <<"two"/utf8>>;
        3 ->
            <<"three"/utf8>>;
        _ ->
            <<"other"/utf8>>
    end.
'
	result := compile_lx(lx_code)
	assert result == expected
}

// Test case with line breaks - complex expressions in body
fn test_case_with_line_breaks_complex() {
	lx_code := 'def test_complex(x) do
    case x do
        {:ok, value} ->
            value + 1
        {:error, reason} ->
            reason
        _ ->
            nil
    end
end'
	expected := '-module(test).
-export([test_complex/1]).

-spec test_complex(any()) -> any().
test_complex(X_1) ->
    case X_1 of
        {ok, VALUE_2} ->
            VALUE_2 + 1;
        {error, REASON_3} ->
            REASON_3;
        _ ->
            nil
    end.
'
	result := compile_lx(lx_code)
	assert result == expected
}
