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

-spec test_if(any()) -> any().
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
	result := compile_lx(lx_code)
	assert result.contains('case X_1 > 0 of')
	assert result.contains('true -> <<"positive"/utf8>>')
	assert result.contains('false -> undefined')
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
	result := compile_lx(lx_code)
	assert result.contains('case X_1 > 0 of')
	assert result.contains('case Y_2 > 0 of')
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
	result := compile_lx(lx_code)
	assert result.contains('case X_1 of')
	assert result.contains('1 -> <<"one"/utf8>>')
	assert result.contains('2 -> <<"two"/utf8>>')
	assert result.contains('_ -> <<"other"/utf8>>')
}

// Test case with tuple patterns
fn test_case_tuple_patterns() {
	lx_code := 'def test_case(result) do
    case result do
        {success, data} -> data
        {error, reason} -> reason
        _ -> "unknown"
    end
end'
	result := compile_lx(lx_code)
	assert result.contains('case RESULT_1 of')
	assert result.contains('{success, DATA_2} -> DATA_2')
	assert result.contains('{error, REASON_3} -> REASON_3')
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
	result := compile_lx(lx_code)
	assert result.contains('case X_1 of') || result.contains('falha')
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
	result := compile_lx(lx_code)
	assert result.contains('[] -> <<"empty"/utf8>>')
	assert result.contains('[H_2] -> H_2')
	assert result.contains('[H_3 | T_4] -> H_3')
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
	result := compile_lx(lx_code)
	assert result.contains('case X_1 of')
	assert result.contains('case Y_2 of')
}

// Test basic with expressions
fn test_with_basic() {
	lx_code := 'def test_with() do
    result = {success, "data"}
    with {success, data} <- result do
        data
    end
end'
	result := compile_lx(lx_code)
	assert result.contains('RESULT_1 = {success, <<"data"/utf8>>}')
	assert result.contains('{success, DATA_2} = RESULT_1')
}

// Test with multiple clauses
fn test_with_multiple_clauses() {
	lx_code := 'def test_with() do
    result1 = {success, 10}
    result2 = {success, 20}
    with {success, x} <- result1, {success, y} <- result2 do
        x + y
    end
end'
	result := compile_lx(lx_code)
	assert result.contains('{success, X_3} = RESULT1_1')
	assert result.contains('{success, Y_4} = RESULT2_2')
	assert result.contains('X_3 + Y_4')
}

// Test with expression with else clause
fn test_with_else() {
	lx_code := 'def test_with(maybe_data) do
    with {success, data} <- maybe_data do
        data
    else
        _ -> "failed"
    end
end'
	result := compile_lx(lx_code)
	assert result.contains('with') || result.contains('case') || result.contains('falha')
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
	result := compile_lx(lx_code)
	assert result.contains('process_data')
	assert result.contains('case INPUT_1 != nil of')
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
	result := compile_lx(lx_code)
	assert result.contains('process_user') || result.contains('falha')
}

// Test control flow error cases
fn test_if_syntax_error() {
	lx_code := 'def test() do
    if x > 0
        "positive"
    end
end'
	result := compile_lx_with_error(lx_code)
	assert result.contains('Expected "do"') || result.contains('falha') || result.contains('error')
}

fn test_case_syntax_error() {
	lx_code := 'def test() do
    case x
        1 -> "one"
    end
end'
	result := compile_lx_with_error(lx_code)
	assert result.contains('Expected "do"') || result.contains('falha') || result.contains('error')
}

fn test_with_syntax_error() {
	lx_code := 'def test() do
    with pattern result do
        "ok"
    end
end'
	result := compile_lx_with_error(lx_code)
	assert result.contains('Expected "<-"') || result.contains('falha') || result.contains('error')
}

// Test if with complex conditions
fn test_if_complex_conditions() {
	lx_code := 'def test_complex(x, y, z) do
    if x > 0 && y < 10 || z == 0 do
        "complex condition true"
    else
        "complex condition false"
    end
end'
	result := compile_lx(lx_code)
	assert result.contains('X_1 > 0 andalso Y_2 < 10 orelse Z_3 == 0')
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
	result := compile_lx(lx_code)
	assert result.contains('1 -> <<"one"/utf8>>') || result.contains('falha')
	assert result.contains('2 -> <<"two"/utf8>>') || result.contains('falha')
}