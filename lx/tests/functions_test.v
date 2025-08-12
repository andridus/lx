module main

fn test_single_head_function_no_args() {
	lx_code := 'def answer() do
    42
end'

	expected := '-module(test).
-export([answer/0]).

-spec answer() -> integer().
answer() ->
    42.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_single_head_function_with_args() {
	lx_code := 'def add(a :: integer, b :: integer) do
    a + b
end'

	expected := '-module(test).
-export([add/2]).

-spec add(integer(), integer()) -> integer().
add(A_1, B_2) ->
    A_1 + B_2.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_single_head_function_with_string_args() {
	lx_code := 'def concat(a :: string, b :: string) do
    a ++ b
end'

	expected := '-module(test).
-export([concat/2]).

-spec concat(binary(), binary()) -> [any()].
concat(A_1, B_2) ->
    A_1 ++ B_2.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_single_head_function_with_complex_expression() {
	lx_code := 'def complex_calc(x :: integer, y :: integer) do
    (x + y) * 2
end'

	expected := '-module(test).
-export([complex_calc/2]).

-spec complex_calc(integer(), integer()) -> integer().
complex_calc(X_1, Y_2) ->
    (X_1 + Y_2) * 2.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_multiple_head_function_basic() {
	lx_code := 'def factorial :: integer do
    (0) -> 1
    (n :: integer) -> n * factorial(n - 1)
end'

	expected := '-module(test).
-export([factorial/1]).

-spec factorial(integer()) -> integer().
factorial(0) ->
    1;
factorial(N_1) ->
    N_1 * factorial(N_1 - 1).
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_multiple_head_function_with_different_types() {
	lx_code := 'def process_value do
    (0) -> "zero"
    (n :: integer) -> n * 2
end'

	expected := '-module(test).
-export([process_value/1]).

-spec process_value(integer()) -> binary() | integer().
process_value(0) ->
    <<"zero"/utf8>>;
process_value(N_1) ->
    N_1 * 2.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_multiple_head_function_with_multiple_args() {
	lx_code := 'def compare do
    (a, a) -> "equal"
    (a, b) -> "different"
end'

	expected := '-module(test).
-export([compare/2]).

-spec compare(any(), any()) -> binary().
compare(A_1, A_1) ->
    <<"equal"/utf8>>;
compare(A_1, B_2) ->
    <<"different"/utf8>>.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_multiple_head_function_with_literals() {
	lx_code := 'def check_status do
    (:ok) -> "success"
    (:error) -> "failure"
    (code) -> "unknown"
end'

	expected := '-module(test).
-export([check_status/1]).

-spec check_status(any()) -> binary().
check_status(ok) ->
    <<"success"/utf8>>;
check_status(error) ->
    <<"failure"/utf8>>;
check_status(CODE_1) ->
    <<"unknown"/utf8>>.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_multiple_head_function_with_complex_patterns() {
	lx_code := 'def process_list do
    ([]) -> "empty"
    ([head | tail]) -> "non_empty"
end'

	expected := '-module(test).
-export([process_list/1]).

-spec process_list(any()) -> binary().
process_list([]) ->
    <<"empty"/utf8>>;
process_list([HEAD_1 | TAIL_2]) ->
    <<"non_empty"/utf8>>.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_multiple_head_function_with_nested_expressions() {
	lx_code := 'def calculate do
    (0, y :: integer) -> y * 2
    (x :: integer, 0) -> x * 3
    (x :: integer, y :: integer) -> (x + y) * 4
end'

	expected := '-module(test).
-export([calculate/2]).

-spec calculate(integer(), integer()) -> integer().
calculate(0, Y_1) ->
    Y_1 * 2;
calculate(X_2, 0) ->
    X_2 * 3;
calculate(X_2, Y_1) ->
    (X_2 + Y_1) * 4.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_function_with_map_operations() {
	lx_code := 'def update_map(map, key, value) do
    map_put(map, key, value)
end'

	expected := '-module(test).
-export([update_map/3]).

-spec update_map(any(), any(), any()) -> map().
update_map(MAP_1, KEY_2, VALUE_3) ->
    maps:put(MAP_1, KEY_2, VALUE_3).
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_function_with_multiple_expressions() {
	lx_code := 'def multi_expr(a :: integer, b :: integer) do
    sum = a + b
    product = a * b
    sum + product
end'

	expected := '-module(test).
-export([multi_expr/2]).

-spec multi_expr(integer(), integer()) -> integer().
multi_expr(A_1, B_2) ->
    SUM_3 = A_1 + B_2,
    PRODUCT_4 = A_1 * B_2,
    SUM_3 + PRODUCT_4.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_function_with_recursive_call() {
	lx_code := 'def countdown :: string do
    (0) -> "done"
    (n :: integer) -> countdown(n - 1)
end'

	expected := '-module(test).
-export([countdown/1]).

-spec countdown(integer()) -> binary().
countdown(0) ->
    <<"done"/utf8>>;
countdown(N_1) ->
    countdown(N_1 - 1).
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_function_with_float_operations() {
	lx_code := 'def divide(a :: float, b :: float) do
    a / b
end'

	expected := '-module(test).
-export([divide/2]).

-spec divide(float(), float()) -> float().
divide(A_1, B_2) ->
    A_1 / B_2.
'

	result := compile_lx(lx_code)
	assert result == expected
}

// fn test_function_with_boolean_operations() {
// 	lx_code := 'def is_even do
//     (n :: integer) -> n % 2 == 0
// end'

// 	expected := '-module(test).
// -export([is_even/1]).

// -spec is_even(integer()) -> boolean().
// is_even(N_1) ->
//     N_1 % 2 == 0.
// '

// 	result := compile_lx(lx_code)
// 	assert result == expected
// }

fn test_function_with_mixed_numeric_types() {
	lx_code := 'def mixed_math(a :: float, b :: float) do
    a + b
end'

	expected := '-module(test).
-export([mixed_math/2]).

-spec mixed_math(float(), float()) -> float().
mixed_math(A_1, B_2) ->
    A_1 + B_2.
'

	result := compile_lx(lx_code)
	assert result == expected
}

// fn test_function_with_list_operations() {
// 	lx_code := 'def list_ops do
//     ([], item) -> [item]
//     ([head | tail], item) -> [head | list_ops(tail, item)]
// end'

// 	expected := '-module(test).
// -export([list_ops/2]).

// -spec list_ops(list(), any()) -> list().
// list_ops([], Item_1) ->
//     [Item_1];
// list_ops([Head_1 | Tail_2], Item_3) ->
//     [Head_1 | list_ops(Tail_2, Item_3)].
// '

// 	result := compile_lx(lx_code)
// 	assert result == expected
// }

// fn test_function_with_tuple_operations() {
// 	lx_code := 'def tuple_ops do
//     ({a, b}) -> a + b
//     ({a, b, c}) -> a + b + c
// end'

// 	expected := '-module(test).
// -export([tuple_ops/1]).

// -spec tuple_ops(tuple()) -> integer().
// tuple_ops({A_1, B_2}) ->
//     A_1 + B_2;
// tuple_ops({A_1, B_2, C_3}) ->
//     A_1 + B_2 + C_3.
// '

// 	result := compile_lx(lx_code)
// 	assert result == expected
// }

fn test_function_with_atom_patterns() {
	lx_code := 'def atom_patterns do
    (:success) -> "operation succeeded"
    (:failure) -> "operation failed"
    (:pending) -> "operation pending"
end'

	expected := '-module(test).
-export([atom_patterns/1]).

-spec atom_patterns(atom()) -> binary().
atom_patterns(success) ->
    <<"operation succeeded"/utf8>>;
atom_patterns(failure) ->
    <<"operation failed"/utf8>>;
atom_patterns(pending) ->
    <<"operation pending"/utf8>>.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_function_with_nil_patterns() {
	lx_code := 'def nil_patterns do
    (nil) -> "empty"
    (value) -> "has value"
end'

	expected := '-module(test).
-export([nil_patterns/1]).

-spec nil_patterns(any()) -> binary().
nil_patterns(nil) ->
    <<"empty"/utf8>>;
nil_patterns(VALUE_1) ->
    <<"has value"/utf8>>.
'

	result := compile_lx(lx_code)
	assert result == expected
}

// fn test_function_with_complex_nested_patterns() {
// 	lx_code := 'def complex_patterns do
//     ({:ok, data}) -> "success with data"
//     ({:error, reason}) -> "error with reason"
//     ({:ok, data, meta}) -> "success with data and meta"
// end'

// 	expected := '-module(test).
// -export([complex_patterns/2]).

// -spec complex_patterns(any()) -> binary().
// complex_patterns({ok, DATA_1}) ->
//     <<"success with data"/utf8>>;
// complex_patterns({error, REASON_2}) ->
//     <<"error with reason"/utf8>>;
// complex_patterns({ok, DATA_1, META_3}) ->
//     <<"success with data and meta"/utf8>>.
// '

// 	result := compile_lx(lx_code)
// 	assert result == expected
// }

// fn test_function_with_guard_like_patterns() {
// 	lx_code := 'def guard_patterns do
//     (0) -> "zero"
//     (1) -> "one"
//     (n) when n > 10 -> "large number"
//     (n) -> "small number"
// end'

// 	expected := '-module(test).
// -export([guard_patterns/1]).

// -spec guard_patterns(integer()) -> binary().
// guard_patterns(0) ->
//     <<"zero"/utf8>>;
// guard_patterns(1) ->
//     <<"one"/utf8>>;
// guard_patterns(N_1) when N_1 > 10 ->
//     <<"large number"/utf8>>;
// guard_patterns(N_1) ->
//     <<"small number"/utf8>>.
// '

// 	result := compile_lx(lx_code)
// 	assert result == expected
// }

fn test_function_with_string_patterns() {
	lx_code := 'def string_patterns do
    ("hello") -> "greeting"
    ("goodbye") -> "farewell"
    (str) -> "other string"
end'

	expected := '-module(test).
-export([string_patterns/1]).

-spec string_patterns(any()) -> binary().
string_patterns(<<"hello"/utf8>>) ->
    <<"greeting"/utf8>>;
string_patterns(<<"goodbye"/utf8>>) ->
    <<"farewell"/utf8>>;
string_patterns(STR_1) ->
    <<"other string"/utf8>>.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_function_with_boolean_patterns() {
	lx_code := 'def boolean_patterns do
    (true) -> "truth"
    (false) -> "falsehood"
end'

	expected := '-module(test).
-export([boolean_patterns/1]).

-spec boolean_patterns(boolean()) -> binary().
boolean_patterns(true) ->
    <<"truth"/utf8>>;
boolean_patterns(false) ->
    <<"falsehood"/utf8>>.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_function_with_numeric_patterns() {
	lx_code := 'def numeric_patterns do
    (0) -> "zero"
    (1) -> "one"
    (2) -> "two"
    (n) -> "other number"
end'

	expected := '-module(test).
-export([numeric_patterns/1]).

-spec numeric_patterns(any()) -> binary().
numeric_patterns(0) ->
    <<"zero"/utf8>>;
numeric_patterns(1) ->
    <<"one"/utf8>>;
numeric_patterns(2) ->
    <<"two"/utf8>>;
numeric_patterns(N_1) ->
    <<"other number"/utf8>>.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_function_with_float_patterns() {
	lx_code := 'def float_patterns do
    (0.0) -> "zero float"
    (1.0) -> "one float"
    (f) -> "other float"
end'

	expected := '-module(test).
-export([float_patterns/1]).

-spec float_patterns(any()) -> binary().
float_patterns(0.0) ->
    <<"zero float"/utf8>>;
float_patterns(1.0) ->
    <<"one float"/utf8>>;
float_patterns(F_1) ->
    <<"other float"/utf8>>.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_function_with_mixed_type_patterns() {
	lx_code := 'def mixed_type_patterns do
    (0) -> "integer zero"
    (0.0) -> "float zero"
    ("zero") -> "string zero"
    (:zero) -> "atom zero"
    (nil) -> "nil"
    (value) -> "other"
end'

	expected := '-module(test).
-export([mixed_type_patterns/1]).

-spec mixed_type_patterns(any()) -> binary().
mixed_type_patterns(0) ->
    <<"integer zero"/utf8>>;
mixed_type_patterns(0.0) ->
    <<"float zero"/utf8>>;
mixed_type_patterns(<<"zero"/utf8>>) ->
    <<"string zero"/utf8>>;
mixed_type_patterns(zero) ->
    <<"atom zero"/utf8>>;
mixed_type_patterns(nil) ->
    <<"nil"/utf8>>;
mixed_type_patterns(VALUE_1) ->
    <<"other"/utf8>>.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_application_block_basic() {
	lx_code := 'application {\n  description: "My App",\n  vsn: "0.1.0",\n  applications: [:kernel, :stdlib],\n  registered: [:main_server],\n  env: %{debug: true, port: 8080}\n}\n\ndef foo() do\n  1\nend'

	expected := '-module(test).\n-export([foo/0]).\n\n%% Application config:\n%%  description: <<"My App"/utf8>>\n%%  vsn: <<"0.1.0"/utf8>>\n%%  applications: [kernel, stdlib]\n%%  registered: [main_server]\n%%  env: #{debug => true, port => 8080}\n\n-spec foo() -> integer().\nfoo() ->\n    1.\n'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_application_block_empty_and_commas() {
	lx_code := 'application { }\n\napplication {\n  vsn: "1.0.0",\n}\n\ndef bar() do\n  2\nend'

	expected := '-module(test).\n-export([bar/0]).\n\n%% Application config:\n\n%% Application config:\n%%  vsn: <<"1.0.0"/utf8>>\n\n-spec bar() -> integer().\nbar() ->\n    2.\n'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_pattern_matching_in_function_arguments() {
	lx_code := 'def test_pattern({name, age}) do
  {name, age}
end

def main() do
  result = test_pattern({"João", 30})
  result
end'

	expected := '-module(test).
-export([test_pattern/1, main/0]).

-spec test_pattern(any()) -> {any(), any()}.
test_pattern(_1) ->
    {NAME_2, AGE_3}.
-spec main() -> {any(), any()}.
main() ->
    RESULT_4 = test_pattern({<<"João"/utf8>>, 30}),
    RESULT_4.
'

	result := compile_lx(lx_code)
	assert result == expected
}

// Forward References Tests
fn test_forward_reference_simple() {
	lx_code := 'def a() do
  b()
end

def b() do
  1
end'

	expected := '-module(test).
-export([a/0, b/0]).

-spec a() -> any().
a() ->
    b().
-spec b() -> integer().
b() ->
    1.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_forward_reference_multiple_functions() {
	lx_code := 'def main() do
  a()
end

def a() do
  b()
end

def b() do
  1
end

def c() do
  2
end'

	expected := '-module(test).
-export([main/0, a/0, b/0, c/0]).

-spec main() -> any().
main() ->
    a().
-spec a() -> any().
a() ->
    b().
-spec b() -> integer().
b() ->
    1.
-spec c() -> integer().
c() ->
    2.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_forward_reference_with_type_annotations() {
	lx_code := 'def process() do
  calculate()
end

def calculate() do
  42
end'

	expected := '-module(test).
-export([process/0, calculate/0]).

-spec process() -> any().
process() ->
    calculate().
-spec calculate() -> integer().
calculate() ->
    42.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_forward_reference_in_nested_calls() {
	lx_code := 'def entry() do
  step1()
end

def step1() do
  step2()
end

def step2() do
  step3()
end

def step3() do
  "done"
end'

	expected := '-module(test).
-export([entry/0, step1/0, step2/0, step3/0]).

-spec entry() -> any().
entry() ->
    step1().
-spec step1() -> any().
step1() ->
    step2().
-spec step2() -> any().
step2() ->
    step3().
-spec step3() -> binary().
step3() ->
    <<"done"/utf8>>.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_forward_reference_with_parameters() {
	lx_code := 'def start(x :: integer) do
  process(x)
end

def process(n :: integer) do
  n * 2
end'

	expected := '-module(test).
-export([start/1, process/1]).

-spec start(integer()) -> any().
start(X_1) ->
    process(X_1).
-spec process(integer()) -> integer().
process(N_2) ->
    N_2 * 2.
'

	result := compile_lx(lx_code)
	assert result == expected
}
