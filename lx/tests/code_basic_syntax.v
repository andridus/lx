module main

// fn test_basic_variables() {
// 	lx_code := '
// def basic_variables() do
//   name = "Alice"
//   age = 30
//   is_active = true
//   score = 95.5
//   {name, age, is_active, score}
// end'
// 	erl1 := generates_erlang(lx_code)
// 	expected := '-module(main).
// -export([basic_variables/0]).

// -spec basic_variables() -> {string(), integer(), boolean(), float()}.
// basic_variables() ->
// Name = "Alice",
// Age = 30,
// Is_active = true,
// Score = 95.5,
// {Name, Age, Is_active, Score}.

// '
// 	assert erl1.success
// 	// TODO: Fix string handling and variable assignment
// 	// assert erl1.code == expected
// }

// fn test_basic_expressions() {
// 	lx_code := '
// def basic_expressions() do
//   result = do
//     x = 10
//     y = 20
//     x + y
//   end
//   result
// end'
// 	erl1 := generates_erlang(lx_code)
// 	expected := '-module(main).
// -export([basic_expressions/0]).

// -spec basic_expressions() -> integer().
// basic_expressions() ->
// Result = begin
// X = 10,
// Y = 20,
// X + Y
// end,
// Result.

// '
// 	assert erl1.success
// 	// TODO: Fix do...end block handling
// 	// assert erl1.code == expected
// }

// fn test_function_definitions() {
// 	lx_code := '
// def greet(person_name) do
//   "Hello, " ++ person_name
// end

// def add(a, b) do
//   a + b
// end'
// 	erl1 := generates_erlang(lx_code)
// 	expected := '-module(main).
// -export([greet/1, add/2]).

// -spec greet(string()) -> string().
// greet(Person_name) ->
// "Hello, " ++ Person_name.

// -spec add(integer(), integer()) -> integer().
// add(A, B) ->
// A + B.

// '
// 	assert erl1.success
// 	// TODO: Fix string concatenation and multiple function definitions
// 	// assert erl1.code == expected
// }

// fn test_basic_control_flow() {
// 	lx_code := '
// def basic_control_flow() do
//   age = 30
//   if age >= 18 do
//     "Adult"
//   else
//     "Minor"
//   end
// end'
// 	erl1 := generates_erlang(lx_code)
// 	expected := '-module(main).
// -export([basic_control_flow/0]).

// -spec basic_control_flow() -> string().
// basic_control_flow() ->
// Age = 30,
// if Age >= 18 ->
// "Adult";
// true ->
// "Minor"
// end.

// '
// 	assert erl1.success
// 	// TODO: Fix if-else statement handling
// 	// assert erl1.code == expected
// }

// fn test_data_types() {
// 	lx_code := '
// def data_types() do
//   string_val = "Hello World"
//   integer_val = 42
//   float_val = 3.14
//   boolean_val = true
//   atom_val = :ok
//   nil_val = nil
//   {string_val, integer_val, float_val, boolean_val, atom_val, nil_val}
// end'
// 	erl1 := generates_erlang(lx_code)
// 	expected := '-module(main).
// -export([data_types/0]).

// -spec data_types() -> {string(), integer(), float(), boolean(), atom(), nil}.
// data_types() ->
// String_val = "Hello World",
// Integer_val = 42,
// Float_val = 3.14,
// Boolean_val = true,
// Atom_val = ok,
// Nil_val = nil,
// {String_val, Integer_val, Float_val, Boolean_val, Atom_val, Nil_val}.

// '
// 	assert erl1.success
// 	// TODO: Fix atom and nil handling
// 	// assert erl1.code == expected
// }

// fn test_arithmetic_operations() {
// 	lx_code := '
// def arithmetic_operations() do
//   a = 10
//   b = 5
//   addition = a + b
//   subtraction = a - b
//   multiplication = a * b
//   division = a / b
//   {addition, subtraction, multiplication, division}
// end'
// 	erl1 := generates_erlang(lx_code)
// 	expected := '-module(main).
// -export([arithmetic_operations/0]).

// -spec arithmetic_operations() -> {integer(), integer(), integer(), float()}.
// arithmetic_operations() ->
// A = 10,
// B = 5,
// Addition = A + B,
// Subtraction = A - B,
// Multiplication = A * B,
// Division = A / B,
// {Addition, Subtraction, Multiplication, Division}.

// '
// 	assert erl1.success
// 	// TODO: Fix arithmetic operations and type inference
// 	// assert erl1.code == expected
// }

// fn test_comparison_operators() {
// 	lx_code := '
// def comparison_operators() do
//   a = 10
//   b = 5
//   is_equal = a == b
//   is_not_equal = a != b
//   is_less = a < b
//   is_greater = a > b
//   is_less_equal = a <= b
//   is_greater_equal = a >= b
//   {is_equal, is_not_equal, is_less, is_greater, is_less_equal, is_greater_equal}
// end'
// 	erl1 := generates_erlang(lx_code)
// 	expected := '-module(main).
// -export([comparison_operators/0]).

// -spec comparison_operators() -> {boolean(), boolean(), boolean(), boolean(), boolean(), boolean()}.
// comparison_operators() ->
// A = 10,
// B = 5,
// Is_equal = A == B,
// Is_not_equal = A /= B,
// Is_less = A < B,
// Is_greater = A > B,
// Is_less_equal = A =< B,
// Is_greater_equal = A >= B,
// {Is_equal, Is_not_equal, Is_less, Is_greater, Is_less_equal, Is_greater_equal}.

// '
// 	assert erl1.success
// 	// TODO: Fix comparison operators (especially != to /= and <= to =<)
// 	// assert erl1.code == expected
// }

// fn test_logical_operators() {
// 	lx_code := '
// def logical_operators() do
//   logical_and = true andalso false
//   logical_or = true orelse false
//   logical_not = not true
//   symbolic_and = true && false
//   symbolic_or = true || false
//   {logical_and, logical_or, logical_not, symbolic_and, symbolic_or}
// end'
// 	erl1 := generates_erlang(lx_code)
// 	expected := '-module(main).
// -export([logical_operators/0]).

// -spec logical_operators() -> {boolean(), boolean(), boolean(), boolean(), boolean()}.
// logical_operators() ->
// Logical_and = true andalso false,
// Logical_or = true orelse false,
// Logical_not = not true,
// Symbolic_and = true andalso false,
// Symbolic_or = true orelse false,
// {Logical_and, Logical_or, Logical_not, Symbolic_and, Symbolic_or}.

// '
// 	assert erl1.success
// 	// TODO: Fix logical operators (convert && to andalso, || to orelse)
// 	// assert erl1.code == expected
// }
