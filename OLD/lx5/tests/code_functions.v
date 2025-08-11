// module main

// fn test_simple_function() {
// 	lx_code := '
// def simple_function() do
//   "Hello from simple function"
// end'
// 	erl1 := generates_erlang(lx_code)
// 	expected := '-module(main).
// -export([simple_function/0]).

// -spec simple_function() -> string().
// simple_function() ->
// "Hello from simple function".

// '
// 	assert erl1.success
// 	// TODO: Fix string literals
// 	// assert erl1.code == expected
// }

// fn test_function_with_parameters() {
// 	lx_code := '
// def function_with_parameters(name, age) do
//   "Name: " ++ name ++ ", Age: " ++ integer_to_string(age)
// end'
// 	erl1 := generates_erlang(lx_code)
// 	expected := '-module(main).
// -export([function_with_parameters/2]).

// -spec function_with_parameters(string(), integer()) -> string().
// function_with_parameters(Name, Age) ->
// "Name: " ++ Name ++ ", Age: " ++ integer_to_string(Age).

// '
// 	assert erl1.success
// 	// TODO: Fix string concatenation and function calls
// 	// assert erl1.code == expected
// }

// fn test_multi_clause_factorial() {
// 	lx_code := '
// def factorial do
//   (0) do 1 end
//   (N) when N > 0 do N * factorial(N - 1) end
// end'
// 	erl1 := generates_erlang(lx_code)
// 	expected := '-module(main).
// -export([factorial/1]).

// -spec factorial(integer()) -> integer().
// factorial(0) ->
// 1;
// factorial(N) when N > 0 ->
// N * factorial(N - 1).

// '
// 	assert erl1.success
// 	// TODO: Fix multi-clause function definitions
// 	// assert erl1.code == expected
// }

// fn test_multi_clause_process_message() {
// 	lx_code := '
// def process_message do
//   (:hello) do "Hello there!" end
//   ({:user, name}) do "Hello user: " ++ name end
//   ({:admin, name, level}) do "Hello admin " ++ name ++ " (level " ++ integer_to_string(level) ++ ")" end
//   (_) do "Unknown message" end
// end'
// 	erl1 := generates_erlang(lx_code)
// 	expected := '-module(main).
// -export([process_message/1]).

// -spec process_message(atom() | {atom(), string()} | {atom(), string(), integer()}) -> string().
// process_message(hello) ->
// "Hello there!";
// process_message({user, Name}) ->
// "Hello user: " ++ Name;
// process_message({admin, Name, Level}) ->
// "Hello admin " ++ Name ++ " (level " ++ integer_to_string(Level) ++ ")";
// process_message(_) ->
// "Unknown message".

// '
// 	assert erl1.success
// 	// TODO: Fix multi-clause functions with complex patterns
// 	// assert erl1.code == expected
// }

// fn test_function_with_guards() {
// 	lx_code := '
// def classify_age do
//   (age) when age < 13 do "Child" end
//   (age) when age >= 13 andalso age < 20 do "Teenager" end
//   (age) when age >= 20 andalso age < 65 do "Adult" end
//   (age) when age >= 65 do "Senior" end
//   (_) do "Invalid age" end
// end'
// 	erl1 := generates_erlang(lx_code)
// 	expected := '-module(main).
// -export([classify_age/1]).

// -spec classify_age(integer()) -> string().
// classify_age(Age) when Age < 13 ->
// "Child";
// classify_age(Age) when Age >= 13 andalso Age < 20 ->
// "Teenager";
// classify_age(Age) when Age >= 20 andalso Age < 65 ->
// "Adult";
// classify_age(Age) when Age >= 65 ->
// "Senior";
// classify_age(_) ->
// "Invalid age".

// '
// 	assert erl1.success
// 	// TODO: Fix guards in function clauses
// 	// assert erl1.code == expected
// }

// fn test_complex_guards() {
// 	lx_code := '
// def validate_user do
//   (%{name: name, age: age}) when name != "" andalso age >= 0 andalso age <= 150 do
//     {:ok, "Valid user"}
//   end
//   (%{name: name, age: _age}) when name == "" do
//     {:error, "Name cannot be empty"}
//   end
//   (%{name: _name, age: age}) when age < 0 or age > 150 do
//     {:error, "Invalid age"}
//   end
//   (_) do
//     {:error, "Invalid user data"}
//   end
// end'
// 	erl1 := generates_erlang(lx_code)
// 	expected := '-module(main).
// -export([validate_user/1]).

// -spec validate_user(map()) -> {atom(), string()}.
// validate_user(#{name := Name, age := Age}) when Name /= "" andalso Age >= 0 andalso Age =< 150 ->
// {ok, "Valid user"};
// validate_user(#{name := Name, age := _Age}) when Name == "" ->
// {error, "Name cannot be empty"};
// validate_user(#{name := _Name, age := Age}) when Age < 0 orelse Age > 150 ->
// {error, "Invalid age"};
// validate_user(_) ->
// {error, "Invalid user data"}.

// '
// 	assert erl1.success
// 	// TODO: Fix map pattern matching and complex guards
// 	// assert erl1.code == expected
// }

// fn test_anonymous_functions() {
// 	lx_code := '
// def anonymous_functions() do
//   add = fn(x, y) do x + y end
//   result1 = add(3, 4)
//   {result1}
// end'
// 	erl1 := generates_erlang(lx_code)
// 	expected := '-module(main).
// -export([anonymous_functions/0]).

// -spec anonymous_functions() -> {integer()}.
// anonymous_functions() ->
// Add = fun(X, Y) -> X + Y end,
// Result1 = Add(3, 4),
// {Result1}.

// '
// 	assert erl1.success
// 	// TODO: Fix anonymous function syntax (fn -> fun)
// 	// assert erl1.code == expected
// }

// fn test_multi_clause_anonymous() {
// 	lx_code := '
// def multi_clause_anonymous() do
//   process = fn do
//     (:ok) do "Success" end
//     (:error) do "Failed" end
//     (_) do "Unknown" end
//   end
//   process(:ok)
// end'
// 	erl1 := generates_erlang(lx_code)
// 	expected := '-module(main).
// -export([multi_clause_anonymous/0]).

// -spec multi_clause_anonymous() -> string().
// multi_clause_anonymous() ->
// Process = fun
// (ok) -> "Success";
// (error) -> "Failed";
// (_) -> "Unknown"
// end,
// Process(ok).

// '
// 	assert erl1.success
// 	// TODO: Fix multi-clause anonymous functions
// 	// assert erl1.code == expected
// }

// fn test_higher_order_functions() {
// 	lx_code := '
// def higher_order_functions() do
//   make_adder = fn(n) do
//     fn(x) do x + n end
//   end
//   add5 = make_adder(5)
//   add5(10)
// end'
// 	erl1 := generates_erlang(lx_code)
// 	expected := '-module(main).
// -export([higher_order_functions/0]).

// -spec higher_order_functions() -> integer().
// higher_order_functions() ->
// Make_adder = fun(N) ->
// fun(X) -> X + N end
// end,
// Add5 = Make_adder(5),
// Add5(10).

// '
// 	assert erl1.success
// 	// TODO: Fix higher-order functions and closures
// 	// assert erl1.code == expected
// }
