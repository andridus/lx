// module main

// fn test_if_else_examples() {
// 	lx_code := '
// def if_else_examples() do
//   age = 25
//   age_status = if age >= 18 do
//     "Adult"
//   else
//     "Minor"
//   end
//   age_status
// end'
// 	erl1 := generates_erlang(lx_code)
// 	expected := '-module(main).
// -export([if_else_examples/0]).

// -spec if_else_examples() -> string().
// if_else_examples() ->
// Age = 25,
// Age_status = if Age >= 18 ->
// "Adult";
// true ->
// "Minor"
// end,
// Age_status.

// '
// 	assert erl1.success
// 	// TODO: Fix if-else statements
// 	// assert erl1.code == expected
// }

// fn test_case_examples() {
// 	lx_code := '
// def case_examples() do
//   status = :ok
//   case status do
//     :ok -> "Success"
//     :error -> "Error occurred"
//     :timeout -> "Operation timed out"
//     _ -> "Unknown status"
//   end
// end'
// 	erl1 := generates_erlang(lx_code)
// 	expected := '-module(main).
// -export([case_examples/0]).

// -spec case_examples() -> string().
// case_examples() ->
// Status = ok,
// case Status of
// ok -> "Success";
// error -> "Error occurred";
// timeout -> "Operation timed out";
// _ -> "Unknown status"
// end.

// '
// 	assert erl1.success
// 	// TODO: Fix case statements and atom handling
// 	// assert erl1.code == expected
// }

// fn test_case_with_guards() {
// 	lx_code := '
// def case_with_guards() do
//   number = 15
//   case number do
//     n when n < 0 -> "Negative"
//     n when n == 0 -> "Zero"
//     n when n > 0 andalso n < 10 -> "Single digit positive"
//     n when n >= 10 andalso n < 100 -> "Double digit positive"
//     _ -> "Large positive number"
//   end
// end'
// 	erl1 := generates_erlang(lx_code)
// 	expected := '-module(main).
// -export([case_with_guards/0]).

// -spec case_with_guards() -> string().
// case_with_guards() ->
// Number = 15,
// case Number of
// N when N < 0 -> "Negative";
// N when N == 0 -> "Zero";
// N when N > 0 andalso N < 10 -> "Single digit positive";
// N when N >= 10 andalso N < 100 -> "Double digit positive";
// _ -> "Large positive number"
// end.

// '
// 	assert erl1.success
// 	// TODO: Fix case with guards
// 	// assert erl1.code == expected
// }

// fn test_with_expressions() {
// 	lx_code := '
// def with_expressions() do
//   user_data = %{name: "Bob", age: 25}
//   with {:ok, name} <= get_name(user_data) do
//     "Hello, " ++ name
//   else
//     "Unknown user"
//   end
// end'
// 	erl1 := generates_erlang(lx_code)
// 	expected := '-module(main).
// -export([with_expressions/0]).

// -spec with_expressions() -> string().
// with_expressions() ->
// User_data = #{name => "Bob", age => 25},
// case get_name(User_data) of
// {ok, Name} -> "Hello, " ++ Name;
// _ -> "Unknown user"
// end.

// '
// 	assert erl1.success
// 	// TODO: Fix with expressions (convert to case statements)
// 	// assert erl1.code == expected
// }

// fn test_for_loop_examples() {
// 	lx_code := '
// def for_loop_examples() do
//   numbers = [1, 2, 3, 4, 5]
//   for x in numbers do
//     x * 2
//   end
// end'
// 	erl1 := generates_erlang(lx_code)
// 	expected := '-module(main).
// -export([for_loop_examples/0]).

// -spec for_loop_examples() -> [integer()].
// for_loop_examples() ->
// Numbers = [1, 2, 3, 4, 5],
// [begin
// X * 2
// end || X <- Numbers].

// '
// 	assert erl1.success
// 	// TODO: Fix for loops (convert to list comprehensions)
// 	// assert erl1.code == expected
// }

// fn test_for_loop_with_guards() {
// 	lx_code := '
// def for_loop_with_guards() do
//   numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
//   for x in numbers when x % 2 == 0 do
//     x
//   end
// end'
// 	erl1 := generates_erlang(lx_code)
// 	expected := '-module(main).
// -export([for_loop_with_guards/0]).

// -spec for_loop_with_guards() -> [integer()].
// for_loop_with_guards() ->
// Numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
// [begin
// X
// end || X <- Numbers, X rem 2 == 0].

// '
// 	assert erl1.success
// 	// TODO: Fix for loops with guards and modulo operator
// 	// assert erl1.code == expected
// }

// fn test_nested_control_flow() {
// 	lx_code := '
// def nested_control_flow() do
//   data = [
//     {:user, "Alice", 25},
//     {:admin, "Bob", 2},
//     {:guest, "Charlie"}
//   ]
//   for item in data do
//     case item do
//       {:user, name, age} when age >= 18 ->
//         "Adult user: " ++ name
//       {:user, name, age} when age < 18 ->
//         "Minor user: " ++ name
//       {:admin, name, level} when level >= 3 ->
//         "Senior admin: " ++ name
//       {:admin, name, level} when level < 3 ->
//         "Junior admin: " ++ name
//       {:guest, name} ->
//         "Guest: " ++ name
//       _ ->
//         "Unknown"
//     end
//   end
// end'
// 	erl1 := generates_erlang(lx_code)
// 	expected := '-module(main).
// -export([nested_control_flow/0]).

// -spec nested_control_flow() -> [string()].
// nested_control_flow() ->
// Data = [
// {user, "Alice", 25},
// {admin, "Bob", 2},
// {guest, "Charlie"}
// ],
// [begin
// case Item of
// {user, Name, Age} when Age >= 18 ->
// "Adult user: " ++ Name;
// {user, Name, Age} when Age < 18 ->
// "Minor user: " ++ Name;
// {admin, Name, Level} when Level >= 3 ->
// "Senior admin: " ++ Name;
// {admin, Name, Level} when Level < 3 ->
// "Junior admin: " ++ Name;
// {guest, Name} ->
// "Guest: " ++ Name;
// _ ->
// "Unknown"
// end
// end || Item <- Data].

// '
// 	assert erl1.success
// 	// TODO: Fix nested control flow with for loops and case statements
// 	// assert erl1.code == expected
// }
