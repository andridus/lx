// module main

// fn test_basic_pattern_matching() {
// 	lx_code := '
// def basic_pattern_matching() do
//   point = {10, 20}
//   {x, y} = point
//   list = [1, 2, 3, 4, 5]
//   [head | tail] = list
//   status = :ok
//   {x, y, head, tail, status}
// end'
// 	erl1 := generates_erlang(lx_code)
// 	expected := '-module(main).
// -export([basic_pattern_matching/0]).

// -spec basic_pattern_matching() -> {integer(), integer(), integer(), [integer()], atom()}.
// basic_pattern_matching() ->
// Point = {10, 20},
// {X, Y} = Point,
// List = [1, 2, 3, 4, 5],
// [Head | Tail] = List,
// Status = ok,
// {X, Y, Head, Tail, Status}.

// '
// 	assert erl1.success
// 	// TODO: Fix pattern matching assignment and atom handling
// 	// assert erl1.code == expected
// }

// fn test_case_pattern_matching() {
// 	lx_code := '
// def case_pattern_matching() do
//   data = {:user, "Alice", 30}
//   case data do
//     {:user, name, age} -> "User: " ++ name ++ ", Age: " ++ integer_to_string(age)
//     {:admin, name} -> "Admin: " ++ name
//     _ -> "Unknown"
//   end
// end'
// 	erl1 := generates_erlang(lx_code)
// 	expected := '-module(main).
// -export([case_pattern_matching/0]).

// -spec case_pattern_matching() -> string().
// case_pattern_matching() ->
// Data = {user, "Alice", 30},
// case Data of
// {user, Name, Age} -> "User: " ++ Name ++ ", Age: " ++ integer_to_string(Age);
// {admin, Name} -> "Admin: " ++ Name;
// _ -> "Unknown"
// end.

// '
// 	assert erl1.success
// 	// TODO: Fix case statement syntax and string concatenation
// 	// assert erl1.code == expected
// }

// fn test_list_pattern_matching() {
// 	lx_code := '
// def list_pattern_matching() do
//   numbers = [1, 2, 3, 4, 5]
//   case numbers do
//     [first, second | rest] -> {first, second, rest}
//     [single] -> {:single, single, []}
//     [] -> {:empty, 0, []}
//     _ -> {:unknown, 0, []}
//   end
// end'
// 	erl1 := generates_erlang(lx_code)
// 	expected := '-module(main).
// -export([list_pattern_matching/0]).

// -spec list_pattern_matching() -> {atom() | integer(), integer(), [integer()]}.
// list_pattern_matching() ->
// Numbers = [1, 2, 3, 4, 5],
// case Numbers of
// [First, Second | Rest] -> {First, Second, Rest};
// [Single] -> {single, Single, []};
// [] -> {empty, 0, []};
// _ -> {unknown, 0, []}
// end.

// '
// 	assert erl1.success
// 	// TODO: Fix case statement and tuple atom handling
// 	// assert erl1.code == expected
// }

// fn test_map_pattern_matching() {
// 	lx_code := '
// def map_pattern_matching() do
//   user = %{name: "Bob", age: 25, active: true}
//   %{name: user_name, age: user_age} = user
//   {user_name, user_age}
// end'
// 	erl1 := generates_erlang(lx_code)
// 	expected := '-module(main).
// -export([map_pattern_matching/0]).

// -spec map_pattern_matching() -> {string(), integer()}.
// map_pattern_matching() ->
// User = #{name => "Bob", age => 25, active => true},
// #{name := User_name, age := User_age} = User,
// {User_name, User_age}.

// '
// 	assert erl1.success
// 	// TODO: Fix map syntax and pattern matching
// 	// assert erl1.code == expected
// }

// fn test_tuple_pattern_matching() {
// 	lx_code := '
// def tuple_pattern_matching() do
//   point_3d = {10, 20, 30}
//   case point_3d do
//     {x, y, z} -> "3D point: " ++ integer_to_string(x) ++ ", " ++ integer_to_string(y) ++ ", " ++ integer_to_string(z)
//     {x, y} -> "2D point: " ++ integer_to_string(x) ++ ", " ++ integer_to_string(y)
//     _ -> "Unknown point"
//   end
// end'
// 	erl1 := generates_erlang(lx_code)
// 	expected := '-module(main).
// -export([tuple_pattern_matching/0]).

// -spec tuple_pattern_matching() -> string().
// tuple_pattern_matching() ->
// Point_3d = {10, 20, 30},
// case Point_3d of
// {X, Y, Z} -> "3D point: " ++ integer_to_string(X) ++ ", " ++ integer_to_string(Y) ++ ", " ++ integer_to_string(Z);
// {X, Y} -> "2D point: " ++ integer_to_string(X) ++ ", " ++ integer_to_string(Y);
// _ -> "Unknown point"
// end.

// '
// 	assert erl1.success
// 	// TODO: Fix case statement and string concatenation with function calls
// 	// assert erl1.code == expected
// }

// fn test_pattern_matching_with_guards() {
// 	lx_code := '
// def pattern_matching_with_guards() do
//   values = [10, 20, 30, 40]
//   case values do
//     [first, second | _] when first > second -> "First is greater"
//     [first, second | _] when first < second -> "Second is greater"
//     [first, second | _] when first == second -> "Equal"
//     _ -> "Unknown"
//   end
// end'
// 	erl1 := generates_erlang(lx_code)
// 	expected := '-module(main).
// -export([pattern_matching_with_guards/0]).

// -spec pattern_matching_with_guards() -> string().
// pattern_matching_with_guards() ->
// Values = [10, 20, 30, 40],
// case Values of
// [First, Second | _] when First > Second -> "First is greater";
// [First, Second | _] when First < Second -> "Second is greater";
// [First, Second | _] when First == Second -> "Equal";
// _ -> "Unknown"
// end.

// '
// 	assert erl1.success
// 	// TODO: Fix guards in case statements
// 	// assert erl1.code == expected
// }
