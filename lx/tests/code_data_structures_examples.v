// module main

// fn test_tuple_examples() {
// 	lx_code := '
// def tuple_examples() do
//   point_2d = {10, 20}
//   point_3d = {10, 20, 30}
//   empty_tuple = {}
//   {point_2d, point_3d, empty_tuple}
// end'
// 	erl1 := generates_erlang(lx_code)
// 	expected := '-module(main).
// -export([tuple_examples/0]).

// -spec tuple_examples() -> {{integer(), integer()}, {integer(), integer(), integer()}, {}}.
// tuple_examples() ->
// Point_2d = {10, 20},
// Point_3d = {10, 20, 30},
// Empty_tuple = {},
// {Point_2d, Point_3d, Empty_tuple}.

// '
// 	assert erl1.success
// 	// TODO: Fix tuple handling and empty tuples
// 	// assert erl1.code == expected
// }

// fn test_list_examples() {
// 	lx_code := '
// def list_examples() do
//   empty_list = []
//   numbers = [1, 2, 3, 4, 5]
//   mixed_list = [1, "hello", :atom, 3.14]
//   {empty_list, numbers, mixed_list}
// end'
// 	erl1 := generates_erlang(lx_code)
// 	expected := '-module(main).
// -export([list_examples/0]).

// -spec list_examples() -> {[], [integer()], [integer() | string() | atom() | float()]}.
// list_examples() ->
// Empty_list = [],
// Numbers = [1, 2, 3, 4, 5],
// Mixed_list = [1, "hello", atom, 3.14],
// {Empty_list, Numbers, Mixed_list}.

// '
// 	assert erl1.success
// 	// TODO: Fix list handling and mixed types
// 	// assert erl1.code == expected
// }

// fn test_list_construction() {
// 	lx_code := '
// def list_construction() do
//   head = 0
//   tail = [1, 2, 3]
//   new_list = [head | tail]
//   new_list
// end'
// 	erl1 := generates_erlang(lx_code)
// 	expected := '-module(main).
// -export([list_construction/0]).

// -spec list_construction() -> [integer()].
// list_construction() ->
// Head = 0,
// Tail = [1, 2, 3],
// New_list = [Head | Tail],
// New_list.

// '
// 	assert erl1.success
// 	// TODO: Fix list cons operator
// 	// assert erl1.code == expected
// }

// fn test_map_examples() {
// 	lx_code := '
// def map_examples() do
//   user = %{name: "Alice", age: 30, active: true}
//   config = %{"database_url" => "localhost", "port" => 5432}
//   empty_map = %{}
//   {user, config, empty_map}
// end'
// 	erl1 := generates_erlang(lx_code)
// 	expected := '-module(main).
// -export([map_examples/0]).

// -spec map_examples() -> {#{name := string(), age := integer(), active := boolean()}, #{string() => string() | integer()}, #{}}.
// map_examples() ->
// User = #{name => "Alice", age => 30, active => true},
// Config = #{"database_url" => "localhost", "port" => 5432},
// Empty_map = #{},
// {User, Config, Empty_map}.

// '
// 	assert erl1.success
// 	// TODO: Fix map syntax and type inference
// 	// assert erl1.code == expected
// }

// fn test_map_access() {
// 	lx_code := '
// def map_access() do
//   user = %{name: "Bob", age: 25, active: true}
//   name = user[:name]
//   age = user[:age]
//   {name, age}
// end'
// 	erl1 := generates_erlang(lx_code)
// 	expected := '-module(main).
// -export([map_access/0]).

// -spec map_access() -> {string(), integer()}.
// map_access() ->
// User = #{name => "Bob", age => 25, active => true},
// Name = maps:get(name, User),
// Age = maps:get(age, User),
// {Name, Age}.

// '
// 	assert erl1.success
// 	// TODO: Fix map access syntax
// 	// assert erl1.code == expected
// }

// fn test_binary_examples() {
// 	lx_code := '
// def binary_examples() do
//   empty_binary = <<>>
//   bytes = <<1, 2, 3, 4>>
//   string_binary = <<"Hello World">>
//   {empty_binary, bytes, string_binary}
// end'
// 	erl1 := generates_erlang(lx_code)
// 	expected := '-module(main).
// -export([binary_examples/0]).

// -spec binary_examples() -> {binary(), binary(), binary()}.
// binary_examples() ->
// Empty_binary = <<>>,
// Bytes = <<1, 2, 3, 4>>,
// String_binary = <<"Hello World">>,
// {Empty_binary, Bytes, String_binary}.

// '
// 	assert erl1.success
// 	// TODO: Fix binary syntax
// 	// assert erl1.code == expected
// }

// fn test_binary_with_types() {
// 	lx_code := '
// def binary_with_types() do
//   integer_bin = <<42/integer>>
//   float_bin = <<3.14159/float>>
//   binary_spec = <<"data"/binary>>
//   {integer_bin, float_bin, binary_spec}
// end'
// 	erl1 := generates_erlang(lx_code)
// 	expected := '-module(main).
// -export([binary_with_types/0]).

// -spec binary_with_types() -> {binary(), binary(), binary()}.
// binary_with_types() ->
// Integer_bin = <<42/integer>>,
// Float_bin = <<3.14159/float>>,
// Binary_spec = <<"data"/binary>>,
// {Integer_bin, Float_bin, Binary_spec}.

// '
// 	assert erl1.success
// 	// TODO: Fix binary type specifiers
// 	// assert erl1.code == expected
// }

// fn test_binary_pattern_matching() {
// 	lx_code := '
// def binary_pattern_matching() do
//   data = <<42, 255:8, 1024:16, "hello"/binary>>
//   case data do
//     <<first, second:8, third:16, rest/binary>> ->
//       {first, second, third, rest}
//     _ ->
//       :error
//   end
// end'
// 	erl1 := generates_erlang(lx_code)
// 	expected := '-module(main).
// -export([binary_pattern_matching/0]).

// -spec binary_pattern_matching() -> {integer(), integer(), integer(), binary()} | atom().
// binary_pattern_matching() ->
// Data = <<42, 255:8, 1024:16, "hello"/binary>>,
// case Data of
// <<First, Second:8, Third:16, Rest/binary>> ->
// {First, Second, Third, Rest};
// _ ->
// error
// end.

// '
// 	assert erl1.success
// 	// TODO: Fix binary pattern matching
// 	// assert erl1.code == expected
// }
