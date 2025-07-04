module main

fn test_simple_function_string() {
	lx_code := '
def f() do
1
end'
	erl1 := generates_erlang(lx_code)
	expected := '-module(main).
-export([f/0]).\n
-spec f() -> integer().
f() ->
1.\n
'
	assert erl1.success
	assert erl1.code == expected
}

fn test_type_alias_basic() {
	lx_code := '
type number :: integer
def f() do
   1
end'
	expected := '-module(main).
-export([f/0]).

-type number() :: integer().
-spec f() -> number().
f() ->
1.

'
	erl1 := generates_erlang(lx_code)
	assert erl1.success
	assert erl1.code == expected
}

fn test_type_alias_opaque() {
	lx_code := 'type opaque user_id :: integer
def f() do
1
end'
	expected := '-module(main).
-export([f/0]).

-opaque user_id() :: integer().
-spec f() -> user_id().
f() ->
1.

'
	erl1 := generates_erlang(lx_code)
	assert erl1.success
	assert erl1.code == expected
}

fn test_type_alias_nominal() {
	lx_code := 'type nominal celsius :: float
def f() do
1.0
end'
	expected := '-module(main).
-export([f/0]).

-nominal celsius() :: float().
-spec f() -> celsius().
f() ->
1.0.

'
	erl1 := generates_erlang(lx_code)
	assert erl1.success
	assert erl1.code == expected
}

fn test_type_alias_union_does_not_return() {
	lx_code := 'type id :: string | integer
def f() do
"test"
end'
	expected := '-module(main).
-export([f/0]).\n
-type id() :: string() | integer().
-spec f() -> string().
f() ->
"test".

'
	erl1 := generates_erlang(lx_code)
	assert erl1.success
	assert erl1.code == expected
}

fn test_type_alias_with_function_params() {
	lx_code := 'type number :: integer
def add(a :: number, b :: number) do
{a, b}
end'
	expected := '-module(main).
-export([add/2]).

-type number() :: integer().
-spec add(number(), number()) -> {number(), number()}.
add(A, B) ->
{A, B}.

'
	erl1 := generates_erlang(lx_code)
	assert erl1.success
	assert erl1.code == expected
}

// fn test_type_alias_opaque_with_function() {
// 	lx_code := 'type opaque user_id :: integer
// def create_user(name :: string, age :: integer) do
// %{id: 1, name: name, age: age}
// end'
// 	expected := '-module(main).
// -export([create_user/2]).

// -opaque user_id() :: integer().
// -spec create_user(string(), integer()) -> #{id := user_id(), name := string(), age := integer()}.
// create_user(Name, Age) ->
// #{id => 1, name => Name, age => Age}.

// '
// 	erl1 := generates_erlang(lx_code)
// 	assert erl1.success
// 	assert erl1.code == expected
// }

// fn test_multiple_type_aliases() {
// 	lx_code := 'type int :: integer
// type opaque number :: float | integer
// type nominal celsius :: float
// def convert(x :: int) do
// x * 1.8 + 32
// end'
// 	expected := '-module(main).
// -export([convert/1]).\n
// -type int() :: integer().
// -opaque number() :: float() | integer().
// -nominal celsius() :: float().
// -spec convert(int()) -> float().
// convert(X) ->
// X * 1.8 + 32.

// '
// 	erl1 := generates_erlang(lx_code)
// 	assert erl1.success
// 	assert erl1.code == expected
// }

fn test_type_alias_with_tuple() {
	lx_code := 'type nominal pair :: {integer, string}
def create_pair(x :: integer, y :: string) do
{x, y}
end'
	expected := '-module(main).
-export([create_pair/2]).

-nominal pair() :: {integer(), string()}.
-spec create_pair(integer(), string()) -> pair().
create_pair(X, Y) ->
{X, Y}.

'
	erl1 := generates_erlang(lx_code)
	assert erl1.success
	assert erl1.code == expected
}

// fn test_type_alias_with_list() {
// 	lx_code := 'type numbers :: [integer]
// def double_list(nums :: numbers) do
// for n in nums do
// n * 2
// end
// end'
// 	expected := '-module(main).
// -export([double_list/1]).

// -type numbers() :: [integer()].
// -spec double_list(numbers()) -> [integer()].
// double_list(Nums) ->
// [begin
// N * 2
// end || N <- Nums].

// '
// 	erl1 := generates_erlang(lx_code)
// 	assert erl1.success
// 	assert erl1.code == expected
// }

// fn test_type_alias_with_map() {
// 	lx_code := 'type user_map :: %{name: string, age: integer}
// def create_user(name :: string, age :: integer) do
// %{name: name, age: age}
// end'
// 	expected := '-module(main).
// -export([create_user/2]).

// -type user_map() :: #{name := string(), age := integer()}.
// -spec create_user(string(), integer()) -> user_map().
// create_user(Name, Age) ->
// #{name => Name, age => Age}.

// '
// 	erl1 := generates_erlang(lx_code)
// 	assert erl1.success
// 	assert erl1.code == expected
// }

// fn test_type_inference_with_aliases() {
// 	lx_code := 'type number :: integer
// def add(a :: number, b :: number) do
// a + b
// end'
// 	expected := '-module(main).
// -export([add/2]).\n
// -type number() :: integer().
// -spec add(number(), number()) -> integer().
// add(A, B) ->
// A + B.

// '
// 	erl1 := generates_erlang(lx_code)
// 	assert erl1.success
// 	assert erl1.code == expected
// }

// fn test_type_alias_with_boolean_operation() {
// 	lx_code := 'type flag :: boolean
// def is_positive(x :: integer) do
// x > 0
// end'
// 	expected := '-module(main).
// -export([is_positive/1]).

// -type flag() :: boolean().
// -spec is_positive(integer()) -> boolean().
// is_positive(X) ->
// X > 0.

// '
// 	erl1 := generates_erlang(lx_code)
// 	assert erl1.success
// 	assert erl1.code == expected
// }

fn test_if_expression_simple() {
	lx_code := '
def simple_if() do
  if true do
    42
  else
    0
  end
end'
	expected := '-module(main).
-export([simple_if/0]).

-spec simple_if() -> integer().
simple_if() ->
case true of
    true ->
        42;
    false ->
        0
end.

'
	erl1 := generates_erlang(lx_code)
	assert erl1.success
	assert erl1.code == expected
}

fn test_if_expression_with_variable() {
	lx_code := '
def if_with_var() do
  x = 10
  if x > 5 do
    "greater"
  else
    "lesser"
  end
end'
	expected := '-module(main).
-export([if_with_var/0]).

-spec if_with_var() -> string().
if_with_var() ->
X_aaaa = 10,
case X_aaaa > 5 of
    true ->
        "greater";
    false ->
        "lesser"
end.

'
	erl1 := generates_erlang(lx_code)
	assert erl1.success
	assert erl1.code == expected
}

fn test_if_expression_without_else() {
	lx_code := '
def if_no_else() do
  if false do
    "never"
  end
end'
	expected := '-module(main).
-export([if_no_else/0]).

-spec if_no_else() -> string().
if_no_else() ->
case false of
    true ->
        "never";
    false ->
        nil
end.

'
	erl1 := generates_erlang(lx_code)
	assert erl1.success
	assert erl1.code == expected
}

fn test_if_expression_nested() {
	lx_code := '
def nested_if() do
  x = 15
  if x > 10 do
    if x > 20 do
      "very large"
    else
      "medium"
    end
  else
    "small"
  end
end'
	expected := '-module(main).
-export([nested_if/0]).

-spec nested_if() -> string().
nested_if() ->
X_aaaa = 15,
case X_aaaa > 10 of
    true ->
        case X_aaaa > 20 of
    true ->
        "very large";
    false ->
        "medium"
end;
    false ->
        "small"
end.

'
	erl1 := generates_erlang(lx_code)
	assert erl1.success
	assert erl1.code == expected
}

fn test_if_expression_complex_condition() {
	lx_code := '
def complex_if() do
  x = 5
  y = 10
  if x + y > 12 do
    "sum is greater"
  else
    "sum is lesser"
  end
end'
	expected := '-module(main).
-export([complex_if/0]).

-spec complex_if() -> string().
complex_if() ->
X_aaaa = 5,
Y_baaa = 10,
case X_aaaa + Y_baaa > 12 of
    true ->
        "sum is greater";
    false ->
        "sum is lesser"
end.

'
	erl1 := generates_erlang(lx_code)
	assert erl1.success
	assert erl1.code == expected
}
