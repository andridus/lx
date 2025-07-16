module main

fn test_simple_function_string() {
	lx_code := '
def f() do
1
end'
	expected := '-module(test).
-export([f/0]).

-spec f() -> integer().
f() ->
1.

'
	code, hrl_content := generates_erlang(lx_code)
	assert code == expected
	assert hrl_content == ''
}

fn test_type_alias_basic() {
	lx_code := '
type number :: integer
def f() do
   1
end'
	expected := '-module(test).
-export([f/0]).


-spec f() -> integer().
f() ->
1.

'
	code, hrl_content := generates_erlang(lx_code)
	assert code == expected
	assert hrl_content == '%% Generated header file for test module
%% Contains records and type definitions

-type number() :: integer().
'
}

fn test_type_alias_opaque() {
	lx_code := 'type opaque user_id :: integer
def f() do
1
end'
	expected := '-module(test).
-export([f/0]).


-spec f() -> integer().
f() ->
1.

'
	code, hrl_content := generates_erlang(lx_code)
	assert code == expected
	assert hrl_content == '%% Generated header file for test module
%% Contains records and type definitions

-opaque user_id() :: integer().
'
}

fn test_type_alias_nominal() {
	lx_code := 'type nominal celsius :: float
def f() do
1.0
end'
	expected := '-module(test).
-export([f/0]).


-spec f() -> float().
f() ->
1.0.

'
	code, hrl_content := generates_erlang(lx_code)
	assert code == expected
	assert hrl_content == '%% Generated header file for test module
%% Contains records and type definitions

-nominal celsius() :: float().
'
}

fn test_type_alias_union_does_not_return() {
	lx_code := 'type id :: string | integer
def f() do
"test"
end'
	expected := '-module(test).
-export([f/0]).


-spec f() -> binary().
f() ->
<<"test"/utf8>>.

'
	code, hrl_content := generates_erlang(lx_code)
	assert code == expected
	assert hrl_content == '%% Generated header file for test module
%% Contains records and type definitions

-type id() :: binary() | integer().
'
}

fn test_type_alias_with_function_params() {
	lx_code := 'type number :: integer
def add(a :: number, b :: number) do
{a, b}
end'
	expected := '-module(test).
-export([add/2]).


-spec add(number(), number()) -> {number, number}.
add(A, B) when is_number(A) andalso is_number(B) ->
{A, B}.

'
	code, hrl_content := generates_erlang(lx_code)
	assert code == expected
	assert hrl_content == '%% Generated header file for test module
%% Contains records and type definitions

-type number() :: integer().
'
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
	expected := '-module(test).
-export([create_pair/2]).


-spec create_pair(integer(), binary()) -> {integer(), binary()}.
create_pair(X, Y) when is_integer(X) andalso is_binary(Y) ->
{X, Y}.

'
	code, hrl_content := generates_erlang(lx_code)
	assert code == expected
	assert hrl_content == '%% Generated header file for test module
%% Contains records and type definitions

-nominal pair() :: {integer(), binary()}.
'
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
	expected := '-module(test).
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
	code, hrl_content := generates_erlang(lx_code)
	assert code == expected
	assert hrl_content == ''
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
	expected := '-module(test).
-export([if_with_var/0]).

-spec if_with_var() -> binary().
if_with_var() ->
X_aaaa = 10,
case X_aaaa > 5 of
    true ->
        <<"greater"/utf8>>;
    false ->
        <<"lesser"/utf8>>
end.

'
	code, hrl_content := generates_erlang(lx_code)
	assert code == expected
	assert hrl_content == ''
}

fn test_if_expression_without_else() {
	lx_code := '
def if_no_else() do
  if false do
    "never"
  end
end'
	expected := '-module(test).
-export([if_no_else/0]).

-spec if_no_else() -> binary() | nil.
if_no_else() ->
case false of
    true ->
        <<"never"/utf8>>;
    false ->
        nil
end.

'
	code, hrl_content := generates_erlang(lx_code)
	assert code == expected
	assert hrl_content == ''
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
	expected := '-module(test).
-export([nested_if/0]).

-spec nested_if() -> binary().
nested_if() ->
X_aaaa = 15,
case X_aaaa > 10 of
    true ->
        case X_aaaa > 20 of
    true ->
        <<"very large"/utf8>>;
    false ->
        <<"medium"/utf8>>
end;
    false ->
        <<"small"/utf8>>
end.

'
	code, hrl_content := generates_erlang(lx_code)
	assert code == expected
	assert hrl_content == ''
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
	expected := '-module(test).
-export([complex_if/0]).

-spec complex_if() -> binary().
complex_if() ->
X_aaaa = 5,
Y_baaa = 10,
case X_aaaa + Y_baaa > 12 of
    true ->
        <<"sum is greater"/utf8>>;
    false ->
        <<"sum is lesser"/utf8>>
end.

'
	code, hrl_content := generates_erlang(lx_code)
	assert code == expected
	assert hrl_content == ''
}
