module main

fn test_list_with_variables_simple() {
	lx_code := '
def list_with_variables() do
  x = 10
  y = 201
  [x, y]
end'
	expected := '-module(main).
-export([list_with_variables/0]).

-spec list_with_variables() -> [integer()].
list_with_variables() ->
X_aaaa = 10,
Y_baaa = 20,
[X_aaaa, Y_baaa].

'
	assert generates_erlang(lx_code) == expected
}

fn test_list_with_variables_mixed_types() {
	lx_code := '
def list_with_mixed_vars() do
  x = 10
  y = "hello"
  z = :atom
  [x, y, z]
end'
	expected := '-module(main).
-export([list_with_mixed_vars/0]).

-spec list_with_mixed_vars() -> [integer()].
list_with_mixed_vars() ->
X_aaaa = 10,
Y_baaa = "hello",
Z_caaa = atom,
[X_aaaa, Y_baaa, Z_caaa].

'
	assert generates_erlang(lx_code) == expected
}

fn test_list_with_variables_and_literals() {
	lx_code := '
def list_mixed_content() do
  x = 5
  y = 15
  [1, x, 10, y, 20]
end'
	expected := '-module(main).
-export([list_mixed_content/0]).

-spec list_mixed_content() -> [integer()].
list_mixed_content() ->
X_aaaa = 5,
Y_baaa = 15,
[1, X_aaaa, 10, Y_baaa, 20].

'
	assert generates_erlang(lx_code) == expected
}

fn test_list_with_variables_nested() {
	lx_code := '
def list_nested_vars() do
  x = 1
  y = 2
  z = 3
  [[x, y], [z, 4], [5, 6]]
end'
	expected := '-module(main).
-export([list_nested_vars/0]).

-spec list_nested_vars() -> [[integer()]].
list_nested_vars() ->
X_aaaa = 1,
Y_baaa = 2,
Z_caaa = 3,
[[X_aaaa, Y_baaa], [Z_caaa, 4], [5, 6]].

'
	assert generates_erlang(lx_code) == expected
}

fn test_list_with_variables_and_expressions() {
	lx_code := '
def list_with_expressions() do
  x = 10
  y = 5
  [x + y, x - y, x * y]
end'
	expected := '-module(main).
-export([list_with_expressions/0]).

-spec list_with_expressions() -> [integer()].
list_with_expressions() ->
X_aaaa = 10,
Y_baaa = 5,
[X_aaaa + Y_baaa, X_aaaa - Y_baaa, X_aaaa * Y_baaa].

'
	assert generates_erlang(lx_code) == expected
}

fn test_list_with_variables_and_function_calls() {
	lx_code := '
def list_with_function_calls() do
  x = [1, 2, 3]
  y = [4, 5]
  [length(x), length(y), length([6, 7, 8, 9])]
end'
	expected := '-module(main).
-export([list_with_function_calls/0]).

-spec list_with_function_calls() -> [any()].
list_with_function_calls() ->
X_aaaa = [1, 2, 3],
Y_baaa = [4, 5],
[length(X_aaaa), length(Y_baaa), length([6, 7, 8, 9])].

'
	assert generates_erlang(lx_code) == expected
}

fn test_list_with_variables_in_function_params() {
	lx_code := '
def list_with_params(x, y) do
  [x, y, x + y]
end'
	expected := '-module(main).
-export([list_with_params/2]).

-spec list_with_params(any(), any()) -> [any()].
list_with_params(X, Y) ->
[X, Y, X + Y].

'
	assert generates_erlang(lx_code) == expected
}

fn test_list_with_variables_and_guards() {
	lx_code := '
def list_with_guards(x, y) when x > 0 andalso y > 0 do
  [x, y, x * y]
end'
	expected := '-module(main).
-export([list_with_guards/2]).

-spec list_with_guards(any(), any()) -> [any()].
list_with_guards(X, Y) when X > 0 andalso Y > 0 ->
[X, Y, X * Y].

'
	assert generates_erlang(lx_code) == expected
}

fn test_list_with_variables_and_case() {
	lx_code := '
def list_with_case(x) do
  case x do
    1 -> [x, 10, 100]
    2 -> [x, 20, 200]
    _ -> [x, 0, 0]
  end
end'
	expected := '-module(main).
-export([list_with_case/1]).

-spec list_with_case(any()) -> [any()].
list_with_case(X) ->
case X of
    1 ->
        [X, 10, 100];
    2 ->
        [X, 20, 200];
    Other ->
        [X, 0, 0]
end.

'
	assert generates_erlang(lx_code) == expected
}

fn test_list_with_variables_and_if() {
	lx_code := '
def list_with_if(x) do
  if x > 0 do
    [x, "positive"]
  else
    [x, "negative"]
  end
end'
	expected := '-module(main).
-export([list_with_if/1]).

-spec list_with_if(any()) -> [any()].
list_with_if(X) ->
case X > 0 of
    true ->
        [X, "positive"];
    false ->
        [X, "negative"]
end.

'
	assert generates_erlang(lx_code) == expected
}
