module main

fn test_for_expression_simple() {
	lx_code := '
def simple_for() do
  for x in [1, 2, 3] do
    x * 2
  end
end'
	expected := '-module(test).
-export([simple_for/0]).

-spec simple_for() -> [integer()].
simple_for() ->
[X * 2 || X <- [1, 2, 3]].

'
	assert generates_erlang(lx_code) == expected
}

fn test_for_expression_with_guard() {
	lx_code := '
def for_with_guard() do
  for x in [1, 2, 3, 4, 5] when x > 3 do
    x * 3
  end
end'
	expected := '-module(test).
-export([for_with_guard/0]).

-spec for_with_guard() -> [integer()].
for_with_guard() ->
[X * 3 || X <- [1, 2, 3, 4, 5], X > 3].

'
	assert generates_erlang(lx_code) == expected
}

fn test_for_expression_with_variable() {
	lx_code := '
def for_with_variable() do
  numbers = [1, 2, 3, 4]
  for n in numbers do
    n + 1
  end
end'
	expected := '-module(test).
-export([for_with_variable/0]).

-spec for_with_variable() -> [integer()].
for_with_variable() ->
Numbers_aaaa = [1, 2, 3, 4],
[N + 1 || N <- Numbers_aaaa].

'
	assert generates_erlang(lx_code) == expected
}

fn test_for_expression_tuple_pattern() {
	lx_code := '
def for_tuple_pattern() do
  pairs = [{1, 2}, {3, 4}, {5, 6}]
  for {a, b} in pairs do
    a + b
  end
end'
	expected := '-module(test).
-export([for_tuple_pattern/0]).

-spec for_tuple_pattern() -> [integer()].
for_tuple_pattern() ->
Pairs_aaaa = [{1, 2}, {3, 4}, {5, 6}],
[A + B || {A, B} <- Pairs_aaaa].

'
	assert generates_erlang(lx_code) == expected
}

fn test_for_expression_complex_body() {
	lx_code := '
def for_complex_body() do
  for x in [1, 2, 3] do
    result = x * 2
    result + 1
  end
end'
	expected := '-module(test).
-export([for_complex_body/0]).

-spec for_complex_body() -> [integer()].
for_complex_body() ->
[Result_aaaa = X * 2,
    Result_aaaa + 1 || X <- [1, 2, 3]].

'
	assert generates_erlang(lx_code) == expected
}

fn test_for_expression_nested_data() {
	lx_code := '
def for_nested_data() do
  data = [[1, 2], [3, 4], [5, 6]]
  for sublist in data do
    length(sublist)
  end
end'
	expected := '-module(test).
-export([for_nested_data/0]).

-spec for_nested_data() -> [integer()].
for_nested_data() ->
Data_aaaa = [[1, 2], [3, 4], [5, 6]],
[length(Sublist) || Sublist <- Data_aaaa].

'
	assert generates_erlang(lx_code) == expected
}

fn test_for_expression_with_map_pattern() {
	lx_code := '
def for_map_pattern() do
  users = [%{name: "Alice", age: 30}, %{name: "Bob", age: 25}]
  for %{name: user_name, age: user_age} in users when user_age >= 30 do
    user_name
  end
end'
	expected := '-module(test).
-export([for_map_pattern/0]).

-spec for_map_pattern() -> [binary()].
for_map_pattern() ->
Users_aaaa = [#{name => <<"Alice"/utf8>>, age => 30}, #{name => <<"Bob"/utf8>>, age => 25}],
[User_name || #{name => User_name, age => User_age} <- Users_aaaa, User_age >= 30].

'
	assert generates_erlang(lx_code) == expected
}

fn test_for_expression_string_result() {
	lx_code := '
def for_string_result() do
  names = ["Alice", "Bob", "Charlie"]
  for name in names do
    name
  end
end'
	expected := '-module(test).
-export([for_string_result/0]).

-spec for_string_result() -> [binary()].
for_string_result() ->
Names_aaaa = [<<"Alice"/utf8>>, <<"Bob"/utf8>>, <<"Charlie"/utf8>>],
[Name || Name <- Names_aaaa].

'
	assert generates_erlang(lx_code) == expected
}

fn test_for_expression_atom_pattern() {
	lx_code := '
def for_atom_pattern() do
  statuses = [:ok, :error, :pending, :ok]
  for :ok in statuses do
    "success"
  end
end'
	expected := '-module(test).
-export([for_atom_pattern/0]).

-spec for_atom_pattern() -> [binary()].
for_atom_pattern() ->
Statuses_aaaa = [ok, error, pending, ok],
[<<"success"/utf8>> || ok <- Statuses_aaaa].

'
	assert generates_erlang(lx_code) == expected
}

fn test_for_expression_list_cons_pattern() {
	lx_code := '
def for_list_cons_pattern() do
  lists = [[1, 2, 3], [4, 5], [6]]
  for [head | _tail] in lists do
    head * 10
  end
end'
	expected := '-module(test).
-export([for_list_cons_pattern/0]).

-spec for_list_cons_pattern() -> [integer()].
for_list_cons_pattern() ->
Lists_aaaa = [[1, 2, 3], [4, 5], [6]],
[Head * 10 || [Head | _tail] <- Lists_aaaa].

'
	assert generates_erlang(lx_code) == expected
}

fn test_simple_list_literal() {
	lx_code := '
def simple_list() do
  [1, 2, 3, 4]
end'
	expected := '-module(test).
-export([simple_list/0]).

-spec simple_list() -> [integer()].
simple_list() ->
[1, 2, 3, 4].

'
	assert generates_erlang(lx_code) == expected
}

fn test_empty_list_literal() {
	lx_code := '
def empty_list() do
  []
end'
	expected := '-module(test).
-export([empty_list/0]).

-spec empty_list() -> [any()].
empty_list() ->
[].

'
	assert generates_erlang(lx_code) == expected
}

fn test_mixed_type_list_literal() {
	lx_code := '
def mixed_list() do
  [1, "hello", :atom, true]
end'
	expected := '-module(test).
-export([mixed_list/0]).

-spec mixed_list() -> [any()].
mixed_list() ->
[1, <<"hello"/utf8>>, atom, true].

'
	assert generates_erlang(lx_code) == expected
}

fn test_nested_list_literal() {
	lx_code := '
def nested_list() do
  [[1, 2], [3, 4], [5, 6]]
end'
	expected := '-module(test).
-export([nested_list/0]).

-spec nested_list() -> [[integer()]].
nested_list() ->
[[1, 2], [3, 4], [5, 6]].

'
	assert generates_erlang(lx_code) == expected
}

fn test_list_with_variables() {
	lx_code := '
def list_with_vars() do
  [1, 2, 3]
end'
	expected := '-module(test).
-export([list_with_vars/0]).

-spec list_with_vars() -> [integer()].
list_with_vars() ->
[1, 2, 3].

'
	assert generates_erlang(lx_code) == expected
}

fn test_list_assignment() {
	lx_code := '
def list_assignment() do
  numbers = [1, 2, 3, 4, 5]
  numbers
end'
	expected := '-module(test).
-export([list_assignment/0]).

-spec list_assignment() -> [integer()].
list_assignment() ->
Numbers_aaaa = [1, 2, 3, 4, 5],
Numbers_aaaa.

'
	assert generates_erlang(lx_code) == expected
}

fn test_list_with_function_calls() {
	lx_code := '
def list_with_calls() do
  [length([1, 2, 3]), length([4, 5])]
end'
	expected := '-module(test).
-export([list_with_calls/0]).

-spec list_with_calls() -> [integer()].
list_with_calls() ->
[length([1, 2, 3]), length([4, 5])].

'
	assert generates_erlang(lx_code) == expected
}
