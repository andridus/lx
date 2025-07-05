module main

fn test_map_literal_basic() {
	lx_code := '
def test_map() do
  %{name: "John", age: 30}
end'
	expected := '-module(main).
-export([test_map/0]).

-spec test_map() -> #{atom() => string()}.
test_map() ->
#{name => "John", age => 30}.

'
	erl1 := generates_erlang(lx_code)
	assert erl1.success
	assert erl1.code == expected
}

fn test_map_literal_with_fat_arrow() {
	lx_code := '
def test_map() do
  %{"name" => "John", "age" => 30}
end'
	expected := '-module(main).
-export([test_map/0]).

-spec test_map() -> #{string() => string()}.
test_map() ->
#{"name" => "John", "age" => 30}.

'
	erl1 := generates_erlang(lx_code)
	assert erl1.success
	assert erl1.code == expected
}

fn test_map_access() {
	lx_code := '
def get_name(user) do
  user[:name]
end'
	expected := '-module(main).
-export([get_name/1]).

-spec get_name(any()) -> any().
get_name(User) ->
maps:get(name, User).

'
	erl1 := generates_erlang(lx_code)
	assert erl1.success
	assert erl1.code == expected
}

fn test_map_update() {
	lx_code := '
def update_age(user) do
  %{user | age: 31}
end'
	expected := '-module(main).
-export([update_age/1]).

-spec update_age(any()) -> any().
update_age(User) ->
#{age => 31 | User}.

'
	erl1 := generates_erlang(lx_code)
	assert erl1.success
	assert erl1.code == expected
}

fn test_map_update_with_fat_arrow() {
	lx_code := '
def update_user(user) do
  %{user | "status" => "active"}
end'
	expected := '-module(main).
-export([update_user/1]).

-spec update_user(any()) -> any().
update_user(User) ->
#{"status" => "active" | User}.

'
	erl1 := generates_erlang(lx_code)
	assert erl1.success
	assert erl1.code == expected
}

fn test_map_pattern_matching() {
	lx_code := '
def extract_name(user) do
  case user do
    %{name: name} -> name
    _ -> "unknown"
  end
end'
	expected := '-module(main).
-export([extract_name/1]).

-spec extract_name(any()) -> string().
extract_name(User) ->
case User of
    #{name => Name} -> Name;
    _ -> "unknown"
end.

'
	erl1 := generates_erlang(lx_code)
	assert erl1.success
	assert erl1.code == expected
}

fn test_map_pattern_with_fat_arrow() {
	lx_code := '
def extract_data(data) do
  case data do
    %{"key" => value} -> value
    _ -> nil
  end
end'
	expected := '-module(main).
-export([extract_data/1]).

-spec extract_data(any()) -> any().
extract_data(Data) ->
case Data of
    #{"key" => Value} -> Value;
    _ -> nil
end.

'
	erl1 := generates_erlang(lx_code)
	assert erl1.success
	assert erl1.code == expected
}

fn test_nested_map_access() {
	lx_code := '
def get_db_host(config) do
  config[:database][:host]
end'
	expected := '-module(main).
-export([get_db_host/1]).

-spec get_db_host(any()) -> any().
get_db_host(Config) ->
maps:get(host, maps:get(database, Config)).

'
	erl1 := generates_erlang(lx_code)
	assert erl1.success
	assert erl1.code == expected
}

// TODO: Fix parsing issue with complex map example
/*
fn test_complex_map_example() {
	lx_code := '
def process_user(user) do
  updated_user = %{user | status: "active"}
  name = updated_user[:name]
  case updated_user do
    %{age: age} when age > 18 -> {name, "adult"}
    %{age: age} -> {name, "minor"}
    _ -> {name, "unknown"}
  end
end'
	expected := '-module(main).
-export([process_user/1]).

-spec process_user(any()) -> any().
process_user(User) ->
UpdatedUser = #{status => "active" | User},
Name = maps:get(name, UpdatedUser),
case UpdatedUser of
    #{age => Age} when Age > 18 -> {Name, "adult"};
    #{age => Age} -> {Name, "minor"};
    _ -> {Name, "unknown"}
end.

'
	erl1 := generates_erlang(lx_code)
	assert erl1.success
	assert erl1.code == expected
}
*/

fn test_empty_map() {
	lx_code := '
def empty_map() do
  %{}
end'
	expected := '-module(main).
-export([empty_map/0]).

-spec empty_map() -> #{}.
empty_map() ->
#{}.

'
	erl1 := generates_erlang(lx_code)
	assert erl1.success
	assert erl1.code == expected
}

fn test_map_with_variables() {
	lx_code := '
def create_user(name, age) do
  %{name: name, age: age, active: true}
end'
	expected := '-module(main).
-export([create_user/2]).

-spec create_user(any(), any()) -> #{atom() => any()}.
create_user(Name, Age) ->
#{name => Name, age => Age, active => true}.

'
	erl1 := generates_erlang(lx_code)
	assert erl1.success
	assert erl1.code == expected
}
