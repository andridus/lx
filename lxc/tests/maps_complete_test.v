module main

fn test_map_literal_basic() {
	lx_code := '
def test_map() do
  %{name: "John", age: 30}
end'
	expected := '-module(test).
-export([test_map/0]).

-spec test_map() -> #{atom() => any()}.
test_map() ->
#{name => <<"John"/utf8>>, age => 30}.

'
	assert generates_erlang(lx_code) == expected
}

fn test_map_literal_with_fat_arrow() {
	lx_code := '
def test_map() do
  %{"name" => "John", "age" => 30}
end'

	expected := '-module(test).
-export([test_map/0]).

-spec test_map() -> #{binary() => any()}.
test_map() ->
#{<<"name"/utf8>> => <<"John"/utf8>>, <<"age"/utf8>> => 30}.

'
	assert generates_erlang(lx_code) == expected
}

fn test_map_access() {
	lx_code := '
def get_name(user) do
  user[:name]
end'

	expected := '-module(test).
-export([get_name/1]).

-spec get_name(any()) -> any().
get_name(User) ->
maps:get(name, User).

'
	assert generates_erlang(lx_code) == expected
}

fn test_map_update() {
	lx_code := '
def update_age(user) do
  %{user | age: 31}
end'

	expected := '-module(test).
-export([update_age/1]).

-spec update_age(any()) -> any().
update_age(User) ->
User#{age => 31}.

'
	assert generates_erlang(lx_code) == expected
}

fn test_map_update_with_fat_arrow() {
	lx_code := '
def update_user(user) do
  %{user | "status" => "active"}
end'

	expected := '-module(test).
-export([update_user/1]).

-spec update_user(any()) -> any().
update_user(User) ->
User#{<<"status"/utf8>> => <<"active"/utf8>>}.

'
	assert generates_erlang(lx_code) == expected
}

fn test_empty_map() {
	lx_code := '
def empty_map() do
  %{}
end'

	expected := '-module(test).
-export([empty_map/0]).

-spec empty_map() -> #{any() => any()}.
empty_map() ->
#{}.

'
	assert generates_erlang(lx_code) == expected
}

fn test_map_with_variables() {
	lx_code := '
def create_user(name) do
  %{name: name}
end'

	expected := '-module(test).
-export([create_user/1]).

-spec create_user(any()) -> #{atom() => any()}.
create_user(Name) ->
#{name => Name}.

'
	assert generates_erlang(lx_code) == expected
}

fn test_nested_map_access() {
	lx_code := '
def get_db_host(config) do
  config[:database][:host]
end'

	expected := '-module(test).
-export([get_db_host/1]).

-spec get_db_host(any()) -> any().
get_db_host(Config) ->
maps:get(host, maps:get(database, Config)).

'
	assert generates_erlang(lx_code) == expected
}

fn test_map_type_inference() {
	lx_code := '
def create_person() do
  %{name: "John", age: 30}
end'

	expected := '-module(test).
-export([create_person/0]).

-spec create_person() -> #{atom() => any()}.
create_person() ->
#{name => <<"John"/utf8>>, age => 30}.

'
	assert generates_erlang(lx_code) == expected
}
