module main

fn test_simple_match_basic() {
	lx_code := 'def test_basic() do
match {:ok, data} <- {:ok, 123}
data
end'

	expected_erlang := '-module(test).
-export([test_basic/0]).

-spec test_basic() -> {atom(), integer()} | integer().
test_basic() ->
case {ok, 123} of
    {ok, Data} ->
        Data;
    Error ->
        Error
end.

'

	code, hrl_content := generates_erlang(lx_code)
	assert code == expected_erlang
	assert hrl_content == ''
}

fn test_simple_match_list_cons() {
	lx_code := 'def test_cons() do
match [h | t] <- [1, 2, 3]
{h, t}
end'

	expected_erlang := '-module(test).
-export([test_cons/0]).

-spec test_cons() -> [integer()] | {integer(), [integer()]}.
test_cons() ->
case [1, 2, 3] of
    [H | T] ->
        {H, T};
    Error ->
        Error
end.

'

	code, hrl_content := generates_erlang(lx_code)
	assert code == expected_erlang
	assert hrl_content == ''
}

fn test_simple_match_multiple_sequential() {
	lx_code := 'def test_sequential() do
match {:ok, user} <- get_user()
match {:ok, perms} <- get_permissions(user)
{user, perms}
end'

	expected_erlang := '-module(test).
-export([test_sequential/0]).

-spec test_sequential() -> any().
test_sequential() ->
case get_user() of
    {ok, User} ->
        case get_permissions(User) of
    {ok, Perms} ->
        {User, Perms};
    Error ->
        Error
end;
    Error ->
        Error
end.

'

	code, hrl_content := generates_erlang(lx_code)
	assert code == expected_erlang
	assert hrl_content == ''
}

fn test_simple_match_three_sequential() {
	lx_code := 'def test_cons() do
match [h | t] <- [1, 2, 3]
match [h | t] <- [1, 2, 3]
match [h | t] <- [1, 2, 3]
{h, t}
end'

	expected_erlang := '-module(test).
-export([test_cons/0]).

-spec test_cons() -> [integer()] | {integer(), [integer()]}.
test_cons() ->
case [1, 2, 3] of
    [H | T] ->
        case [1, 2, 3] of
    [H | T] ->
        case [1, 2, 3] of
    [H | T] ->
        {H, T};
    Error ->
        Error
end;
    Error ->
        Error
end;
    Error ->
        Error
end.

'
	code, hrl_content := generates_erlang(lx_code)
	assert code == expected_erlang
	assert hrl_content == ''
}

// TODO: Fix map pattern matching - lexer issue with colon in maps
// fn test_simple_match_map_pattern() {
// 	lx_code := 'def test_map() do
// match %{name: user_name, age: user_age} <- get_user_data()
// {user_name, user_age}
// end'
//
// 	expected_erlang := '-module(main).
// -export([test_map/0]).
//
// -spec test_map() -> {user_name(), user_age()}.
// test_map() ->
// case get_user_data() of
//     #{name := User_name, age := User_age} ->
//         {User_name, User_age};
//     Other ->
//         Other
// end.'
//
// 	assert_lx_generates_erlang(lx_code, expected_erlang)
// }

fn test_simple_match_tuple_pattern() {
	lx_code := 'def test_tuple() do
match {x, y, z} <- get_coordinates()
{x, y, z}
end'

	expected_erlang := '-module(test).
-export([test_tuple/0]).

-spec test_tuple() -> any().
test_tuple() ->
case get_coordinates() of
    {X, Y, Z} ->
        {X, Y, Z};
    Error ->
        Error
end.

'

	code, hrl_content := generates_erlang(lx_code)
	assert code == expected_erlang
	assert hrl_content == ''
}

fn test_simple_match_atom_pattern() {
	lx_code := 'def test_atom() do
match :ok <- process_data()
"success"
end'

	expected_erlang := '-module(test).
-export([test_atom/0]).

-spec test_atom() -> any().
test_atom() ->
case process_data() of
    ok ->
        <<"success"/utf8>>;
    Error ->
        Error
end.

'

	code, hrl_content := generates_erlang(lx_code)
	assert code == expected_erlang
	assert hrl_content == ''
}

fn test_simple_match_with_computation() {
	lx_code := 'def test_computation() do
match {:ok, value} <- compute_value(10, 20)
value
end'

	expected_erlang := '-module(test).
-export([test_computation/0]).

-spec test_computation() -> integer().
test_computation() ->
case compute_value(10, 20) of
    {ok, Value} ->
        Value;
    Error ->
        Error
end.

'

	code, hrl_content := generates_erlang(lx_code)
	assert code == expected_erlang
	assert hrl_content == ''
}

fn test_simple_match_empty_list() {
	lx_code := 'def test_empty() do
match [] <- get_list()
"empty list"
end'

	expected_erlang := '-module(test).
-export([test_empty/0]).

-spec test_empty() -> any().
test_empty() ->
case get_list() of
    [] ->
        <<"empty list"/utf8>>;
    Error ->
        Error
end.

'

	code, hrl_content := generates_erlang(lx_code)
	assert code == expected_erlang
	assert hrl_content == ''
}

fn test_simple_match_mixed_with_rescue() {
	lx_code := 'def test_mixed() do
match {:ok, user} <- get_user()
match {:ok, data} <- get_data(user) rescue error do
  {:error, error}
end
data
end'

	expected_erlang := '-module(test).
-export([test_mixed/0]).

-spec test_mixed() -> any().
test_mixed() ->
case get_user() of
    {ok, User} ->
        case get_data(User) of
    {ok, Data} ->
        Data;
    Error ->
        {error, Error}
end;
    Error ->
        Error
end.

'

	code, hrl_content := generates_erlang(lx_code)
	assert code == expected_erlang
	assert hrl_content == ''
}
