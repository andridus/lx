module main

// Helper function to assert that LX code generates expected Erlang code
fn assert_lx_generates_erlang(lx_code string, expected_erlang string) {
	result := generates_erlang(lx_code)
	assert result.success, 'Code generation failed: ${result.errors}'
	assert result.code.trim_space() == expected_erlang.trim_space(), 'Generated Erlang does not match expected.\nExpected:\n${expected_erlang}\nActual:\n${result.code}'
}

fn test_simple_match_basic() {
	lx_code := 'def test_basic() do
match {:ok, data} <- {:ok, 123}
data
end'

	expected_erlang := '-module(main).
-export([test_basic/0]).

-spec test_basic() -> data().
test_basic() ->
case {ok, 123} of
    {ok, Data} ->
        Data;
    Other ->
        Other
end.'

	assert_lx_generates_erlang(lx_code, expected_erlang)
}

fn test_simple_match_list_cons() {
	lx_code := 'def test_cons() do
match [h | t] <- [1, 2, 3]
{h, t}
end'

	expected_erlang := '-module(main).
-export([test_cons/0]).

-spec test_cons() -> {h(), t()}.
test_cons() ->
case [1, 2, 3] of
    [H | T] ->
        {H, T};
    Other ->
        Other
end.'

	assert_lx_generates_erlang(lx_code, expected_erlang)
}

fn test_simple_match_multiple_sequential() {
	lx_code := 'def test_sequential() do
match {:ok, user} <- get_user()
match {:ok, perms} <- get_permissions(user)
{user, perms}
end'

	expected_erlang := '-module(main).
-export([test_sequential/0]).

-spec test_sequential() -> {user(), perms()}.
test_sequential() ->
case get_user() of
    {ok, User} ->
        case get_permissions(User) of
            {ok, Perms} ->
                {User, Perms};
            Other ->
                Other
        end;
    Other ->
        Other
end.'

	assert_lx_generates_erlang(lx_code, expected_erlang)
}

fn test_simple_match_three_sequential() {
	lx_code := 'def test_cons() do
match [h | t] <- [1, 2, 3]
match [h | t] <- [1, 2, 3]
match [h | t] <- [1, 2, 3]
{h, t}
end'

	expected_erlang := '-module(main).
-export([test_cons/0]).

-spec test_cons() -> {h(), t()}.
test_cons() ->
case [1, 2, 3] of
    [H | T] ->
        case [1, 2, 3] of
            [H | T] ->
                case [1, 2, 3] of
                    [H | T] ->
                        {H, T};
                    Other ->
                        Other
                end;
            Other ->
                Other
        end;
    Other ->
        Other
end.'

	assert_lx_generates_erlang(lx_code, expected_erlang)
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
x + y + z
end'

	expected_erlang := '-module(main).
-export([test_tuple/0]).

-spec test_tuple() -> any().
test_tuple() ->
case get_coordinates() of
    {X, Y, Z} ->
        X + Y + Z;
    Other ->
        Other
end.'

	assert_lx_generates_erlang(lx_code, expected_erlang)
}

fn test_simple_match_atom_pattern() {
	lx_code := 'def test_atom() do
match :ok <- process_data()
"success"
end'

	expected_erlang := '-module(main).
-export([test_atom/0]).

-spec test_atom() -> string().
test_atom() ->
case process_data() of
    ok ->
        "success";
    Other ->
        Other
end.'

	assert_lx_generates_erlang(lx_code, expected_erlang)
}

fn test_simple_match_with_computation() {
	lx_code := 'def test_computation() do
match {:ok, value} <- compute_value(10, 20)
value * 2
end'

	expected_erlang := '-module(main).
-export([test_computation/0]).

-spec test_computation() -> any().
test_computation() ->
case compute_value(10, 20) of
    {ok, Value} ->
        Value * 2;
    Other ->
        Other
end.'

	assert_lx_generates_erlang(lx_code, expected_erlang)
}

fn test_simple_match_empty_list() {
	lx_code := 'def test_empty() do
match [] <- get_list()
"empty list"
end'

	expected_erlang := '-module(main).
-export([test_empty/0]).

-spec test_empty() -> string().
test_empty() ->
case get_list() of
    [] ->
        "empty list";
    Other ->
        Other
end.'

	assert_lx_generates_erlang(lx_code, expected_erlang)
}

fn test_simple_match_mixed_with_rescue() {
	lx_code := 'def test_mixed() do
match {:ok, user} <- get_user()
match {:ok, data} <- get_data(user) rescue error do
  {:error, error}
end
data
end'

	expected_erlang := '-module(main).
-export([test_mixed/0]).

-spec test_mixed() -> data().
test_mixed() ->
case get_user() of
    {ok, User} ->
        case get_data(User) of
            {ok, Data} ->
                Data;
            Error ->
                {error, Error}
        end;
    Other ->
        Other
end.'

	assert_lx_generates_erlang(lx_code, expected_erlang)
}
