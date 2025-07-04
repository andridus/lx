module main

fn test_match_rescue_simple_success() {
	lx_code := '
def test_simple() do
  match {:ok, value} <- {:ok, 42} rescue error do
    {:error, error}
  end
  value
end'
	expected := '-module(main).
-export([test_simple/0]).

-spec test_simple() -> value().
test_simple() ->
case {ok, 42} of
    {ok, Value} ->
        Value;
    Error ->
        {error, Error}
end.

'
	erl1 := generates_erlang(lx_code)
	assert erl1.success
	assert erl1.code == expected
}

fn test_match_rescue_simple_failure() {
	lx_code := '
def test_failure() do
  match {:ok, value} <- {:error, "not_found"} rescue error do
    {:rescued, error}
  end
  :completed
end'
	expected := '-module(main).
-export([test_failure/0]).

-spec test_failure() -> atom().
test_failure() ->
case {error, "not_found"} of
    {ok, Value} ->
        completed;
    Error ->
        {rescued, Error}
end.

'
	erl1 := generates_erlang(lx_code)
	assert erl1.success
	assert erl1.code == expected
}

fn test_match_rescue_complex_pattern() {
	lx_code := '
def test_complex() do
  match {:ok, name, age} <- {:ok, "alice", 25} rescue error do
    {:error, "failed", error}
  end
  {name, age}
end'
	expected := '-module(main).
-export([test_complex/0]).

-spec test_complex() -> {name(), age()}.
test_complex() ->
case {ok, "alice", 25} of
    {ok, Name, Age} ->
        {Name, Age};
    Error ->
        {error, "failed", Error}
end.

'
	erl1 := generates_erlang(lx_code)
	assert erl1.success
	assert erl1.code == expected
}

fn test_match_rescue_sequential() {
	lx_code := '
def test_sequential() do
  match {:ok, user} <- {:ok, "alice"} rescue result do
    {:user_error, result}
  end
  match {:ok, perms} <- {:ok, "admin"} rescue result do
    {:perm_error, result}
  end
  {user, perms}
end'
	expected := '-module(main).
-export([test_sequential/0]).

-spec test_sequential() -> {user(), perms()}.
test_sequential() ->
case {ok, "alice"} of
    {ok, User} ->
        case {ok, "admin"} of
            {ok, Perms} ->
                {User, Perms};
            Result ->
                {perm_error, Result}
        end;
    Result ->
        {user_error, Result}
end.

'
	erl1 := generates_erlang(lx_code)
	assert erl1.success
	assert erl1.code == expected
}

fn test_match_rescue_with_function_call() {
	lx_code := '
def get_data() do
  {:ok, "data"}
end

def test_with_call() do
  match {:ok, data} <- get_data() rescue error do
    {:error, "Failed", error}
  end
  data
end'
	expected := '-module(main).
-export([get_data/0, test_with_call/0]).

-spec get_data() -> {atom(), string()}.
get_data() ->
{ok, "data"}.

-spec test_with_call() -> data().
test_with_call() ->
case get_data() of
    {ok, Data} ->
        Data;
    Error ->
        {error, "Failed", Error}
end.

'
	erl1 := generates_erlang(lx_code)
	assert erl1.success
	assert erl1.code == expected
}

fn test_match_rescue_simple_list() {
	lx_code := '
def test_list() do
  match [x] <- [42] rescue error do
    {:empty_list, error}
  end
  x
end'
	expected := '-module(main).
-export([test_list/0]).

-spec test_list() -> x().
test_list() ->
case [42] of
    [X] ->
        X;
    Error ->
        {empty_list, Error}
end.

'
	erl1 := generates_erlang(lx_code)
	assert erl1.success
	assert erl1.code == expected
}

fn test_match_rescue_list_cons() {
	lx_code := '
def test_cons() do
  match [h | t] <- [1, 2, 3] rescue error do
    {:empty_list, error}
  end
  {h, t}
end'
	expected := '-module(main).
-export([test_cons/0]).

-spec test_cons() -> {h(), t()}.
test_cons() ->
case [1, 2, 3] of
    [H | T] ->
        {H, T};
    Error ->
        {empty_list, Error}
end.

'
	erl1 := generates_erlang(lx_code)
	assert erl1.success
	assert erl1.code == expected
}
