module main

fn test_list_cons_pattern() {
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

fn test_list_cons_success() {
	lx_code := '
def test_success() do
  match [first | rest] <- [42, 10, 5] rescue error do
    {:error, "empty list"}
  end
  {first, rest}
end'
	expected := '-module(main).
-export([test_success/0]).

-spec test_success() -> {first(), rest()}.
test_success() ->
case [42, 10, 5] of
    [First | Rest] ->
        {First, Rest};
    Error ->
        {error, "empty list"}
end.

'
	erl1 := generates_erlang(lx_code)
	assert erl1.success
	assert erl1.code == expected
}
