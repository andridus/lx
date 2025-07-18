module main

fn test_list_cons_pattern() {
	lx_code := '
def test_cons() do
  match [h | t] <- [1, 2, 3] rescue error do
    {:empty_list, error}
  end
  {h, t}
end'
	expected := '-module(test).
-export([test_cons/0]).

-spec test_cons() -> {atom(), [integer()]} | {integer(), [integer()]}.
test_cons() ->
case [1, 2, 3] of
    [H | T] ->
        {H, T};
    Error ->
        {empty_list, Error}
end.

'
	code, hrl_content := generates_erlang(lx_code)
	assert code == expected
	assert hrl_content == ''
}

fn test_list_cons_success() {
	lx_code := '
def test_success() do
  match [first | rest] <- [42, 10, 5] rescue error do
    {:error, "empty list"}
  end
  {first, rest}
end'
	expected := '-module(test).
-export([test_success/0]).

-spec test_success() -> {atom(), binary()} | {integer(), [integer()]}.
test_success() ->
case [42, 10, 5] of
    [First | Rest] ->
        {First, Rest};
    Error ->
        {error, <<"empty list"/utf8>>}
end.

'
	code, hrl_content := generates_erlang(lx_code)
	assert code == expected
	assert hrl_content == ''
}
