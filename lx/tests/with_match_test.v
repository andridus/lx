module main

fn test_with_expression_simple() {
	lx_code := '
def a1() do
  with :ok <- :ok do
    :ok
  end
end'
	expected := '-module(test).
-export([a1/0]).

-spec a1() -> atom().
a1() ->
case ok of
    ok ->
        ok;
    Other ->
        Other
end.

'
	code, hrl_content := generates_erlang(lx_code)
	assert code == expected
	assert hrl_content == ''
}

fn test_simple_match_expression() {
	lx_code := '
def a2() do
  match :ok <- :ok
    :ok
end'
	expected := '-module(test).
-export([a2/0]).

-spec a2() -> atom().
a2() ->
case ok of
    ok ->
        ok;
    Other ->
        Other
end.

'
	code, hrl_content := generates_erlang(lx_code)
	assert code == expected
	assert hrl_content == ''
}

fn test_match_rescue_expression() {
	lx_code := '
def a3() do
  match :ok <- :error rescue err do
    :error_handled
  end
end'
	expected := '-module(test).
-export([a3/0]).

-spec a3() -> atom().
a3() ->
case error of
    ok ->
        ok;
    Err ->
        error_handled
end.

'
	code, hrl_content := generates_erlang(lx_code)
	assert code == expected
	assert hrl_content == ''
}
