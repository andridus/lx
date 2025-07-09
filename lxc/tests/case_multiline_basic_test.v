module main

fn test_case_multiline_basic() {
	lx_code := '
def simple_case() do
  case 1 do
    1 ->
      x = 10
      20
    2 -> 0
    _ -> -1
  end
end'
	expected := '-module(main).
-export([simple_case/0]).

-spec simple_case() -> integer().
simple_case() ->
case 1 of
    1 -> X_aaaa = 10,
    20;
    2 -> 0;
    _ -> -1
end.

'
	assert generates_erlang(lx_code) == expected
}

fn test_case_multiline_two_statements() {
	lx_code := '
def two_statements() do
  case 1 do
    1 ->
      a = 100
      b = 200
      300
    _ -> 999
  end
end'
	expected := '-module(main).
-export([two_statements/0]).

-spec two_statements() -> integer().
two_statements() ->
case 1 of
    1 -> A_aaaa = 100,
    B_baaa = 200,
    300;
    _ -> 999
end.

'
	assert generates_erlang(lx_code) == expected
}

fn test_case_single_line_still_works() {
	lx_code := '
def single_line() do
  case 1 do
    1 -> :one
    2 -> :two
    _ -> :other
  end
end'
	expected := '-module(main).
-export([single_line/0]).

-spec single_line() -> atom().
single_line() ->
case 1 of
    1 -> one;
    2 -> two;
    _ -> other
end.

'
	assert generates_erlang(lx_code) == expected
}
