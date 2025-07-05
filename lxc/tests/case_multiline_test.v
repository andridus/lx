module main

fn test_case_multiline_simple() {
	lx_code := '
def simple_case() do
  case 1 do
    1 ->
      x = 10
      y = 20
      x + y
    2 -> 0
    _ -> 999
  end
end'
	expected := '-module(main).
-export([simple_case/0]).

-spec simple_case() -> integer().
simple_case() ->
case 1 of
    1 -> X_aaaa = 10,
    Y_baaa = 20,
    X_aaaa + Y_baaa;
    2 -> 0;
    _ -> 999
end.

'
	erl1 := generates_erlang(lx_code)
	assert erl1.success
	assert erl1.code == expected
}

fn test_case_multiline_tuple() {
	lx_code := '
def tuple_case() do
  case {1, 2} do
    {1, 2} ->
      a = 10
      b = 20
      {a, b}
    _ -> {0, 0}
  end
end'
	expected := '-module(main).
-export([tuple_case/0]).

-spec tuple_case() -> {a(), b()}.
tuple_case() ->
case {1, 2} of
    {1, 2} -> A_aaaa = 10,
    B_baaa = 20,
    {A_aaaa, B_baaa};
    _ -> {0, 0}
end.

'
	erl1 := generates_erlang(lx_code)
	assert erl1.success
	assert erl1.code == expected
}