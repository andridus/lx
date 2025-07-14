module main

fn test_case_type_inference_debug() {
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
	expected := '-module(test).
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

fn test_case_type_inference_simple() {
	lx_code := '
def simple_case() do
  case 1 do
    1 -> 20
    2 -> 0
    _ -> -1
  end
end'
	expected := '-module(test).
-export([simple_case/0]).

-spec simple_case() -> integer().
simple_case() ->
case 1 of
    1 -> 20;
    2 -> 0;
    _ -> -1
end.

'
	assert generates_erlang(lx_code) == expected
}

fn test_case_type_inference_with_block() {
	lx_code := '
def case_with_block() do
  case 1 do
    1 ->
      x = 10
      y = 20
      x + y
    2 -> 0
    _ -> -1
  end
end'
	expected := '-module(test).
-export([case_with_block/0]).

-spec case_with_block() -> integer().
case_with_block() ->
case 1 of
    1 -> X_aaaa = 10,
    Y_baaa = 20,
    X_aaaa + Y_baaa;
    2 -> 0;
    _ -> -1
end.

'
	assert generates_erlang(lx_code) == expected
}
