module main

fn test_case_expression_simple() {
	lx_code := '
def simple_case() do
  case 1 do
    1 -> "one"
    2 -> "two"
    _ -> "other"
  end
end'
	expected := '-module(main).
-export([simple_case/0]).

-spec simple_case() -> string().
simple_case() ->
case 1 of
    1 -> "one";
    2 -> "two";
    _ -> "other"
end.

'
	erl1 := generates_erlang(lx_code)
	assert erl1.success
	assert erl1.code == expected
}

fn test_case_expression_with_variable() {
	lx_code := '
def case_with_var(x) do
  case x do
    1 -> "um"
    2 -> "dois"
    3 -> "três"
    _ -> "qualquer"
  end
end'
	expected := '-module(main).
-export([case_with_var/1]).

-spec case_with_var(any()) -> string().
case_with_var(X) ->
case X of
    1 -> "um";
    2 -> "dois";
    3 -> "três";
    _ -> "qualquer"
end.

'
	erl1 := generates_erlang(lx_code)
	assert erl1.success
	assert erl1.code == expected
}

fn test_case_expression_with_guards() {
	lx_code := '
def case_with_guards(x) do
  case x do
    x when x > 3 -> "maior que 3"
    2 -> "dois"
    3 -> "três"
    _ -> "qualquer"
  end
end'
	expected := '-module(main).
-export([case_with_guards/1]).

-spec case_with_guards(any()) -> string().
case_with_guards(X) ->
case X of
    X when X > 3 -> "maior que 3";
    2 -> "dois";
    3 -> "três";
    _ -> "qualquer"
end.

'
	erl1 := generates_erlang(lx_code)
	assert erl1.success
	assert erl1.code == expected
}

fn test_case_expression_integer_type() {
	lx_code := '
def integer_case(x) do
  case x do
    1 -> 10
    2 -> 20
    _ -> 0
  end
end'
	expected := '-module(main).
-export([integer_case/1]).

-spec integer_case(any()) -> integer().
integer_case(X) ->
case X of
    1 -> 10;
    2 -> 20;
    _ -> 0
end.

'
	erl1 := generates_erlang(lx_code)
	assert erl1.success
	assert erl1.code == expected
}

fn test_case_expression_boolean_type() {
	lx_code := '
def boolean_case(x) do
  case x do
    1 -> true
    2 -> false
    _ -> true
  end
end'
	expected := '-module(main).
-export([boolean_case/1]).

-spec boolean_case(any()) -> boolean().
boolean_case(X) ->
case X of
    1 -> true;
    2 -> false;
    _ -> true
end.

'
	erl1 := generates_erlang(lx_code)
	assert erl1.success
	assert erl1.code == expected
}
