module main

fn test_case_expression_simple() {
	lx_code := '
def simple_case() do
  case 1 do
    1 ->
      x = 10
      y = 20
      x + y
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
    Y_baaa = 20,
    X_aaaa + Y_baaa;
    2 -> 0;
    _ -> -1
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

fn test_case_expression_with_guards_simple() {
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

fn test_case_expression_atom_patterns() {
	lx_code := '
def atom_case() do
  case :ok do
    :ok ->
      success_msg = "Operation successful"
      count = 1
      {success_msg, count}
    :error ->
      error_msg = "Operation failed"
      error_code = -1
      {error_msg, error_code}
    _ ->
      unknown_msg = "Unknown status"
      unknown_code = 0
      {unknown_msg, unknown_code}
  end
end'
	expected := '-module(main).
-export([atom_case/0]).

-spec atom_case() -> {any(), any()}.
atom_case() ->
case ok of
    ok -> Success_msg_aaaa = "Operation successful",
    Count_baaa = 1,
    {Success_msg_aaaa, Count_baaa};
    error -> Error_msg_caaa = "Operation failed",
    Error_code_daaa = -1,
    {Error_msg_caaa, Error_code_daaa};
    _ -> Unknown_msg_eaaa = "Unknown status",
    Unknown_code_faaa = 0,
    {Unknown_msg_eaaa, Unknown_code_faaa}
end.

'
	erl1 := generates_erlang(lx_code)
	assert erl1.success
	assert erl1.code == expected
}

fn test_case_expression_single_statement() {
	lx_code := '
def single_statement_case() do
  case 1 do
    1 -> :ok
    2 -> :error
    _ -> :unknown
  end
end'
	expected := '-module(main).
-export([single_statement_case/0]).

-spec single_statement_case() -> atom().
single_statement_case() ->
case 1 of
    1 -> ok;
    2 -> error;
    _ -> unknown
end.

'
	erl1 := generates_erlang(lx_code)
	assert erl1.success
	assert erl1.code == expected
}
