module main

fn test_with_expression_simple() {
	lx_code := '
def func_a() do
  {1, 1}
end

def func_b() do
  {1, 2}
end

def simple() do
  with {1, 1} <- func_a(),
       {1, 2} <- func_b() do
    1
  end
end'
	expected := '-module(main).
-export([func_a/0, func_b/0, simple/0]).

-spec func_a() -> {integer(), integer()}.
func_a() ->
{1, 1}.

-spec func_b() -> {integer(), integer()}.
func_b() ->
{1, 2}.

-spec simple() -> integer().
simple() ->
case func_a() of
    {1, 1} ->
        case func_b() of
            {1, 2} ->
                1;
            Other ->
                Other
        end;
    Other ->
        Other
end.

'
	erl1 := generates_erlang(lx_code)
	assert erl1.success
	assert erl1.code == expected
}

fn test_with_expression_with_else() {
	lx_code := '
def func_a() do
  {1, 1}
end

def func_b() do
  {1, 2}
end

def simple_2() do
  with {1, 1} <- func_a(),
       {1, 2} <- func_b() do
    1
  else
    0
  end
end'
	expected := '-module(main).
-export([func_a/0, func_b/0, simple_2/0]).

-spec func_a() -> {integer(), integer()}.
func_a() ->
{1, 1}.

-spec func_b() -> {integer(), integer()}.
func_b() ->
{1, 2}.

-spec simple_2() -> integer().
simple_2() ->
case func_a() of
    {1, 1} ->
        case func_b() of
            {1, 2} ->
                1;
            Other ->
                0
        end;
    Other ->
        0
end.

'
	erl1 := generates_erlang(lx_code)
	assert erl1.success
	assert erl1.code == expected
}

fn test_with_expression_single_binding() {
	lx_code := '
def get_value() do
  {1, "hello"}
end

def single_with() do
  with {1, value} <- get_value() do
    value
  else
    "failed"
  end
end'
	expected := '-module(main).
-export([get_value/0, single_with/0]).

-spec get_value() -> {integer(), string()}.
get_value() ->
{1, "hello"}.

-spec single_with() -> string().
single_with() ->
case get_value() of
    {1, Value} ->
        Value;
    Other ->
        "failed"
end.

'
	erl1 := generates_erlang(lx_code)
	assert erl1.success
	assert erl1.code == expected
}

fn test_with_expression_no_bindings() {
	lx_code := '
def no_bindings() do
  with do
    "success"
  end
end'
	expected := '-module(main).
-export([no_bindings/0]).

-spec no_bindings() -> string().
no_bindings() ->
"success".

'
	erl1 := generates_erlang(lx_code)
	assert erl1.success
	assert erl1.code == expected
}
