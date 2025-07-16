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
	expected := '-module(test).
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
	code, hrl_content := generates_erlang(lx_code)
	assert code == expected
	assert hrl_content == ''
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
	expected := '-module(test).
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
	code, hrl_content := generates_erlang(lx_code)
	assert code == expected
	assert hrl_content == ''
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
	expected := '-module(test).
-export([get_value/0, single_with/0]).

-spec get_value() -> {integer(), binary()}.
get_value() ->
{1, <<"hello"/utf8>>}.

-spec single_with() -> binary().
single_with() ->
case get_value() of
    {1, Value} ->
        Value;
    Other ->
        <<"failed"/utf8>>
end.

'
	code, hrl_content := generates_erlang(lx_code)
	assert code == expected
	assert hrl_content == ''
}

fn test_with_expression_no_bindings() {
	lx_code := '
def no_bindings() do
  with do
    "success"
  end
end'
	expected := '-module(test).
-export([no_bindings/0]).

-spec no_bindings() -> binary().
no_bindings() ->
<<"success"/utf8>>.

'
	code, hrl_content := generates_erlang(lx_code)
	assert code == expected
	assert hrl_content == ''
}

fn test_with_expression_with_guards() {
	lx_code := '
record User {
  id :: integer,
  active :: boolean
}

def check_user_status(user) do
	with User{id: id, active: true} when id > 0 <- user do
		"active_user"
	else
		"inactive_user"
	end
end'
	expected := '-module(test).
-include("test.hrl").

-export([check_user_status/1]).


-spec check_user_status(any()) -> binary().
check_user_status(User) ->
case User of
    #user{id = Id, active = true} when Id > 0 ->
        <<"active_user"/utf8>>;
    Other ->
        <<"inactive_user"/utf8>>
end.

'
	code, hrl_content := generates_erlang(lx_code)
	assert code == expected
	assert hrl_content == '%% Generated header file for test module
%% Contains records and type definitions

-record(user, {id, active}).
'
}
