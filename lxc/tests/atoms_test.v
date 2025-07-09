module main

fn test_simple_atom() {
	lx_code := '
def simple() do
  :ok
end'
	expected := '-module(main).
-export([simple/0]).

-spec simple() -> atom().
simple() ->
ok.

'
	assert generates_erlang(lx_code) == expected
}

fn test_multiple_atoms() {
	lx_code := '
def get_status() do
  :success
end

def get_error() do
  :error
end

def get_pending() do
  :pending
end'
	expected := '-module(main).
-export([get_status/0, get_error/0, get_pending/0]).

-spec get_status() -> atom().
get_status() ->
success.

-spec get_error() -> atom().
get_error() ->
error.

-spec get_pending() -> atom().
get_pending() ->
pending.

'
	assert generates_erlang(lx_code) == expected
}

fn test_tuple_with_atoms() {
	lx_code := '
def result_ok() do
  {:ok, "success"}
end

def result_error() do
  {:error, "failed"}
end'
	expected := '-module(main).
-export([result_ok/0, result_error/0]).

-spec result_ok() -> {atom(), string()}.
result_ok() ->
{ok, "success"}.

-spec result_error() -> {atom(), string()}.
result_error() ->
{error, "failed"}.

'
	assert generates_erlang(lx_code) == expected
}

fn test_complex_tuple_with_atoms() {
	lx_code := '
def complex_result() do
  {:ok, :data, 42, "message"}
end

def simple_response() do
  {:response, :success}
end'
	expected := '-module(main).
-export([complex_result/0, simple_response/0]).

-spec complex_result() -> {atom(), atom(), integer(), string()}.
complex_result() ->
{ok, data, 42, "message"}.

-spec simple_response() -> {atom(), atom()}.
simple_response() ->
{response, success}.

'
	assert generates_erlang(lx_code) == expected
}

fn test_external_function_call() {
	lx_code := '
def print_hello() do
  :io.format("Hello")
end

def print_with_args() do
  :io.format("Hello ~s", ["World"])
end'
	expected := '-module(main).
-export([print_hello/0, print_with_args/0]).

-spec print_hello() -> any().
print_hello() ->
io:format("Hello").

-spec print_with_args() -> any().
print_with_args() ->
io:format("Hello ~s", ["World"]).

'
	assert generates_erlang(lx_code) == expected
}

fn test_multiple_external_calls() {
	lx_code := '
def debug_info() do
  :erlang.process_info()
end

def get_node() do
  :erlang.node()
end'
	expected := '-module(main).
-export([debug_info/0, get_node/0]).

-spec debug_info() -> any().
debug_info() ->
erlang:process_info().

-spec get_node() -> any().
get_node() ->
erlang:node().

'
	assert generates_erlang(lx_code) == expected
}

fn test_atoms_with_case_expression() {
	lx_code := '
def handle_status(status) do
  case status do
    :ok -> "Success"
    :error -> "Failed"
    :pending -> "Waiting"
    _ -> "Unknown"
  end
end'
	expected := '-module(main).
-export([handle_status/1]).

-spec handle_status(any()) -> string().
handle_status(Status) ->
case Status of
    ok -> "Success";
    error -> "Failed";
    pending -> "Waiting";
    _ -> "Unknown"
end.

'
	assert generates_erlang(lx_code) == expected
}

fn test_tuple_destructuring_with_atoms() {
	lx_code := '
def extract_ok_value(result) do
  case result do
    {:ok, value} -> value
    {:error, _} -> nil
  end
end

def check_simple_response(response) do
  case response do
    {:success, data} -> data
    {:failure, _} -> "Failed"
    _ -> "Unknown response"
  end
end'
	expected := '-module(main).
-export([extract_ok_value/1, check_simple_response/1]).

-spec extract_ok_value(any()) -> any().
extract_ok_value(Result) ->
case Result of
    {ok, Value} -> Value;
    {error, _} -> nil
end.

-spec check_simple_response(any()) -> string().
check_simple_response(Response) ->
case Response of
    {success, Data} -> Data;
    {failure, _} -> "Failed";
    _ -> "Unknown response"
end.

'
	assert generates_erlang(lx_code) == expected
}
