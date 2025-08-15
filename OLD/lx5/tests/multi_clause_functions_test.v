module main

fn test_simple_multi_clause_function() {
	lx_code := '
def f do
  (x) -> 1
  (y) -> 2
end'
	expected := '-module(test).
-export([f/1]).

-spec f(any()) -> integer().
f(X) ->
1;
f(Y) ->
2.

'
	code, hrl_content := generates_erlang(lx_code)
	assert code == expected
	assert hrl_content == ''
}

fn test_multi_clause_function_with_different_arities() {
	lx_code := '
def process do
  () -> {:ok, nil}
  (x) -> {:ok, x}
  (x, y) -> {:ok, x, y}
end'
	expected := '-module(test).
-export([process/0, process/1, process/2]).

-spec process() -> {atom(), nil}.
process() ->
{ok, nil}.
-spec process(any()) -> {atom(), any()}.
process(X) ->
{ok, X}.
-spec process(any(), any()) -> {atom(), any(), any()}.
process(X, Y) ->
{ok, X, Y}.

'
	code, hrl_content := generates_erlang(lx_code)
	assert code == expected
	assert hrl_content == ''
}

fn test_multi_clause_function_with_blocks() {
	lx_code := '
def calculate do
  (x) ->
    x * 2
  (x, y) ->
    result = x + y
    result * 2
end'
	expected := '-module(test).
-export([calculate/1, calculate/2]).

-spec calculate(any()) -> integer().
calculate(X) ->
X * 2.
-spec calculate(any(), any()) -> integer().
calculate(X, Y) ->
Result_aaaa = X + Y,
Result_aaaa * 2.

'
	code, hrl_content := generates_erlang(lx_code)
	assert code == expected
	assert hrl_content == ''
}

fn test_multi_clause_function_with_tuples() {
	lx_code := '
def f do
  () -> {:ok, nil}
  (x) -> {:ok, x}
end'
	expected := '-module(test).
-export([f/0, f/1]).

-spec f() -> {atom(), nil}.
f() ->
{ok, nil}.
-spec f(any()) -> {atom(), any()}.
f(X) ->
{ok, X}.

'
	code, hrl_content := generates_erlang(lx_code)
	assert code == expected
	assert hrl_content == ''
}

fn test_multi_clause_function_with_atoms() {
	lx_code := '
def handle_status do
  (:ok) -> "Success"
  (:error) -> "Failed"
  (:pending) -> "Waiting"
  (_) -> "Unknown"
end'
	expected := '-module(test).
-export([handle_status/1]).

-spec handle_status(any()) -> binary().
handle_status(ok) ->
<<"Success"/utf8>>;
handle_status(error) ->
<<"Failed"/utf8>>;
handle_status(pending) ->
<<"Waiting"/utf8>>;
handle_status(_) ->
<<"Unknown"/utf8>>.

'
	code, hrl_content := generates_erlang(lx_code)
	assert code == expected
	assert hrl_content == ''
}

fn test_multi_clause_function_with_pattern_matching() {
	lx_code := '
def extract_value do
  ({:ok, value}) -> value
  ({:error, _}) -> nil
  (_) -> "Unknown"
end'
	expected := '-module(test).
-export([extract_value/1]).

-spec extract_value(any()) -> any().
extract_value({ok, Value}) ->
Value;
extract_value({error, _}) ->
nil;
extract_value(_) ->
<<"Unknown"/utf8>>.

'
	code, hrl_content := generates_erlang(lx_code)
	assert code == expected
	assert hrl_content == ''
}

fn test_multi_clause_function_with_guards() {
	lx_code := '
def process_number do
  (x) when x > 0 -> "Positive"
  (x) when x < 0 -> "Negative"
  (0) -> "Zero"
end'
	expected := '-module(test).
-export([process_number/1]).

-spec process_number(any()) -> binary().
process_number(X) when X > 0 ->
<<"Positive"/utf8>>;
process_number(X) when X < 0 ->
<<"Negative"/utf8>>;
process_number(0) ->
<<"Zero"/utf8>>.

'
	code, hrl_content := generates_erlang(lx_code)
	assert code == expected
	assert hrl_content == ''
}

fn test_multi_clause_function_with_external_calls() {
	lx_code := '
def debug_info do
  (:process) -> :erlang.process_info()
  (:node) -> :erlang.node()
  (:self) -> :erlang.self()
  (_) -> :io.format("Unknown debug type")
end'
	expected := '-module(test).
-export([debug_info/1]).

-spec debug_info(any()) -> any().
debug_info(process) ->
erlang:process_info();
debug_info(node) ->
erlang:node();
debug_info(self) ->
erlang:self();
debug_info(_) ->
io:format(<<"Unknown debug type"/utf8>>).

'
	code, hrl_content := generates_erlang(lx_code)
	assert code == expected
	assert hrl_content == ''
}

fn test_multi_clause_function_with_complex_patterns() {
	lx_code := '
def handle_message do
  ({:text, message}) -> :io.format("Text: ~s", [message])
  ({:number, num}) -> :io.format("Number: ~p", [num])
  ({:list, items}) -> :io.format("List: ~p", [items])
  (_) -> :io.format("Unknown message type")
end'
	expected := '-module(test).
-export([handle_message/1]).

-spec handle_message(any()) -> any().
handle_message({text, Message}) ->
io:format(<<"Text: ~s"/utf8>>, [Message]);
handle_message({number, Num}) ->
io:format(<<"Number: ~p"/utf8>>, [Num]);
handle_message({list, Items}) ->
io:format(<<"List: ~p"/utf8>>, [Items]);
handle_message(_) ->
io:format(<<"Unknown message type"/utf8>>).

'
	code, hrl_content := generates_erlang(lx_code)
	assert code == expected
	assert hrl_content == ''
}
