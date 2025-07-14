module main

fn test_simple_external_function_call() {
	lx_code := '
def print_hello() do
  :io.format("Hello")
end'
	expected := '-module(test).
-export([print_hello/0]).

-spec print_hello() -> any().
print_hello() ->
io:format(<<"Hello"/utf8>>).

'
	assert generates_erlang(lx_code) == expected
}

fn test_external_function_call_with_multiple_arguments() {
	lx_code := '
def print_formatted() do
  :io.format("~p", [1, 2, 3])
end'
	expected := '-module(test).
-export([print_formatted/0]).

-spec print_formatted() -> any().
print_formatted() ->
io:format(<<"~p"/utf8>>, [1, 2, 3]).

'
	assert generates_erlang(lx_code) == expected
}

fn test_multiple_external_function_calls() {
	lx_code := '
def debug_info() do
  :erlang.process_info()
end

def get_node() do
  :erlang.node()
end

def get_pid() do
  :erlang.self()
end'
	expected := '-module(test).
-export([debug_info/0, get_node/0, get_pid/0]).

-spec debug_info() -> any().
debug_info() ->
erlang:process_info().

-spec get_node() -> any().
get_node() ->
erlang:node().

-spec get_pid() -> any().
get_pid() ->
erlang:self().

'
	assert generates_erlang(lx_code) == expected
}

fn test_external_call_with_variables() {
	lx_code := '
def print_message(message) do
  :io.format("Message: ~s", [message])
end

def print_number(num) do
  :io.format("Number: ~p", [num])
end'
	expected := '-module(test).
-export([print_message/1, print_number/1]).

-spec print_message(any()) -> any().
print_message(Message) ->
io:format(<<"Message: ~s"/utf8>>, [Message]).

-spec print_number(any()) -> any().
print_number(Num) ->
io:format(<<"Number: ~p"/utf8>>, [Num]).

'
	assert generates_erlang(lx_code) == expected
}

fn test_external_call_with_complex_arguments() {
	lx_code := '
def print_list() do
  :io.format("List: ~p", [[1, 2, 3, 4, 5]])
end

def print_map() do
  :io.format("Map: ~p", [%{name: "John", age: 30}])
end'
	expected := '-module(test).
-export([print_list/0, print_map/0]).

-spec print_list() -> any().
print_list() ->
io:format(<<"List: ~p"/utf8>>, [[1, 2, 3, 4, 5]]).

-spec print_map() -> any().
print_map() ->
io:format(<<"Map: ~p"/utf8>>, [#{name => <<"John"/utf8>>, age => 30}]).

'
	assert generates_erlang(lx_code) == expected
}

fn test_external_call_in_case_expression() {
	lx_code := '
def handle_command(command) do
  case command do
    :print -> :io.format("Printing...")
    :debug -> :io.format("Debug info: ~p", [:erlang.process_info()])
    :status -> :io.format("Status: ~s", ["OK"])
    _ -> :io.format("Unknown command")
  end
end'
	expected := '-module(test).
-export([handle_command/1]).

-spec handle_command(any()) -> any().
handle_command(Command) ->
case Command of
    print -> io:format(<<"Printing..."/utf8>>);
    debug -> io:format(<<"Debug info: ~p"/utf8>>, [erlang:process_info()]);
    status -> io:format(<<"Status: ~s"/utf8>>, [<<"OK"/utf8>>]);
    _ -> io:format(<<"Unknown command"/utf8>>)
end.

'
	assert generates_erlang(lx_code) == expected
}

fn test_external_call_with_nested_expressions() {
	lx_code := '
def complex_print() do
  :io.format("Result: ~p", [:erlang.node(), :erlang.self()])
end

def debug_with_info() do
  :io.format("Debug: ~p", [:erlang.process_info(:erlang.self())])
end'
	expected := '-module(test).
-export([complex_print/0, debug_with_info/0]).

-spec complex_print() -> any().
complex_print() ->
io:format(<<"Result: ~p"/utf8>>, [erlang:node(), erlang:self()]).

-spec debug_with_info() -> any().
debug_with_info() ->
io:format(<<"Debug: ~p"/utf8>>, [erlang:process_info(erlang:self())]).

'
	assert generates_erlang(lx_code) == expected
}

// fn test_record_access_vs_external_call() {
// 	lx_code := '
// def get_person_name(person) do
//   person.name
// end

// def print_person_info(person) do
//   :io.format("Name: ~s, Age: ~p", [person.name, person.age])
// end'
// 	expected := '-module(test).
// -export([get_person_name/1, print_person_info/1]).

// -spec get_person_name(any()) -> any().
// get_person_name(Person) ->
// Person#record.name.

// -spec print_person_info(any()) -> any().
// print_person_info(Person) ->
// io:format(<<"Name: ~s, Age: ~p"/utf8>>, [Person#record.name, Person#record.age]).

// '
// 	assert generates_erlang(lx_code) == expected
// }
