-module(lx0_compiler_test).
-include_lib("eunit/include/eunit.hrl").

%% Test basic compilation
basic_compilation_test() ->
    Source = "def hello() do\n    \"Hello, World!\"\nend",
    {ok, ErlangCode} = lx0_compiler:compile(Source),
    ?assert(is_list(ErlangCode)),
    ?assert(string:find(ErlangCode, "-module") =/= nomatch),
    ?assert(string:find(ErlangCode, "hello()") =/= nomatch).

%% Test record definition
record_definition_test() ->
    Source = "record User {\n    name :: string,\n    age :: integer\n}\n\ndef create_user(name, age) do\n    User{name: name, age: age}\nend",
    {ok, ErlangCode} = lx0_compiler:compile(Source),
    ?assert(string:find(ErlangCode, "-record(user") =/= nomatch),
    ?assert(string:find(ErlangCode, "create_user") =/= nomatch).

%% Test function with type annotations
function_with_types_test() ->
    Source = "def add(a :: integer, b :: integer) :: integer do\n    a + b\nend",
    {ok, ErlangCode} = lx0_compiler:compile(Source),
    ?assert(string:find(ErlangCode, "-spec add") =/= nomatch),
    ?assert(string:find(ErlangCode, "integer") =/= nomatch).

%% Test case expression
case_expression_test() ->
    Source = "def classify_number(n) do\n    case n do\n        0 -> :zero\n        n when n > 0 -> :positive\n        _ -> :negative\n    end\nend",
    {ok, ErlangCode} = lx0_compiler:compile(Source),
    ?assert(string:find(ErlangCode, "case") =/= nomatch),
    ?assert(string:find(ErlangCode, "when") =/= nomatch).

%% Test list operations
list_operations_test() ->
    Source = "def create_list do\n    [1, 2, 3, 4, 5]\nend",
    {ok, ErlangCode} = lx0_compiler:compile(Source),
    ?assert(string:find(ErlangCode, "[1, 2, 3, 4, 5]") =/= nomatch).

%% Test tuple operations
tuple_operations_test() ->
    Source = "def create_tuple do\n    {:ok, \"success\"}\nend",
    {ok, ErlangCode} = lx0_compiler:compile(Source),
    ?assert(string:find(ErlangCode, "{ok, \"success\"}") =/= nomatch).

%% Test map operations
map_operations_test() ->
    Source = "def create_map do\n    %{name: \"John\", age: 30}\nend",
    {ok, ErlangCode} = lx0_compiler:compile(Source),
    ?assert(string:find(ErlangCode, "#{") =/= nomatch).

%% Test binary operations
binary_operations_test() ->
    Source = "def create_binary do\n    <<1, 2, 3, 4>>\nend",
    {ok, ErlangCode} = lx0_compiler:compile(Source),
    ?assert(string:find(ErlangCode, "<<1, 2, 3, 4>>") =/= nomatch).

%% Test if expression
if_expression_test() ->
    Source = "def check_number(n) do\n    if n > 0 do\n        :positive\n    else\n        :non_positive\n    end\nend",
    {ok, ErlangCode} = lx0_compiler:compile(Source),
    ?assert(string:find(ErlangCode, "if") =/= nomatch).

%% Test block expression
block_expression_test() ->
    Source = "def calculate do\n    do\n        a = 10\n        b = 20\n        a + b\n    end\nend",
    {ok, ErlangCode} = lx0_compiler:compile(Source),
    ?assert(string:find(ErlangCode, "a = 10") =/= nomatch),
    ?assert(string:find(ErlangCode, "b = 20") =/= nomatch).

%% Test type definitions
type_definitions_test() ->
    Source = "type status :: :ok | :error\n\ndef get_status do\n    :ok\nend",
    {ok, ErlangCode} = lx0_compiler:compile(Source),
    ?assert(string:find(ErlangCode, "-type status") =/= nomatch).

%% Test error handling for invalid syntax
invalid_syntax_test() ->
    Source = "def invalid_function do\n    this is not valid syntax\nend",
    {error, _} = lx0_compiler:compile(Source).

%% Test file compilation
file_compilation_test() ->
    Filename = "test_file.lx",
    Source = "def test_function do\n    42\nend",
    ok = file:write_file(Filename, Source),
    try
        {ok, ErlangCode} = lx0_compiler:compile_file(Filename),
        ?assert(string:find(ErlangCode, "test_function") =/= nomatch)
    after
        file:delete(Filename)
    end.

%% Test parsing without code generation
parse_only_test() ->
    Source = "def parse_test do\n    :ok\nend",
    {ok, AST} = lx0_compiler:parse(Source),
    ?assert(is_list(AST)),
    ?assert(length(AST) > 0).

%% Test complex example
complex_example_test() ->
    Source = "
record Person {
    name :: string,
    age :: integer
}

def create_person(name, age) do
    Person{name: name, age: age}
end

def process_person(person) do
    case person do
        Person{name: name, age: age} when age >= 18 ->
            \"Adult: #{name}\"
        Person{name: name} ->
            \"Minor: #{name}\"
    end
end",
    {ok, ErlangCode} = lx0_compiler:compile(Source),
    ?assert(string:find(ErlangCode, "-record(person") =/= nomatch),
    ?assert(string:find(ErlangCode, "create_person") =/= nomatch),
    ?assert(string:find(ErlangCode, "process_person") =/= nomatch),
    ?assert(string:find(ErlangCode, "case") =/= nomatch).