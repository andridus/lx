module main

// =============================================================================
// FUNCTION RETURN TYPE TESTS
// =============================================================================

fn test_function_with_return_type_success() {
	lx_code := '
record Usuario {
  id :: integer,
  nome :: string,
  email :: string,
  ativo :: boolean
}

def criar_usuario(nome :: string, email :: string) :: {atom, Usuario} do
  {:ok, Usuario{
    id: 1,
    nome: nome,
    email: email,
    ativo: true
  }}
end'

	expected := '-module(test).
-export([criar_usuario/2]).

-record(usuario, {id, nome, email, ativo}).
-spec criar_usuario(binary(), binary()) -> {atom(), #usuario{}}.
criar_usuario(Nome, Email) when is_binary(Nome) andalso is_binary(Email) ->
{ok, #usuario{id = 1, nome = Nome, email = Email, ativo = true}}.

'

	assert generates_erlang(lx_code) == expected
}

fn test_function_with_return_type_mismatch() {
	lx_code := '
def add(a :: integer, b :: integer) :: integer do
	{a, b}
end'

	expected := '-module(test).
-export([add/2]).

-spec add(integer(), integer()) -> integer().
add(A, B) when is_integer(A) andalso is_integer(B) ->
{A, B}.

'

	assert generates_erlang(lx_code) == expected
}

fn test_function_with_simple_return_type() {
	lx_code := '
def get_name() :: string do
  "John Doe"
end

def get_age() :: integer do
  30
end

def is_active() :: boolean do
  true
end'

	expected := '-module(test).
-export([get_name/0, get_age/0, is_active/0]).

-spec get_name() -> binary().
get_name() ->
<<"John Doe"/utf8>>.

-spec get_age() -> integer().
get_age() ->
30.

-spec is_active() -> boolean().
is_active() ->
true.

'

	assert generates_erlang(lx_code) == expected
}

fn test_function_with_tuple_return_type() {
	lx_code := '
def create_pair(x :: integer, y :: string) :: {integer, string} do
  {x, y}
end

def create_triple(a :: integer, b :: string, c :: boolean) :: {integer, string, boolean} do
  {a, b, c}
end'

	expected := '-module(test).
-export([create_pair/2, create_triple/3]).

-spec create_pair(integer(), binary()) -> {integer(), binary()}.
create_pair(X, Y) when is_integer(X) andalso is_binary(Y) ->
{X, Y}.

-spec create_triple(integer(), binary(), boolean()) -> {integer(), binary(), boolean()}.
create_triple(A, B, C) when is_integer(A) andalso is_binary(B) andalso is_boolean(C) ->
{A, B, C}.

'

	assert generates_erlang(lx_code) == expected
}

fn test_function_with_list_return_type() {
	lx_code := '
def create_numbers() :: list(integer) do
  [1, 2, 3, 4, 5]
end

def create_strings() :: list(string) do
  ["hello", "world"]
end'

	expected := '-module(test).
-export([create_numbers/0, create_strings/0]).

-spec create_numbers() -> [integer()].
create_numbers() ->
[1, 2, 3, 4, 5].

-spec create_strings() -> [binary()].
create_strings() ->
[<<"hello"/utf8>>, <<"world"/utf8>>].

'

	assert generates_erlang(lx_code) == expected
}

fn test_function_with_map_return_type() {
	lx_code := '
def create_user_map(name :: string, age :: integer) :: map(atom, string | integer) do
  %{name: name, age: age}
end'

	expected := '-module(test).
-export([create_user_map/2]).

-spec create_user_map(binary(), integer()) -> #{atom() => binary() | integer()}.
create_user_map(Name, Age) when is_binary(Name) andalso is_integer(Age) ->
#{name => Name, age => Age}.

'

	assert generates_erlang(lx_code) == expected
}

fn test_function_with_record_return_type() {
	lx_code := '
record Person {
  name :: string,
  age :: integer,
  email :: string
}

def create_person(name :: string, age :: integer, email :: string) :: Person do
  Person{
    name: name,
    age: age,
    email: email
  }
end'

	expected := '-module(test).
-export([create_person/3]).

-record(person, {name, age, email}).
-spec create_person(binary(), integer(), binary()) -> #person{}.
create_person(Name, Age, Email) when is_binary(Name) andalso is_integer(Age) andalso is_binary(Email) ->
#person{name = Name, age = Age, email = Email}.

'

	assert generates_erlang(lx_code) == expected
}

fn test_function_with_union_return_type() {
	lx_code := '
def validate_input(input :: string) do
  if input != "" do
    {:ok, input}
  else
    {:error, "input cannot be empty"}
  end
end'

	expected := '-module(test).
-export([validate_input/1]).

-spec validate_input(binary()) -> {atom(), binary()}.
validate_input(Input) when is_binary(Input) ->
case Input =/= <<""/utf8>> of
    true ->
        {ok, Input};
    false ->
        {error, <<"input cannot be empty"/utf8>>}
end.

'
	assert generates_erlang(lx_code) == expected
}

// fn test_function_with_function_return_type() {
// 	lx_code := '
// def create_adder(x :: integer) :: (integer) -> integer do
//   fn(y :: integer) :: integer do
//     x + y
//   end
// end'

// 	result := generates_erlang_result(lx_code)
// 	assert result.success
// 	assert result.code.contains('-spec create_adder(integer()) -> fun((integer()) -> integer())')
// 	assert result.code.contains('create_adder(X) ->')
// 	assert result.code.contains('fun(Y) -> X + Y end')
// }

// fn test_function_with_no_return_type() {
// 	lx_code := '
// def print_message(msg :: string) do
//   IO.puts(msg)
// end

// def calculate_sum(a :: integer, b :: integer) do
//   a + b
// end'

// 	result := generates_erlang_result(lx_code)
// 	assert result.success
// 	// Functions without return type should still work
// 	assert result.code.contains('print_message(Msg) ->')
// 	assert result.code.contains('calculate_sum(A, B) ->')
// 	assert result.code.contains('A + B')
// }

fn test_function_with_complex_return_type() {
	lx_code := '
record User {
  id :: integer,
  name :: string,
  email :: string
}

def process_user_data(data :: string) :: {atom, User} | {atom, string} do
  if data == "" do
    {:error, "data cannot be empty"}
  else
    if data == "pending" do
      {:pending, "processing"}
    else
      {:ok, User{
        id: 1,
        name: "John",
        email: data
      }}
    end
  end
end'

	expected := '-module(test).
-export([process_user_data/1]).

-record(user, {id, name, email}).
-spec process_user_data(binary()) -> {atom(), #user{}} | {atom(), binary()}.
process_user_data(Data) when is_binary(Data) ->
case Data =:= <<""/utf8>> of
    true ->
        {error, <<"data cannot be empty"/utf8>>};
    false ->
        case Data =:= <<"pending"/utf8>> of
    true ->
        {pending, <<"processing"/utf8>>};
    false ->
        {ok, #user{id = 1, name = <<"John"/utf8>>, email = Data}}
end
end.

'

	assert generates_erlang(lx_code) == expected
}

fn test_function_with_nested_return_type() {
	lx_code := '
def create_nested_structure() :: {list(integer), map(atom, string), {atom, string} | {atom, string}} do
  numbers = [1, 2, 3]
  config = %{debug: "enabled", mode: "production"}
  result = {:ok, "success"}
  {numbers, config, result}
end'

	expected := '-module(test).
-export([create_nested_structure/0]).

-spec create_nested_structure() -> {[integer()], #{atom() => binary()}, {atom(), binary()}}.
create_nested_structure() ->
Numbers_aaaa = [1, 2, 3],
Config_baaa = #{debug => <<"enabled"/utf8>>, mode => <<"production"/utf8>>},
Result_caaa = {ok, <<"success"/utf8>>},
{Numbers_aaaa, Config_baaa, Result_caaa}.

'

	assert generates_erlang(lx_code) == expected
}

fn test_function_with_return_type_validation_error() {
	lx_code := '
def get_user_info(id :: integer) :: {:ok, string} do
  if id > 0 do
    "user found"
  else
    {:error, "invalid id"}
  end
end'

	expected := '-module(test).
-export([get_user_info/1]).

-spec get_user_info(integer()) -> {:ok, binary()}.
get_user_info(Id) when is_integer(Id) ->
case Id > 0 of
    true ->
        <<"user found"/utf8>>;
    false ->
        {error, <<"invalid id"/utf8>>}
end.

'

	assert generates_erlang(lx_code) == expected
}
