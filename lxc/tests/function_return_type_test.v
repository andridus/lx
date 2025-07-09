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

def criar_usuario(nome :: string, email :: string) :: {:ok, Usuario} do
  {:ok, Usuario{
    id: 1,
    nome: nome,
    email: email,
    ativo: true
  }}
end'

	result := generates_erlang_result(lx_code)
	assert result.success
	assert result.code.contains('-spec criar_usuario(string(), string()) -> {ok, #usuario{}}')
	assert result.code.contains('criar_usuario(Nome, Email) ->')
	assert result.code.contains('#usuario{id = 1, nome = Nome, email = Email, ativo = true}')
}

fn test_function_with_return_type_mismatch() {
	lx_code := '
def add(a :: integer, b :: integer) :: integer do
	{a, b}
end'

	result := generates_erlang_result(lx_code)
	assert !result.success
	assert result.errors.len > 0
	assert result.errors.any(it.contains('Function return type mismatch'))
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

	result := generates_erlang_result(lx_code)
	assert result.success
	assert result.code.contains('-spec get_name() -> string()')
	assert result.code.contains('-spec get_age() -> integer()')
	assert result.code.contains('-spec is_active() -> boolean()')
	assert result.code.contains('get_name() ->')
	assert result.code.contains('"John Doe"')
	assert result.code.contains('get_age() ->')
	assert result.code.contains('30')
	assert result.code.contains('is_active() ->')
	assert result.code.contains('true')
}

fn test_function_with_tuple_return_type() {
	lx_code := '
def create_pair(x :: integer, y :: string) :: {integer, string} do
  {x, y}
end

def create_triple(a :: integer, b :: string, c :: boolean) :: {integer, string, boolean} do
  {a, b, c}
end'

	result := generates_erlang_result(lx_code)
	assert result.success
	assert result.code.contains('-spec create_pair(integer(), string()) -> {integer(), string()}')
	assert result.code.contains('-spec create_triple(integer(), string(), boolean()) -> {integer(), string(), boolean()}')
	assert result.code.contains('create_pair(X, Y) ->')
	assert result.code.contains('{X, Y}')
	assert result.code.contains('create_triple(A, B, C) ->')
	assert result.code.contains('{A, B, C}')
}

fn test_function_with_list_return_type() {
	lx_code := '
def create_numbers() :: list(integer) do
  [1, 2, 3, 4, 5]
end

def create_strings() :: list(string) do
  ["hello", "world"]
end'

	result := generates_erlang_result(lx_code)
	assert result.success
	assert result.code.contains('-spec create_numbers() -> [integer()]')
	assert result.code.contains('-spec create_strings() -> [string()]')
	assert result.code.contains('create_numbers() ->')
	assert result.code.contains('[1, 2, 3, 4, 5]')
	assert result.code.contains('create_strings() ->')
	assert result.code.contains('["hello", "world"]')
}

fn test_function_with_map_return_type() {
	lx_code := '
def create_user_map(name :: string, age :: integer) :: map(atom, string | integer) do
  %{name: name, age: age}
end'

	result := generates_erlang_result(lx_code)
	assert result.success
	assert result.code.contains('-spec create_user_map(string(), integer()) -> #{atom() => string() | integer()}')
	assert result.code.contains('create_user_map(Name, Age) ->')
	assert result.code.contains('#{name => Name, age => Age}')
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

	result := generates_erlang_result(lx_code)
	assert result.success
	assert result.code.contains('-spec create_person(string(), integer(), string()) -> #person{}')
	assert result.code.contains('create_person(Name, Age, Email) ->')
	assert result.code.contains('#person{name = Name, age = Age, email = Email}')
}

fn test_function_with_union_return_type() {
	lx_code := '
def validate_input(input :: string) :: {:ok, string} | {:error, string} do
  if input != "" do
    {:ok, input}
  else
    {:error, "input cannot be empty"}
  end
end'

	result := generates_erlang_result(lx_code)
	assert result.success
	assert result.code.contains('-spec validate_input(string()) -> {ok, string()} | {error, string()}')
	assert result.code.contains('validate_input(Input) ->')
	assert result.code.contains('if Input =/= "" do')
	assert result.code.contains('{ok, Input}')
	assert result.code.contains('else')
	assert result.code.contains('{error, "input cannot be empty"}')
}

fn test_function_with_function_return_type() {
	lx_code := '
def create_adder(x :: integer) :: (integer) -> integer do
  fn(y :: integer) :: integer do
    x + y
  end
end'

	result := generates_erlang_result(lx_code)
	assert result.success
	assert result.code.contains('-spec create_adder(integer()) -> fun((integer()) -> integer())')
	assert result.code.contains('create_adder(X) ->')
	assert result.code.contains('fun(Y) -> X + Y end')
}

fn test_function_with_no_return_type() {
	lx_code := '
def print_message(msg :: string) do
  IO.puts(msg)
end

def calculate_sum(a :: integer, b :: integer) do
  a + b
end'

	result := generates_erlang_result(lx_code)
	assert result.success
	// Functions without return type should still work
	assert result.code.contains('print_message(Msg) ->')
	assert result.code.contains('calculate_sum(A, B) ->')
	assert result.code.contains('A + B')
}

fn test_function_with_complex_return_type() {
	lx_code := '
record User {
  id :: integer,
  name :: string,
  email :: string
}

def process_user_data(data :: string) :: {:ok, User} | {:error, string} | {:pending, string} do
  if data == "" do
    {:error, "data cannot be empty"}
  else if data == "pending" do
    {:pending, "processing"}
  else
    {:ok, User{
      id: 1,
      name: "John",
      email: data
    }}
  end
end'

	result := generates_erlang_result(lx_code)
	assert result.success
	assert result.code.contains('-spec process_user_data(string()) -> {ok, #user{}} | {error, string()} | {pending, string()}')
	assert result.code.contains('process_user_data(Data) ->')
	assert result.code.contains('if Data =:= "" do')
	assert result.code.contains('{error, "data cannot be empty"}')
	assert result.code.contains('else if Data =:= "pending" do')
	assert result.code.contains('{pending, "processing"}')
	assert result.code.contains('else')
	assert result.code.contains('#user{id = 1, name = "John", email = Data}')
}

fn test_function_with_nested_return_type() {
	lx_code := '
def create_nested_structure() :: {list(integer), map(atom, string), {:ok, string} | {:error, string}} do
  numbers = [1, 2, 3]
  config = %{debug: "enabled", mode: "production"}
  result = {:ok, "success"}
  {numbers, config, result}
end'

	result := generates_erlang_result(lx_code)
	assert result.success
	assert result.code.contains('-spec create_nested_structure() -> {[integer()], #{atom() => string()}, {ok, string()} | {error, string()}}')
	assert result.code.contains('create_nested_structure() ->')
	assert result.code.contains('Numbers = [1, 2, 3]')
	assert result.code.contains('Config = #{debug => "enabled", mode => "production"}')
	assert result.code.contains('Result = {ok, "success"}')
	assert result.code.contains('{Numbers, Config, Result}')
}

fn test_function_with_return_type_validation_error() {
	lx_code := '
def get_user_info(id :: integer) :: {:ok, string} do
  if id > 0 do
    "user found"  // This should be {:ok, string} not string
  else
    {:error, "invalid id"}  // This should be {:ok, string} not {:error, string}
  end
end'

	result := generates_erlang_result(lx_code)
	assert !result.success
	assert result.errors.len > 0
	assert result.errors.any(it.contains('Function return type mismatch'))
}
