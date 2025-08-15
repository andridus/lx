module main

// ============ Custom Types Tests ============

// Test basic type definition
fn test_basic_type_definition() {
	lx_code := 'type my_type :: integer

def test_function(x :: my_type) do
    x
end'
	expected := '-module(test).
-export([test_function/1]).

-type my_type() :: integer().
-spec test_function(my_type()) -> my_type().
test_function(X_1) ->
    X_1.
'
	result := compile_lx(lx_code)
	assert result == expected
}

// Test simple union types
fn test_union_types() {
	lx_code := 'type status :: boolean

def process_status(s :: status) do
    s
end'
	expected := '-module(test).
-export([process_status/1]).

-type status() :: boolean().
-spec process_status(status()) -> status().
process_status(S_1) ->
    S_1.
'
	result := compile_lx(lx_code)
	assert result == expected
}

// Test opaque types
fn test_opaque_types() {
	lx_code := 'type opaque user_id :: integer

def create_user_id(id :: integer) do
    id
end

def get_user_id(uid :: user_id) do
    uid
end'
	expected := '-module(test).
-export([create_user_id/1, get_user_id/1]).

-opaque user_id() :: integer().
-spec create_user_id(integer()) -> integer().
create_user_id(ID_1) ->
    ID_1.
-spec get_user_id(user_id()) -> user_id().
get_user_id(UID_2) ->
    UID_2.
'
	result := compile_lx(lx_code)
	assert result == expected
}

// Test nominal types
fn test_nominal_types() {
	lx_code := 'type nominal email :: string

def create_email(addr :: string) do
    addr
end

def send_email(e :: email) do
    e
end'
	expected := '-module(test).
-export([create_email/1, send_email/1]).

-nominal email() :: binary().
-spec create_email(binary()) -> binary().
create_email(ADDR_1) ->
    ADDR_1.
-spec send_email(email()) -> email().
send_email(E_2) ->
    E_2.
'
	result := compile_lx(lx_code)
	assert result == expected
}

// Test multiple type definitions
fn test_multiple_types() {
	lx_code := 'type user_id :: integer
type email :: string
type status :: :active

def create_user(id :: user_id, email :: email, status :: status) do
    {id, email, status}
end'
	expected := '-module(test).
-export([create_user/3]).

-type user_id() :: integer().
-type email() :: binary().
-type status() :: atom().
-spec create_user(user_id(), email(), status()) -> {user_id(), email(), status()}.
create_user(ID_1, EMAIL_2, STATUS_3) ->
    {ID_1, EMAIL_2, STATUS_3}.
'
	result := compile_lx(lx_code)
	assert result == expected
}

// Test types with records
fn test_types_with_records() {
	lx_code := 'record User{ id :: integer, name :: string }
type user_type :: User

def create_typed_user(id :: integer, name :: string) :: user_type do
    User{id: id, name: name}
end'
	expected := '-module(test).
-export([create_typed_user/2]).

-record(user, {id = nil :: integer(), name = nil :: binary()}).
-type user_type() :: #user{}.
-spec create_typed_user(integer(), binary()) -> user_type().
create_typed_user(ID_1, NAME_2) ->
    #user{id = ID_1, name = NAME_2}.
'
	result := compile_lx(lx_code)
	assert result == expected
}

// Test types in function signatures
fn test_types_in_signatures() {
	lx_code := 'type id :: integer
type name :: string

def process_data(id :: id, name :: name) do
    {id, name}
end'
	result := compile_lx(lx_code)
	assert result.contains('process_data')
	assert result.contains('-spec process_data')
}

// Test nested type usage
fn test_nested_type_usage() {
	lx_code := 'type inner :: integer
type outer :: inner

def process(x :: outer) do
    x
end'
	result := compile_lx(lx_code)
	assert result.contains('process')
}

// Test type with complex expressions
fn test_type_complex_expressions() {
	lx_code := 'type result :: {:integer, atom}

def compute() :: result do
    value = 42
    result = :success
    {value, result}
end'
	expected := '-module(test).
-export([compute/0]).

-type result() :: {atom(), atom()}.
-spec compute() -> result().
compute() ->
    VALUE_1 = 42,
    RESULT_2 = success,
    {VALUE_1, RESULT_2}.
'
	result := compile_lx(lx_code)
	assert result == expected
}

// Test type error cases
fn test_type_syntax_errors() {
	// Missing type name
	lx_code := 'type :: integer

def test_function() do
    1
end'
	result := compile_lx_with_error(lx_code)
	assert result.contains('Expected type name') || result.contains('falha')
		|| result.contains('error')
}

// Test missing double colon
fn test_missing_double_colon() {
	lx_code := 'type my_type integer

def test_function() do
    1
end'
	result := compile_lx_with_error(lx_code)
	assert result.contains('Expected "::"') || result.contains('falha') || result.contains('error')
}

// Test opaque without base type
fn test_opaque_error() {
	lx_code := 'type opaque

def test_function() do
    1
end'
	result := compile_lx_with_error(lx_code)
	assert result.contains('Expected type name') || result.contains('falha')
		|| result.contains('error')
}

// Test nominal without base type
fn test_nominal_error() {
	lx_code := 'type nominal

def test_function() do
    1
end'
	result := compile_lx_with_error(lx_code)
	assert result.contains('Expected type name') || result.contains('falha')
		|| result.contains('error')
}

// Test type with simple values
fn test_type_simple_values() {
	lx_code := 'type simple :: atom

def get_simple() :: simple do
    :atom
end'
	expected := '-module(test).
-export([get_simple/0]).

-type simple() :: atom().
-spec get_simple() -> simple().
get_simple() ->
    atom.
'
	result := compile_lx(lx_code)
	assert result == expected
}

// Test integration with existing features
fn test_type_integration() {
	lx_code := 'type result :: {:atom, integer}
record Data { value :: integer }

def process_with_type(d :: Data) :: result do
    result = :success
    {result, d.value}
end'
	expected := '-module(test).
-export([process_with_type/1]).

-record(data, {value = nil :: integer()}).
-type result() :: {atom(), integer()}.
-spec process_with_type(#data{}) -> result().
process_with_type(D_1) ->
    RESULT_2 = success,
    {RESULT_2, D_1#data.value}.
'
	result := compile_lx(lx_code)
	assert result == expected
}

// ============ Record Usage Detection Tests ============

// Test record usage in patterns
fn test_record_usage_in_patterns() {
	lx_code := 'record User{ name :: string, age :: integer }

def process_user(u) do
    case u do
        User{name: _name, age: age} when age > 18 ->
            "adult user"
        User{age: age} when age <= 18 -> "minor"
        _ -> "invalid user"
    end
end'
	expected := '-module(test).
-export([process_user/1]).

-record(user, {name = nil :: binary(), age = nil :: integer()}).
-spec process_user(any()) -> binary().
process_user(U_1) ->
    case U_1 of
        #user{name = _NAME_2, age = AGE_3} when AGE_3 > 18 ->
            <<"adult user"/utf8>>;
        #user{age = AGE_3} when AGE_3 =< 18 ->
            <<"minor"/utf8>>;
        _ ->
            <<"invalid user"/utf8>>
    end.
'
	result := compile_lx(lx_code)
	assert result == expected
}

// Test record usage in literals
fn test_record_usage_in_literals() {
	lx_code := 'record Person { name :: string, age :: integer }

def create_person(name :: string, age :: integer) do
    Person{name: name, age: age}
end'
	expected := '-module(test).
-export([create_person/2]).

-record(person, {name = nil :: binary(), age = nil :: integer()}).
-spec create_person(binary(), integer()) -> #person{}.
create_person(NAME_1, AGE_2) ->
    #person{name = NAME_1, age = AGE_2}.
'
	result := compile_lx(lx_code)
	assert result == expected
}

// Test record usage in field access
fn test_record_usage_in_field_access() {
	lx_code := 'record Person { name :: string, age :: integer }

def get_name(person :: Person) do
    person.name
end'
	expected := '-module(test).
-export([get_name/1]).

-record(person, {name = nil :: binary(), age = nil :: integer()}).
-spec get_name(#person{}) -> binary().
get_name(PERSON_1) ->
    PERSON_1#person.name.
'
	result := compile_lx(lx_code)
	assert result == expected
}

// ============ Type Validation Error Tests ============

// Test undefined base type error
fn test_undefined_base_type_error() {
	lx_code := 'record User{ id :: integer, name :: string }
type user_type :: user

def create_typed_user(id :: integer, name :: string) do
    User{id: id, name: name}
end'

	// Should fail compilation due to undefined type 'user'
	result := compile_lx_with_error(lx_code)
	assert result.contains('Undefined type: user')
}

// Test valid base type (record reference)
fn test_valid_record_base_type() {
	lx_code := 'record User{ id :: integer, name :: string }
type user_type :: User

def create_typed_user(id :: integer, name :: string) :: user_type do
    User{id: id, name: name}
end'
	expected := '-module(test).
-export([create_typed_user/2]).

-record(user, {id = nil :: integer(), name = nil :: binary()}).
-type user_type() :: #user{}.
-spec create_typed_user(integer(), binary()) -> user_type().
create_typed_user(ID_1, NAME_2) ->
    #user{id = ID_1, name = NAME_2}.
'
	result := compile_lx(lx_code)
	assert result == expected
}

// ============ Return Type Compatibility Tests ============

// Test compatible specialized atom types
fn test_compatible_specialized_atom_types() {
	lx_code := 'type status :: :ok

def get_status() :: status do
    :ok
end'
	expected := '-module(test).
-export([get_status/0]).

-type status() :: atom().
-spec get_status() -> status().
get_status() ->
    ok.
'
	result := compile_lx(lx_code)
	assert result == expected
}

// Test incompatible specialized atom types
fn test_incompatible_specialized_atom_types() {
	lx_code := 'type status :: :ok

def get_status() :: status do
    :error
end'

	// Should fail due to type mismatch
	result := compile_lx_with_error(lx_code)
	assert result.contains('Return type mismatch')
}

// Test generic atom to specialized atom compatibility
fn test_generic_atom_compatibility() {
	lx_code := 'type my_atom :: atom

def get_atom() :: my_atom do
    :anything
end'
	expected := '-module(test).
-export([get_atom/0]).

-type my_atom() :: atom().
-spec get_atom() -> my_atom().
get_atom() ->
    anything.
'
	result := compile_lx(lx_code)
	assert result == expected
}

// Test type alias compatibility
fn test_type_alias_compatibility() {
	lx_code := 'type status :: atom
type result :: status

def process() :: result do
    :success
end'
	expected := '-module(test).
-export([process/0]).

-type status() :: atom().
-type result() :: status().
-spec process() -> result().
process() ->
    success.
'
	result := compile_lx(lx_code)
	assert result == expected
}

// Test any to specific type resolution
fn test_any_to_specific_type_resolution() {
	lx_code := 'type user_id :: integer

def get_id() :: user_id do
    42
end'
	expected := '-module(test).
-export([get_id/0]).

-type user_id() :: integer().
-spec get_id() -> user_id().
get_id() ->
    42.
'
	result := compile_lx(lx_code)
	assert result == expected
}

// ============ Unused Type Detection Tests ============

// Test unused type error
fn test_unused_type_error() {
	lx_code := 'type status :: :ok

def process_data(input) do
    input
end'

	// Should fail due to unused type
	result := compile_lx_with_error(lx_code)
	assert result.contains('Unused type: status')
}

// Test opaque type usage detection
fn test_opaque_type_usage() {
	lx_code := 'type opaque user_id :: integer

def create_user_id(id :: integer) :: user_id do
    id
end

def get_user_id(uid :: user_id) :: user_id do
    uid
end'
	expected := '-module(test).
-export([create_user_id/1, get_user_id/1]).

-opaque user_id() :: integer().
-spec create_user_id(integer()) -> user_id().
create_user_id(ID_1) ->
    ID_1.
-spec get_user_id(user_id()) -> user_id().
get_user_id(UID_2) ->
    UID_2.
'
	result := compile_lx(lx_code)
	assert result == expected
}

// Test nominal type usage detection
fn test_nominal_type_usage() {
	lx_code := 'type nominal temperature :: float

def create_temp(t :: float) :: temperature do
    t
end

def get_celsius(temp :: temperature) :: temperature do
    temp
end'
	expected := '-module(test).
-export([create_temp/1, get_celsius/1]).

-nominal temperature() :: float().
-spec create_temp(float()) -> temperature().
create_temp(T_1) ->
    T_1.
-spec get_celsius(temperature()) -> temperature().
get_celsius(TEMP_2) ->
    TEMP_2.
'
	result := compile_lx(lx_code)
	assert result == expected
}
