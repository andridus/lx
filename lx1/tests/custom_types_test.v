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
type status :: active

def create_user(id :: user_id, email :: email, status :: status) do
    {id, email, status}
end'
	expected := '-module(test).
-export([create_user/3]).

-type user_id() :: integer().
-type email() :: binary().
-type status() :: active.
-spec create_user(user_id(), email(), status()) -> {user_id(), email(), status()}.
create_user(ID_1, EMAIL_2, STATUS_3) ->
    {ID_1, EMAIL_2, STATUS_3}.
'
	result := compile_lx(lx_code)
	assert result == expected
}

// Test types with records
fn test_types_with_records() {
	lx_code := 'record user { id :: integer, name :: string }
type user_type :: user

def create_typed_user(id :: integer, name :: string) do
    user{id: id, name: name}
end'
	expected := '-module(test).
-export([create_typed_user/2]).

-record(user, {id = nil :: integer(), name = nil :: binary()}).
-type user_type() :: user.
-spec create_typed_user(integer(), binary()) -> user().
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
	lx_code := 'type result :: atom

def compute() do
    value = 42
    result = :success
    {value, result}
end'
	expected := '-module(test).
-export([compute/0]).

-type result() :: atom().
-spec compute() -> {integer(), atom()}.
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

def get_simple() do
    :atom
end'
	expected := '-module(test).
-export([get_simple/0]).

-type simple() :: atom().
-spec get_simple() -> atom().
get_simple() ->
    atom.
'
	result := compile_lx(lx_code)
	assert result == expected
}

// Test integration with existing features
fn test_type_integration() {
	lx_code := 'type result :: atom
record data { value :: integer }

def process_with_type(d :: data) do
    result = :success
    {result, d.value}
end'
	expected := '-module(test).
-export([process_with_type/1]).

-record(data, {value = nil :: integer()}).
-type result() :: atom().
-spec process_with_type(data()) -> {atom(), integer()}.
process_with_type(D_1) ->
    RESULT_2 = success,
    {RESULT_2, D_1#record.value}.
'
	result := compile_lx(lx_code)
	assert result == expected
}
