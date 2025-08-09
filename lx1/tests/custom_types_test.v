module main

// ============ Custom Types Tests ============

// Test basic type definition
fn test_basic_type_definition() {
	lx_code := 'type my_type :: integer

def test_function(x :: my_type) do
    x
end'
	result := compile_lx(lx_code)
	assert result.contains('-module(test)')
	assert result.contains('test_function')
}

// Test simple union types
fn test_union_types() {
	lx_code := 'type status :: success

def process_status(s :: status) do
    s
end'
	result := compile_lx(lx_code)
	assert result.contains('process_status')
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
	result := compile_lx(lx_code)
	assert result.contains('create_user_id')
	assert result.contains('get_user_id')
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
	result := compile_lx(lx_code)
	assert result.contains('create_email')
	assert result.contains('send_email')
}

// Test multiple type definitions
fn test_multiple_types() {
	lx_code := 'type user_id :: integer
type email :: string
type status :: active

def create_user(id :: user_id, email :: email, status :: status) do
    {id, email, status}
end'
	result := compile_lx(lx_code)
	assert result.contains('create_user')
	assert result.contains('{ID_4, EMAIL_5, STATUS_6}')
}

// Test types with records
fn test_types_with_records() {
	lx_code := 'record user { id :: integer, name :: string }
type user_type :: user

def create_typed_user(id :: integer, name :: string) do
    user{id: id, name: name}
end'
	result := compile_lx(lx_code)
	assert result.contains('-record(user')
	assert result.contains('create_typed_user')
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
	lx_code := 'type result :: success

def compute() do
    value = 42
    result = success
    {value, result}
end'
	result := compile_lx(lx_code)
	assert result.contains('compute')
	assert result.contains('VALUE_1 = 42')
}

// Test type error cases
fn test_type_syntax_errors() {
	// Missing type name
	lx_code := 'type :: integer

def test() do
    1
end'
	result := compile_lx_with_error(lx_code)
	assert result.contains('Expected type name') || result.contains('falha') || result.contains('error')
}

// Test missing double colon
fn test_missing_double_colon() {
	lx_code := 'type my_type integer

def test() do
    1
end'
	result := compile_lx_with_error(lx_code)
	assert result.contains('Expected "::"') || result.contains('falha') || result.contains('error')
}

// Test opaque without base type
fn test_opaque_error() {
	lx_code := 'type opaque

def test() do
    1
end'
	result := compile_lx_with_error(lx_code)
	assert result.contains('Expected type name') || result.contains('falha') || result.contains('error')
}

// Test nominal without base type
fn test_nominal_error() {
	lx_code := 'type nominal

def test() do
    1
end'
	result := compile_lx_with_error(lx_code)
	assert result.contains('Expected type name') || result.contains('falha') || result.contains('error')
}

// Test type with simple values
fn test_type_simple_values() {
	lx_code := 'type simple :: atom

def get_simple() do
    atom
end'
	result := compile_lx(lx_code)
	assert result.contains('get_simple')
	assert result.contains('atom')
}

// Test integration with existing features
fn test_type_integration() {
	lx_code := 'type result :: success
record data { value :: integer }

def process_with_type(d :: data) do
    result = success
    {result, d.value}
end'
	result := compile_lx(lx_code)
	assert result.contains('process_with_type')
	assert result.contains('#data{value = VALUE_')
}