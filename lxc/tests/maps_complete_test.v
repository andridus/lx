module main

fn test_map_literal_basic() {
	lx_code := '
def test_map() do
  %{name: "John", age: 30}
end'

	result := generates_erlang_result(lx_code)
	assert result.success
	assert result.code.contains('#{name => "John", age => 30}')
}

fn test_map_literal_with_fat_arrow() {
	lx_code := '
def test_map() do
  %{"name" => "John", "age" => 30}
end'

	result := generates_erlang_result(lx_code)
	assert result.success
	assert result.code.contains('#{"name" => "John", "age" => 30}')
}

fn test_map_access() {
	lx_code := '
def get_name(user) do
  user[:name]
end'

	result := generates_erlang_result(lx_code)
	assert result.success
	assert result.code.contains('maps:get(name, User)')
}

fn test_map_update() {
	lx_code := '
def update_age(user) do
  %{user | age: 31}
end'

	result := generates_erlang_result(lx_code)
	assert result.success
	assert result.code.contains('User#{age => 31}')
}

fn test_map_update_with_fat_arrow() {
	lx_code := '
def update_user(user) do
  %{user | "status" => "active"}
end'

	result := generates_erlang_result(lx_code)
	assert result.success
	assert result.code.contains('User#{"status" => "active"}')
}

fn test_empty_map() {
	lx_code := '
def empty_map() do
  %{}
end'

	result := generates_erlang_result(lx_code)
	assert result.success
	assert result.code.contains('#{}.')
}

fn test_map_with_variables() {
	lx_code := '
def create_user(name) do
  %{name: name}
end'

	result := generates_erlang_result(lx_code)
	assert result.success
	assert result.code.contains('#{name => Name}')
}

fn test_nested_map_access() {
	lx_code := '
def get_db_host(config) do
  config[:database][:host]
end'

	result := generates_erlang_result(lx_code)
	assert result.success
	assert result.code.contains('maps:get(host, maps:get(database, Config))')
}

fn test_map_type_inference() {
	lx_code := '
def create_person() do
  %{name: "John", age: 30}
end'

	result := generates_erlang_result(lx_code)
	assert result.success
	// Check that the function spec is generated with correct map type
	assert result.code.contains('-spec create_person() -> #{atom() => string()}.')
	assert result.code.contains('#{name => "John", age => 30}')
}
