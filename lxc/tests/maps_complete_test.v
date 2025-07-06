module main

fn test_map_literal_basic() {
	lx_code := '
def test_map() do
  %{name: "John", age: 30}
end'

	result := generates_erlang(lx_code)
	assert result.success
	assert result.code.contains('#{name => "John", age => 30}')
	println('✓ Map literal basic: passed')
}

fn test_map_literal_with_fat_arrow() {
	lx_code := '
def test_map() do
  %{"name" => "John", "age" => 30}
end'

	result := generates_erlang(lx_code)
	assert result.success
	assert result.code.contains('#{"name" => "John", "age" => 30}')
	println('✓ Map literal with fat arrow: passed')
}

fn test_map_access() {
	lx_code := '
def get_name(user) do
  user[:name]
end'

	result := generates_erlang(lx_code)
	assert result.success
	assert result.code.contains('maps:get(name, User)')
	println('✓ Map access: passed')
}

fn test_map_update() {
	lx_code := '
def update_age(user) do
  %{user | age: 31}
end'

	result := generates_erlang(lx_code)
	assert result.success
	assert result.code.contains('User#{age => 31}')
	println('✓ Map update: passed')
}

fn test_map_update_with_fat_arrow() {
	lx_code := '
def update_user(user) do
  %{user | "status" => "active"}
end'

	result := generates_erlang(lx_code)
	assert result.success
	assert result.code.contains('User#{"status" => "active"}')
	println('✓ Map update with fat arrow: passed')
}

fn test_empty_map() {
	lx_code := '
def empty_map() do
  %{}
end'

	result := generates_erlang(lx_code)
	assert result.success
	assert result.code.contains('#{}.')
	println('✓ Empty map: passed')
}

fn test_map_with_variables() {
	lx_code := '
def create_user(name) do
  %{name: name}
end'

	result := generates_erlang(lx_code)
	assert result.success
	assert result.code.contains('#{name => Name}')
	println('✓ Map with variables: passed')
}

fn test_nested_map_access() {
	lx_code := '
def get_db_host(config) do
  config[:database][:host]
end'

	result := generates_erlang(lx_code)
	assert result.success
	assert result.code.contains('maps:get(host, maps:get(database, Config))')
	println('✓ Nested map access: passed')
}

fn test_map_type_inference() {
	lx_code := '
def create_person() do
  %{name: "John", age: 30}
end'

	result := generates_erlang(lx_code)
	assert result.success
	// Check that the function spec is generated with correct map type
	assert result.code.contains('-spec create_person() -> #{atom() => string()}.')
	assert result.code.contains('#{name => "John", age => 30}')
	println('✓ Map type inference: passed')
}

// Test all core map functionality
fn test_all_map_features() {
	println('Testing Maps Implementation:')
	println('========================================')

	test_map_literal_basic()
	test_map_literal_with_fat_arrow()
	test_map_access()
	test_map_update()
	test_map_update_with_fat_arrow()
	test_empty_map()
	test_map_with_variables()
	test_nested_map_access()
	test_map_type_inference()

	println('========================================')
	println('All core map tests passed! 🎉')
}
