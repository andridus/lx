module main

fn test_application_with_map_structure() {
	code := '
application {
  description: "Test Application",
  vsn: "1.0.0",
  applications: [:kernel, :stdlib],
  registered: [:main],
  env: %{debug: true, port: "8080"},
  deps: [:cowboy, {:jsx, "~> 2.0"}]
}
'
	// Test that it compiles without error
	result := generates_erlang_result(code)
	assert result.success == true

	println('✓ Application with map structure test passed')
}

fn test_application_minimal() {
	code := '
application {
  description: "Minimal App",
  vsn: "0.1.0"
}
'
	// Test that it compiles without error
	result := generates_erlang_result(code)
	assert result.success == true

	println('✓ Minimal application test passed')
}

fn test_application_with_key_tokens() {
	code := '
application {
  description: "Key Token Test",
  vsn: "1.0.0",
  env: %{port: "3000", debug: false}
}
'
	// Test that it compiles without error
	result := generates_erlang_result(code)
	assert result.success == true

	println('✓ Application with key tokens test passed')
}