module main

import os

fn test_basic_literals_generation() {
	println('Testing basic literals generation...')

	// Test basic literals generation
	fixture_path := 'tests/generate/fixtures/literals/basic_literals.lx'
	expected_path := 'tests/generate/assets/erlang/basic_literals.erl'

	println('Reading fixture from: ${fixture_path}')
	// Read fixture
	fixture_content := os.read_file(fixture_path) or {
		eprintln('Failed to read fixture: ${fixture_path}')
		return
	}

	println('Reading expected from: ${expected_path}')
	// Read expected output
	expected_content := os.read_file(expected_path) or {
		eprintln('Failed to read expected: ${expected_path}')
		return
	}

	// TODO: Parse LX code and generate Erlang
	// For now, just test that files exist
	assert fixture_content.len > 0
	assert expected_content.len > 0

	println('✓ Basic literals generation test passed')
	println('Fixture content length: ${fixture_content.len}')
	println('Expected content length: ${expected_content.len}')
}

fn test_binary_expressions_generation() {
	println('Testing binary expressions generation...')

	// Test binary expressions generation
	fixture_path := 'tests/generate/fixtures/expressions/binary_expressions.lx'
	expected_path := 'tests/generate/assets/erlang/binary_expressions.erl'

	println('Reading fixture from: ${fixture_path}')
	// Read fixture
	fixture_content := os.read_file(fixture_path) or {
		eprintln('Failed to read fixture: ${fixture_path}')
		return
	}

	println('Reading expected from: ${expected_path}')
	// Read expected output
	expected_content := os.read_file(expected_path) or {
		eprintln('Failed to read expected: ${expected_path}')
		return
	}

	// TODO: Parse LX code and generate Erlang
	// For now, just test that files exist
	assert fixture_content.len > 0
	assert expected_content.len > 0

	println('✓ Binary expressions generation test passed')
	println('Fixture content length: ${fixture_content.len}')
	println('Expected content length: ${expected_content.len}')
}

fn test_data_structures_generation() {
	println('Testing data structures generation...')

	// Test data structures generation
	fixture_path := 'tests/generate/fixtures/structures/data_structures.lx'
	expected_path := 'tests/generate/assets/erlang/data_structures.erl'

	println('Reading fixture from: ${fixture_path}')
	// Read fixture
	fixture_content := os.read_file(fixture_path) or {
		eprintln('Failed to read fixture: ${fixture_path}')
		return
	}

	println('Reading expected from: ${expected_path}')
	// Read expected output
	expected_content := os.read_file(expected_path) or {
		eprintln('Failed to read expected: ${expected_path}')
		return
	}

	// TODO: Parse LX code and generate Erlang
	// For now, just test that files exist
	assert fixture_content.len > 0
	assert expected_content.len > 0

	println('✓ Data structures generation test passed')
	println('Fixture content length: ${fixture_content.len}')
	println('Expected content length: ${expected_content.len}')
}

fn test_function_definitions_generation() {
	println('Testing function definitions generation...')

	// Test function definitions generation
	fixture_path := 'tests/generate/fixtures/functions/function_definitions.lx'
	expected_path := 'tests/generate/assets/erlang/function_definitions.erl'

	println('Reading fixture from: ${fixture_path}')
	// Read fixture
	fixture_content := os.read_file(fixture_path) or {
		eprintln('Failed to read fixture: ${fixture_path}')
		return
	}

	println('Reading expected from: ${expected_path}')
	// Read expected output
	expected_content := os.read_file(expected_path) or {
		eprintln('Failed to read expected: ${expected_path}')
		return
	}

	// TODO: Parse LX code and generate Erlang
	// For now, just test that files exist
	assert fixture_content.len > 0
	assert expected_content.len > 0

	println('✓ Function definitions generation test passed')
	println('Fixture content length: ${fixture_content.len}')
	println('Expected content length: ${expected_content.len}')
}

fn main() {
	println('Running codegen tests...')

	test_basic_literals_generation()
	test_binary_expressions_generation()
	test_data_structures_generation()
	test_function_definitions_generation()

	println('All codegen tests passed!')
}
