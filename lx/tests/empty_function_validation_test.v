import os

// Test validation of empty functions
fn test_empty_function_validation() {
	lx_code := 'def empty_function() do
end'

	// Write test file
	test_file := 'test_empty.lx'
	os.write_file(test_file, lx_code) or { panic('Failed to write test file') }
	defer { os.rm(test_file) or {} }

	// Try to compile - should fail
	result := os.execute('v run . ${test_file}')

	// Should fail (exit code != 0)
	assert result.exit_code != 0

	// Should contain error message about empty body
	assert result.output.contains('cannot have empty body')
	assert result.output.contains('must contain at least one expression')
}

// Test valid function compiles successfully
fn test_valid_function_compiles() {
	lx_code := 'def valid_function() do
    42
end

def main() do
    valid_function()
end'

	// Write test file
	test_file := 'test_valid.lx'
	os.write_file(test_file, lx_code) or { panic('Failed to write test file') }
	defer {
		os.rm(test_file) or {}
		os.rm(test_file.replace('.lx', '.erl')) or {}
	}

	// Try to compile - should succeed
	result := os.execute('v run . ${test_file}')

	// Should succeed (exit code == 0)
	assert result.exit_code == 0

	// Should not contain error messages
	assert !result.output.contains('cannot have empty body')
}

// Test function with only comments is considered empty
fn test_function_with_only_comments() {
	lx_code := 'def comment_only() do
    # This is just a comment
    # Another comment
end'

	// Write test file
	test_file := 'test_comments.lx'
	os.write_file(test_file, lx_code) or { panic('Failed to write test file') }
	defer { os.rm(test_file) or {} }

	// Try to compile - should fail
	result := os.execute('v run . ${test_file}')

	// Should fail (exit code != 0)
	assert result.exit_code != 0

	// Should contain error message about empty body
	assert result.output.contains('cannot have empty body')
}
