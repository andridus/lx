module main

import frontend.parser
import frontend.lexer

fn test_function_without_parentheses_and_clauses() {
	// Test case: def func do a end (invalid - no clauses)
	code := 'def func do\n  a\nend'

	// Lex the code
	mut lexer_instance := lexer.new_lexer(code, 'test.lx')
	mut tokens := []lexer.Token{}

	// Collect all tokens
	for !lexer_instance.is_at_end() {
		token := lexer_instance.next_token()
		tokens << token
		if token is lexer.EOFToken {
			break
		}
	}

	// Parse the code
	mut parser_instance := parser.new_main_parser(tokens)

	// Try to parse the module
	_ = parser_instance.parse_module() or { return }

	// Should have errors
	assert parser_instance.has_errors()

	// Check the error message
	errors := parser_instance.get_errors()
	assert errors.len > 0

	// Look for the specific error message
	mut found_error := false
	for error in errors {
		if error.message.contains('Multi-clause function must have at least one clause') {
			found_error = true
			break
		}
	}
	assert found_error

	println('✓ Function without parentheses and clauses test passed')
}

fn test_valid_single_clause_function() {
	// Test case: def func() do a end (valid)
	code := 'def func() do\n  a\nend'

	// Lex the code
	mut lexer_instance := lexer.new_lexer(code, 'test.lx')
	mut tokens := []lexer.Token{}

	// Collect all tokens
	for !lexer_instance.is_at_end() {
		token := lexer_instance.next_token()
		tokens << token
		if token is lexer.EOFToken {
			break
		}
	}

	// Parse the code
	mut parser_instance := parser.new_main_parser(tokens)

	// Try to parse the module
	_ = parser_instance.parse_module() or { return }

	// Should not have errors
	assert !parser_instance.has_errors()

	println('✓ Valid single clause function test passed')
}

fn test_valid_multi_clause_function() {
	// Test case: def func do (x) -> x end (valid)
	code := 'def func do\n  (x) -> x\nend'

	// Lex the code
	mut lexer_instance := lexer.new_lexer(code, 'test.lx')
	mut tokens := []lexer.Token{}

	// Collect all tokens
	for !lexer_instance.is_at_end() {
		token := lexer_instance.next_token()
		tokens << token
		if token is lexer.EOFToken {
			break
		}
	}

	// Parse the code
	mut parser_instance := parser.new_main_parser(tokens)

	// Try to parse the module
	_ = parser_instance.parse_module() or { return }

	// Should not have errors
	assert !parser_instance.has_errors()

	println('✓ Valid multi clause function test passed')
}

fn main() {
	test_function_without_parentheses_and_clauses()
	test_valid_single_clause_function()
	test_valid_multi_clause_function()
	println('All function syntax tests passed!')
}