module main

import errors
import ast

fn test_error_formatting_with_categories() {
	formatter := errors.new_error_formatter()

	// Test syntax error
	syntax_err := errors.new_compilation_error(
		errors.SyntaxError{
			message:  'Expected function name after \'def\', but found reserved keyword \'do\'.'
			expected: 'identifier'
			found:    'do'
		},
		ast.Position{line: 7, column: 5, filename: 'src/user_controller.lx'},
		'Expected function name after \'def\', but found reserved keyword \'do\'.'
	)

	// Test type error
	type_err := errors.new_compilation_error(
		errors.TypeError{
			message:    'Type mismatch: expected integer, got string'
			expected:   'integer'
			actual:     'string'
			suggestion: 'use an integer value instead of a string'
		},
		ast.Position{line: 3, column: 7, filename: 'src/math.lx'},
		'Type mismatch: expected integer, got string'
	)

	// Test pattern error
	pattern_err := errors.new_compilation_error(
		errors.PatternError{
			message:   'Cannot match pattern: field \'age\' not found in map'
			pattern:   '%{ name: user_name, age: user_age }'
			value:     'user_data'
			suggestion: 'check if the map contains the \'age\' field'
		},
		ast.Position{line: 12, column: 15, filename: 'src/user_service.lx'},
		'Cannot match pattern: field \'age\' not found in map'
	)

	// Test record error
	record_err := errors.new_compilation_error(
		errors.RecordError{
			message:     'Record \'Person\' field \'email\' is required but not provided'
			record_name: 'Person'
			field_name:  'email'
			suggestion:  'add the missing \'email\' field'
		},
		ast.Position{line: 8, column: 10, filename: 'src/models.lx'},
		'Record \'Person\' field \'email\' is required but not provided'
	)

	// Test binary error
	binary_err := errors.new_compilation_error(
		errors.BinaryError{
			message:       'Binary pattern size mismatch: expected 16 bits, got 8 bits'
			expected_size: 16
			actual_size:   8
			suggestion:    'ensure the binary contains at least 24 bits before the variable-length data'
		},
		ast.Position{line: 15, column: 20, filename: 'src/protocol.lx'},
		'Binary pattern size mismatch: expected 16 bits, got 8 bits'
	)

	// Test guard error
	guard_err := errors.new_compilation_error(
		errors.GuardError{
			message:     'Invalid guard expression: \'++\' operator not allowed in guards'
			expression:  'x > 0 andalso "error" ++ "message"'
			suggestion:  'guards must be pure boolean expressions'
		},
		ast.Position{line: 5, column: 25, filename: 'src/validator.lx'},
		'Invalid guard expression: \'++\' operator not allowed in guards'
	)

	// Test dependency error
	dependency_err := errors.new_compilation_error(
		errors.DependencyError{
			message:     'Module \'crypto\' not found in dependencies'
			module_name: 'crypto'
			suggestion:  'add \':crypto\' to your deps list'
		},
		ast.Position{line: 3, column: 5, filename: 'src/auth.lx'},
		'Module \'crypto\' not found in dependencies'
	)

	// Verify error categories
	assert syntax_err.kind.get_error_category() == 'Syntax'
	assert type_err.kind.get_error_category() == 'Type'
	assert pattern_err.kind.get_error_category() == 'Pattern'
	assert record_err.kind.get_error_category() == 'Record'
	assert binary_err.kind.get_error_category() == 'Binary'
	assert guard_err.kind.get_error_category() == 'Guard'
	assert dependency_err.kind.get_error_category() == 'Dependency'

	// Test simple formatting
	syntax_formatted := errors.format_error_simple(syntax_err)
	assert syntax_formatted.contains('[Syntax Error]')
	assert syntax_formatted.contains('src/user_controller.lx:7:5')

	type_formatted := errors.format_error_simple(type_err)
	assert type_formatted.contains('[Type Error]')
	assert type_formatted.contains('src/math.lx:3:7')

	println('✓ Error formatting with categories test passed')
}

fn test_suggestion_generation() {
	// Test syntax suggestions
	syntax_err := errors.new_compilation_error(
		errors.SyntaxError{
			message:  'Expected identifier, found keyword'
			expected: 'identifier'
			found:    'do'
		},
		ast.Position{line: 1, column: 1, filename: 'test.lx'},
		'Expected identifier, found keyword'
	)

	syntax_suggestions := errors.generate_suggestions(syntax_err)
	assert syntax_suggestions.len > 0
	assert syntax_suggestions[0].contains('identifier')

	// Test type suggestions
	type_err := errors.new_compilation_error(
		errors.TypeError{
			message:    'Type mismatch'
			expected:   'integer'
			actual:     'string'
			suggestion: 'convert string to integer'
		},
		ast.Position{line: 1, column: 1, filename: 'test.lx'},
		'Type mismatch'
	)

	type_suggestions := errors.generate_suggestions(type_err)
	assert type_suggestions.len > 0
	assert type_suggestions[0].contains('integer')

	// Test pattern suggestions
	pattern_err := errors.new_compilation_error(
		errors.PatternError{
			message:   'field not found in map'
			pattern:   '%{ name: user_name, age: user_age }'
			value:     'user_data'
			suggestion: 'check if the map contains the required field'
		},
		ast.Position{line: 1, column: 1, filename: 'test.lx'},
		'field not found in map'
	)

	pattern_suggestions := errors.generate_suggestions(pattern_err)
	assert pattern_suggestions.len > 0
	assert pattern_suggestions[0].contains('map')

	// Test record suggestions
	record_err := errors.new_compilation_error(
		errors.RecordError{
			message:     'field is required but not provided'
			record_name: 'Person'
			field_name:  'email'
			suggestion:  'add the missing field'
		},
		ast.Position{line: 1, column: 1, filename: 'test.lx'},
		'field is required but not provided'
	)

	record_suggestions := errors.generate_suggestions(record_err)
	assert record_suggestions.len > 0
	assert record_suggestions[0].contains('Person')

	// Test binary suggestions
	binary_err := errors.new_compilation_error(
		errors.BinaryError{
			message:       'size mismatch'
			expected_size: 16
			actual_size:   8
			suggestion:    'ensure the binary contains enough bits'
		},
		ast.Position{line: 1, column: 1, filename: 'test.lx'},
		'size mismatch'
	)

	binary_suggestions := errors.generate_suggestions(binary_err)
	assert binary_suggestions.len > 0
	assert binary_suggestions[0].contains('binary')

	// Test guard suggestions
	guard_err := errors.new_compilation_error(
		errors.GuardError{
			message:     'operator not allowed in guards'
			expression:  'x > 0 andalso "error" ++ "message"'
			suggestion:  'guards must be pure boolean expressions'
		},
		ast.Position{line: 1, column: 1, filename: 'test.lx'},
		'operator not allowed in guards'
	)

	guard_suggestions := errors.generate_suggestions(guard_err)
	assert guard_suggestions.len > 0
	assert guard_suggestions[0].contains('boolean')

	// Test dependency suggestions
	dependency_err := errors.new_compilation_error(
		errors.DependencyError{
			message:     'Module not found in dependencies'
			module_name: 'crypto'
			suggestion:  'add to your deps list'
		},
		ast.Position{line: 1, column: 1, filename: 'test.lx'},
		'Module not found in dependencies'
	)

	dependency_suggestions := errors.generate_suggestions(dependency_err)
	assert dependency_suggestions.len > 0
	assert dependency_suggestions[0].contains('deps')

	println('✓ Suggestion generation test passed')
}

fn test_error_collection() {
	mut collection := errors.new_error_collection()

	// Add errors
	syntax_err := errors.new_compilation_error(
		errors.SyntaxError{
			message:  'Test syntax error'
			expected: 'identifier'
			found:    'keyword'
		},
		ast.Position{line: 1, column: 1, filename: 'test.lx'},
		'Test syntax error'
	)

	type_err := errors.new_compilation_error(
		errors.TypeError{
			message:    'Test type error'
			expected:   'integer'
			actual:     'string'
			suggestion: 'convert types'
		},
		ast.Position{line: 2, column: 1, filename: 'test.lx'},
		'Test type error'
	)

	collection.add_error(syntax_err)
	collection.add_error(type_err)

	// Add warning
	warning := errors.new_compilation_error_with_severity(
		errors.SyntaxError{
			message:  'Test warning'
			expected: 'identifier'
			found:    'keyword'
		},
		ast.Position{line: 3, column: 1, filename: 'test.lx'},
		'Test warning',
		errors.ErrorSeverity.warning
	)

	collection.add_warning(warning)

	// Verify collection
	assert collection.has_errors() == true
	assert collection.has_warnings() == true
	assert collection.get_errors().len == 2
	assert collection.get_warnings().len == 1
	assert collection.get_all_errors().len == 3

	// Test clear
	collection.clear()
	assert collection.has_errors() == false
	assert collection.has_warnings() == false
	assert collection.get_errors().len == 0
	assert collection.get_warnings().len == 0

	println('✓ Error collection test passed')
}

fn test_improved_syntax_suggestions() {
	// Test floating point number error (the specific case mentioned)
	floating_point_err := errors.new_compilation_error(
		errors.SyntaxError{
			message:  'Expected simple expression: Got .'
			expected: 'simple expression'
			found:    '.'
		},
		ast.Position{line: 2, column: 3, filename: 'ex/ex1.lx'},
		'Expected simple expression: Got .'
	)

	floating_point_suggestions := errors.generate_suggestions(floating_point_err)
	assert floating_point_suggestions.len > 0
	assert floating_point_suggestions[0].contains('Floating point numbers must start with a digit')
	assert floating_point_suggestions[1].contains('Use 0.098 instead of .098')
	assert floating_point_suggestions[2].contains('Decimal numbers need a leading zero')

	// Test other common syntax errors
	comma_err := errors.new_compilation_error(
		errors.SyntaxError{
			message:  'Unexpected comma found'
			expected: 'operator'
			found:    ','
		},
		ast.Position{line: 1, column: 1, filename: 'test.lx'},
		'Unexpected comma found'
	)

	comma_suggestions := errors.generate_suggestions(comma_err)
	assert comma_suggestions.len > 0
	assert comma_suggestions[0].contains('Unexpected comma found')
	assert comma_suggestions[1].contains('semicolon')

	// Test identifier error
	identifier_err := errors.new_compilation_error(
		errors.SyntaxError{
			message:  'Expected identifier, found 123'
			expected: 'identifier'
			found:    '123'
		},
		ast.Position{line: 1, column: 1, filename: 'test.lx'},
		'Expected identifier, found 123'
	)

	identifier_suggestions := errors.generate_suggestions(identifier_err)
	assert identifier_suggestions.len > 0
	assert identifier_suggestions[0].contains('valid identifier')
	assert identifier_suggestions[1].contains('cannot start with a digit')

	// Test string error - when found is '@', it matches the '@' case
	string_err := errors.new_compilation_error(
		errors.SyntaxError{
			message:  'Expected string, found @'
			expected: 'string'
			found:    '@'
		},
		ast.Position{line: 1, column: 1, filename: 'test.lx'},
		'Expected string, found @'
	)

	string_suggestions := errors.generate_suggestions(string_err)
	assert string_suggestions.len > 0
	assert string_suggestions[0].contains('Unexpected @ symbol found')
	assert string_suggestions[1].contains('not a valid character')

	// Test operator error
	operator_err := errors.new_compilation_error(
		errors.SyntaxError{
			message:  'Expected operator, found &'
			expected: 'operator'
			found:    '&'
		},
		ast.Position{line: 1, column: 1, filename: 'test.lx'},
		'Expected operator, found &'
	)

	operator_suggestions := errors.generate_suggestions(operator_err)
	assert operator_suggestions.len > 0
	assert operator_suggestions.any(it.contains('andalso'))
	assert operator_suggestions.any(it.contains('&&'))

	// Test a case where found doesn't match any specific symbol
	generic_err := errors.new_compilation_error(
		errors.SyntaxError{
			message:  'Expected string, found invalid'
			expected: 'string'
			found:    'invalid'
		},
		ast.Position{line: 1, column: 1, filename: 'test.lx'},
		'Expected string, found invalid'
	)

	generic_suggestions := errors.generate_suggestions(generic_err)
	assert generic_suggestions.len > 0
	assert generic_suggestions[0].contains('double quotes')
	assert generic_suggestions[1].contains('Escape quotes')

	println('✓ Improved syntax suggestions test passed')
}

fn main() {
	test_error_formatting_with_categories()
	test_suggestion_generation()
	test_error_collection()
	test_improved_syntax_suggestions()
	println('All error formatting tests passed!')
}