module errors

// Suggestion represents a suggestion for fixing an error
pub struct Suggestion {
pub:
	message     string
	code        string
	explanation string
	priority    int // Higher priority = more important
}

// new_suggestion creates a new Suggestion
pub fn new_suggestion(message string, code string, explanation string) Suggestion {
	return Suggestion{
		message:     message
		code:        code
		explanation: explanation
		priority:    1
	}
}

// new_suggestion_with_priority creates a new Suggestion with custom priority
pub fn new_suggestion_with_priority(message string, code string, explanation string, priority int) Suggestion {
	return Suggestion{
		message:     message
		code:        code
		explanation: explanation
		priority:    priority
	}
}

// generate_suggestions generates suggestions for fixing an error
pub fn generate_suggestions(err CompilationError) []string {
	return match err.kind {
		SyntaxError {
			generate_syntax_suggestions(err.kind.expected, err.kind.found)
		}
		TypeError {
			generate_type_suggestions(err.kind.expected, err.kind.actual)
		}
		UnboundVariableError {
			generate_unbound_variable_suggestions(err.kind.variable, err.kind.similar)
		}
		PatternError {
			generate_pattern_suggestions(err.kind.message, err.kind.pattern, err.kind.value)
		}
		RecordError {
			generate_record_suggestions(err.kind.message, err.kind.record_name, err.kind.field_name)
		}
		BinaryError {
			generate_binary_suggestions(err.kind.message, err.kind.expected_size, err.kind.actual_size)
		}
		GuardError {
			generate_guard_suggestions(err.kind.message, err.kind.expression)
		}
		DependencyError {
			generate_dependency_suggestions(err.kind.message, err.kind.module_name)
		}
		UnboundFunctionError {
			if err.kind.suggestion.len > 0 {
				[err.kind.suggestion]
			} else {
				[
					'Define the function ${err.kind.function}/${err.kind.arity} as needed.',
				]
			}
		}
		else {
			[]string{}
		}
	}
}

// generate_syntax_suggestions generates suggestions for syntax errors
fn generate_syntax_suggestions(expected string, found string) []string {
	mut suggestions := []string{}

	// Specific suggestions based on found token
	match found {
		'.' {
			// Common cases where a dot is found unexpectedly
			if expected == 'simple expression' {
				suggestions << 'Floating point numbers must start with a digit, not a decimal point'
				suggestions << 'Use 0.098 instead of .098'
				suggestions << 'Decimal numbers need a leading zero: 0.5, 0.123, etc.'
			} else {
				suggestions << 'Unexpected decimal point (.) found'
				suggestions << 'Check if you meant to write a floating point number'
				suggestions << 'Floating point numbers must start with a digit: 0.5, 1.23, etc.'
			}
		}
		',' {
			suggestions << 'Unexpected comma found'
			suggestions << 'Check if you meant to use a semicolon (;) to separate statements'
			suggestions << 'Or use a comma to separate function arguments'
		}
		';' {
			suggestions << 'Unexpected semicolon found'
			suggestions << 'Check if you meant to use a comma (,) to separate function arguments'
			suggestions << 'Or use a semicolon to separate guard clauses'
		}
		'(' {
			suggestions << 'Unexpected opening parenthesis found'
			suggestions << 'Check if you meant to call a function'
			suggestions << 'Or check for missing closing parenthesis in previous expression'
		}
		')' {
			suggestions << 'Unexpected closing parenthesis found'
			suggestions << 'Check for missing opening parenthesis'
			suggestions << 'Or check if you have an extra closing parenthesis'
		}
		'{' {
			suggestions << 'Unexpected opening brace found'
			suggestions << 'Check if you meant to create a tuple or record'
			suggestions << 'Or check for missing closing brace in previous expression'
		}
		'}' {
			suggestions << 'Unexpected closing brace found'
			suggestions << 'Check for missing opening brace'
			suggestions << 'Or check if you have an extra closing brace'
		}
		'[' {
			suggestions << 'Unexpected opening bracket found'
			suggestions << 'Check if you meant to create a list or access a map'
			suggestions << 'Or check for missing closing bracket in previous expression'
		}
		']' {
			suggestions << 'Unexpected closing bracket found'
			suggestions << 'Check for missing opening bracket'
			suggestions << 'Or check if you have an extra closing bracket'
		}
		'=' {
			suggestions << 'Unexpected assignment operator found'
			suggestions << 'Check if you meant to use == for comparison'
			suggestions << 'Or check if you have an extra = sign'
		}
		'==' {
			suggestions << 'Unexpected equality operator found'
			suggestions << 'Check if you meant to use = for assignment'
			suggestions << 'Or check if you have an extra = sign'
		}
		'!' {
			suggestions << 'Unexpected exclamation mark found'
			suggestions << 'Check if you meant to use the send operator (!) for message passing'
			suggestions << 'Or check if you meant to use != for inequality comparison'
		}
		'@' {
			suggestions << 'Unexpected @ symbol found'
			suggestions << '@ is not a valid character in Lx syntax'
			suggestions << 'Check if you meant to use : for atoms or module calls'
		}
		'#' {
			suggestions << 'Unexpected # symbol found'
			suggestions << '# is used for comments in Lx'
			suggestions << 'Check if you meant to use : for atoms or module calls'
		}
		'$' {
			suggestions << 'Unexpected $ symbol found'
			suggestions << '$ is not a valid character in Lx syntax'
			suggestions << 'Check if you meant to use a different symbol'
		}
		'%' {
			suggestions << 'Unexpected % symbol found'
			suggestions << '% is used for map creation in Lx: %{key: value}'
			suggestions << 'Check if you meant to create a map or use a different symbol'
		}
		'^' {
			suggestions << 'Unexpected ^ symbol found'
			suggestions << '^ is not a valid character in Lx syntax'
			suggestions << 'Check if you meant to use a different symbol'
		}
		'&' {
			suggestions << 'Unexpected & symbol found'
			suggestions << 'Use "and" or "andalso" for logical AND operations'
			suggestions << 'Check if you meant to use && (which becomes andalso in Erlang)'
		}
		'|' {
			suggestions << 'Unexpected | symbol found'
			suggestions << '| is used for record updates: {record | field: value}'
			suggestions << 'Use "or" or "orelse" for logical OR operations'
		}
		'\\' {
			suggestions << 'Unexpected backslash found'
			suggestions << 'Backslash is used for escape sequences in strings'
			suggestions << 'Check if you meant to use / for division'
		}
		'`' {
			suggestions << 'Unexpected backtick found'
			suggestions << 'Backticks are not used in Lx syntax'
			suggestions << 'Check if you meant to use single quotes or double quotes'
		}
		'~' {
			suggestions << 'Unexpected tilde found'
			suggestions << '~ is not a valid character in Lx syntax'
			suggestions << 'Check if you meant to use a different symbol'
		}
		else {
			// Specific suggestions based on expected token
			match expected {
				'identifier' {
					suggestions << 'Use a valid identifier (letters, digits, underscore)'
					suggestions << 'Identifiers cannot start with a digit'
					suggestions << 'Valid examples: name, user_id, _unused'
				}
				'string' {
					suggestions << 'Use double quotes for strings: "hello"'
					suggestions << 'Escape quotes with backslash: "hello\\"world"'
					suggestions << 'Strings must contain only printable ASCII characters'
				}
				'number' {
					suggestions << 'Use digits 0-9 for numbers'
					suggestions << 'Use decimal point for floats: 3.14'
					suggestions << 'Floating point numbers must start with a digit: 0.5, not .5'
				}
				'operator' {
					suggestions << 'Use valid operators: +, -, *, /, ==, !=, <, >, <=, >='
					suggestions << 'Logical operators: and, or, not, andalso, orelse'
					suggestions << 'Message passing: ! (send operator)'
				}
				'simple expression' {
					suggestions << 'Expected a valid expression (number, string, variable, function call)'
					suggestions << 'Check for missing operands or invalid syntax'
					suggestions << 'Valid expressions: 42, "hello", x, func(1, 2)'
				}
				'keyword' {
					suggestions << 'Expected a keyword like def, do, end, case, if, etc.'
					suggestions << 'Check if you have a typo in the keyword'
					suggestions << 'Keywords are reserved and cannot be used as identifiers'
				}
				'punctuation' {
					suggestions << 'Expected punctuation like (, ), {, }, [, ], ,, ;'
					suggestions << 'Check for missing or extra punctuation marks'
					suggestions << 'Ensure proper nesting of parentheses, braces, and brackets'
				}
				'( for single-clause function or do for multi-clause function' {
					suggestions << 'For single-clause functions, add parentheses: def func() do ... end'
					suggestions << 'For multi-clause functions, add at least one clause with parameters:'
					suggestions << '  def func do\n    (x) -> x + 1\n    (y) -> y * 2\n  end'
					suggestions << 'Multi-clause functions require at least one clause with parameters.'
				}
				else {
					suggestions << 'Expected ${expected}, but found ${found}'
					suggestions << 'Check the syntax rules for ${expected}'
					suggestions << 'Review the Lx syntax reference for valid constructs'
				}
			}
		}
	}

	return suggestions
}

// generate_type_suggestions generates suggestions for type errors
fn generate_type_suggestions(expected string, actual string) []string {
	mut suggestions := []string{}

	// Common type error suggestions
	suggestions << 'Type mismatch: expected ${expected}, got ${actual}'

	// Specific suggestions based on type conversion
	match expected {
		'integer' {
			match actual {
				'string' { suggestions << 'Convert string to integer: string_to_int("123")' }
				'float' { suggestions << 'Convert float to integer: int(3.14)' }
				else { suggestions << 'Ensure the expression evaluates to integer type' }
			}
		}
		'string' {
			match actual {
				'integer' { suggestions << 'Convert integer to string: int(123).str()' }
				'float' { suggestions << 'Convert float to string: float(3.14).str()' }
				else { suggestions << 'Ensure the expression evaluates to string type' }
			}
		}
		'float' {
			match actual {
				'integer' { suggestions << 'Convert integer to float: f64(123)' }
				'string' { suggestions << 'Convert string to float: string_to_float("3.14")' }
				else { suggestions << 'Ensure the expression evaluates to float type' }
			}
		}
		else {
			suggestions << 'Ensure the expression evaluates to ${expected} type'
		}
	}

	return suggestions
}

// generate_unbound_variable_suggestions generates suggestions for unbound variable errors
fn generate_unbound_variable_suggestions(variable string, similar []string) []string {
	mut suggestions := []string{}

	// Suggest similar variable names
	if similar.len > 0 {
		closest := find_closest_match(variable, similar)
		suggestions << 'Did you mean ${closest}?'
		suggestions << 'Check if you meant to use ${closest} instead of ${variable}'
	}

	// Suggest common variable name patterns
	if variable.ends_with('s') {
		singular := variable[..variable.len - 1]
		suggestions << 'Consider using the singular form ${singular}'
	}

	// Suggest common variable names
	common_vars := ['x', 'y', 'z', 'i', 'j', 'k', 'n', 'm', 'result', 'value', 'item', 'element']
	for common in common_vars {
		if variable.contains(common) {
			suggestions << 'Consider using a more descriptive name'
			break
		}
	}

	return suggestions
}

// generate_undefined_function_suggestions generates suggestions for undefined function errors
fn generate_undefined_function_suggestions(function string, mod_name string, similar_names []string) []string {
	mut suggestions := []string{}

	// Suggest similar function names
	if similar_names.len > 0 {
		closest := find_closest_match(function, similar_names)
		suggestions << 'Did you mean ${closest}?'
		suggestions << 'Check if you meant to call ${closest} instead of ${function}'
	}

	// Suggest common function patterns
	if function.starts_with('is_') {
		suggestions << 'Check if the function exists in the module'
		suggestions << 'Make sure the function is exported from the module'
	}

	// Suggest import if module is specified
	if mod_name.len > 0 {
		suggestions << 'Import the required module'
		suggestions << 'Add an import statement for the ${mod_name} module'
	}

	return suggestions
}

// generate_pattern_suggestions generates suggestions for pattern matching errors
fn generate_pattern_suggestions(message string, pattern string, value string) []string {
	mut suggestions := []string{}

	// Common pattern matching issues
	if pattern.contains('_') && !value.contains('_') {
		suggestions << 'Use wildcard pattern for any value'
		suggestions << 'The underscore _ matches any value in pattern matching'
	}

	if pattern.contains('[') && value.contains('{') {
		suggestions << 'Use tuple pattern for tuple values'
		suggestions << 'Use curly braces for tuple patterns'
	}

	if pattern.contains('{') && value.contains('[') {
		suggestions << 'Use list pattern for list values'
		suggestions << 'Use square brackets for list patterns'
	}

	// Field missing in map pattern
	if message.contains('field') && message.contains('not found') {
		suggestions << 'Check if the map contains the required field'
		suggestions << 'Use a simpler pattern that only matches existing fields'
		suggestions << 'Or use unsafe to bypass validation: unsafe ${pattern} <- ${value}'
		suggestions << 'Or provide a default value for missing fields'
	}

	return suggestions
}

// generate_record_suggestions generates suggestions for record errors
fn generate_record_suggestions(message string, record_name string, field_name string) []string {
	mut suggestions := []string{}

	// Missing required field
	if message.contains('required') && message.contains('not provided') {
		suggestions << 'Add the missing ${field_name} field to the ${record_name} record'
		suggestions << 'Provide a value for the ${field_name} field'
		suggestions << 'Or make the field optional in the record definition'
	}

	// Unknown field
	if message.contains('unknown field') {
		suggestions << 'Check the ${record_name} record definition for valid field names'
		suggestions << 'Remove the unknown field ${field_name}'
		suggestions << 'Or add the field to the record definition'
	}

	// Type mismatch in field
	if message.contains('type mismatch') {
		suggestions << 'Ensure the ${field_name} field has the correct type'
		suggestions << 'Check the record definition for the expected type'
	}

	return suggestions
}

// generate_binary_suggestions generates suggestions for binary errors
fn generate_binary_suggestions(message string, expected_size int, actual_size int) []string {
	mut suggestions := []string{}

	// Size mismatch
	if message.contains('size mismatch') {
		suggestions << 'Ensure the binary contains at least ${expected_size} bits before the variable-length data'
		suggestions << 'Check binary size first: if byte_size(binary) >= ${expected_size / 8} do'
		suggestions << 'Or use a safer pattern with size validation'
		suggestions << 'Consider using a case statement with size guards'
	}

	// Invalid format
	if message.contains('invalid format') {
		suggestions << 'Verify the binary pattern matches the actual data structure'
		suggestions << 'Check if the binary contains the expected fields in the correct order'
		suggestions << 'Use pattern matching with error handling'
	}

	return suggestions
}

// generate_guard_suggestions generates suggestions for guard errors
fn generate_guard_suggestions(message string, expression string) []string {
	mut suggestions := []string{}

	// Invalid operator in guard
	if message.contains('operator not allowed') {
		suggestions << 'Guards must be pure boolean expressions'
		suggestions << 'Move complex logic to the function body'
		suggestions << 'Use only allowed operators: ==, !=, >, <, >=, <=, andalso, orelse'
	}

	// Non-boolean expression
	if message.contains('non-boolean') {
		suggestions << 'Ensure the guard expression evaluates to a boolean value'
		suggestions << 'Use comparison operators or boolean functions'
	}

	// Function call in guard
	if message.contains('function call') {
		suggestions << 'Only built-in functions are allowed in guards'
		suggestions << 'Move the function call to the function body'
		suggestions << 'Use a simpler guard condition'
	}

	return suggestions
}

// generate_dependency_suggestions generates suggestions for dependency errors
fn generate_dependency_suggestions(message string, module_name string) []string {
	mut suggestions := []string{}

	// Module not found
	if message.contains('not found') {
		suggestions << 'Add ${module_name} to your deps list: deps [:erlang, :${module_name}]'
		suggestions << 'Or add to your lx.config: project { deps [:erlang, :${module_name}, :stdlib] }'
		suggestions << 'Check if the module name is spelled correctly'
		suggestions << 'Or use an alternative approach that doesn\'t require ${module_name}'
	}

	// Version conflict
	if message.contains('version') {
		suggestions << 'Check for version conflicts in your dependencies'
		suggestions << 'Update the module version in your deps list'
		suggestions << 'Or use a compatible version of the module'
	}

	return suggestions
}

// find_closest_match finds the closest match in a list of strings
fn find_closest_match(target string, candidates []string) string {
	if candidates.len == 0 {
		return target
	}

	mut closest := candidates[0]
	mut min_distance := levenshtein_distance(target, closest)

	for candidate in candidates {
		distance := levenshtein_distance(target, candidate)
		if distance < min_distance {
			min_distance = distance
			closest = candidate
		}
	}

	return closest
}

// levenshtein_distance calculates the Levenshtein distance between two strings
fn levenshtein_distance(s1 string, s2 string) int {
	len1 := s1.len
	len2 := s2.len

	if len1 == 0 {
		return len2
	}
	if len2 == 0 {
		return len1
	}

	mut matrix := [][]int{len: len1 + 1, init: []int{len: len2 + 1}}

	for i := 0; i <= len1; i++ {
		matrix[i][0] = i
	}

	for j := 0; j <= len2; j++ {
		matrix[0][j] = j
	}

	for i := 1; i <= len1; i++ {
		for j := 1; j <= len2; j++ {
			cost := if s1[i - 1] == s2[j - 1] { 0 } else { 1 }
			matrix[i][j] = min_of_three(matrix[i - 1][j] + 1, // deletion
			 matrix[i][j - 1] + 1, // insertion
			 matrix[i - 1][j - 1] + cost // substitution
			 )
		}
	}

	return matrix[len1][len2]
}

// min_of_three returns the minimum of three integers
fn min_of_three(a int, b int, c int) int {
	return if a < b {
		if a < c {
			a
		} else {
			c
		}
	} else {
		if b < c {
			b
		} else {
			c
		}
	}
}
