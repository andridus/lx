module error

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
		else {
			[]
		}
	}
}

// generate_syntax_suggestions generates suggestions for syntax errors
fn generate_syntax_suggestions(expected string, found string) []string {
	mut suggestions := []string{}

	// Common syntax error suggestions
	if expected.len > 0 && found.len > 0 {
		suggestions << 'Expected ${expected}, but found ${found}'
		suggestions << 'Check the syntax rules for ${expected}'
	}

	// Specific suggestions based on expected token
	match expected {
		'identifier' {
			suggestions << 'Use a valid identifier (letters, digits, underscore)'
			suggestions << 'Identifiers cannot start with a digit'
		}
		'string' {
			suggestions << 'Use double quotes for strings: "hello"'
			suggestions << 'Escape quotes with backslash: "hello\\"world"'
		}
		'number' {
			suggestions << 'Use digits 0-9 for numbers'
			suggestions << 'Use decimal point for floats: 3.14'
		}
		'operator' {
			suggestions << 'Use valid operators: +, -, *, /, ==, !=, etc.'
		}
		else {
			suggestions << 'Review the syntax rules for ${expected}'
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

	return suggestions
}

// generate_record_suggestions generates suggestions for record errors
fn generate_record_suggestions(message string, record string, field string) []string {
	mut suggestions := []string{}

	// Common record field issues
	suggestions << 'Check if the record is defined'
	suggestions << 'Make sure the record ${record} is defined with field ${field}'
	suggestions << 'Check field name spelling'
	suggestions << 'Verify the field name is spelled correctly'

	return suggestions
}

// generate_guard_suggestions generates suggestions for guard errors
fn generate_guard_suggestions(message string, condition string) []string {
	mut suggestions := []string{}

	// Common guard expression issues
	if condition.contains('&&') {
		suggestions << 'Use comma to separate guard conditions'
		suggestions << 'In LX guards, use comma to separate conditions, not &&'
	}

	if condition.contains('||') {
		suggestions << 'Use semicolon to separate guard alternatives'
		suggestions << 'Use separate when clauses for alternatives'
	}

	if condition.contains('=') {
		suggestions << 'Use == for comparison in guards'
		suggestions << 'Use == for equality comparison in guard expressions'
	}

	return suggestions
}

// find_closest_match finds the closest string match using Levenshtein distance
fn find_closest_match(target string, candidates []string) string {
	if candidates.len == 0 {
		return ''
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
			// Manual min function
			min1 := if matrix[i - 1][j] + 1 < matrix[i][j - 1] + 1 {
				matrix[i - 1][j] + 1
			} else {
				matrix[i][j - 1] + 1
			}
			min2 := if min1 < matrix[i - 1][j - 1] + cost {
				min1
			} else {
				matrix[i - 1][j - 1] + cost
			}
			matrix[i][j] = min2
		}
	}

	return matrix[len1][len2]
}
