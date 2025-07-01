module errors

import ast

// LexicalError represents a lexical error
pub struct LexicalError {
pub:
	message string
}

// SyntaxError represents a syntax error
pub struct SyntaxError {
pub:
	message  string
	expected string
	found    string
}

// TypeError represents a type error
pub struct TypeError {
pub:
	message    string
	expected   string
	actual     string
	suggestion string
}

// UnboundVariableError represents an unbound variable error
pub struct UnboundVariableError {
pub:
	variable   string
	similar    []string
	suggestion string
}

// PatternError represents a pattern matching error
pub struct PatternError {
pub:
	message   string
	pattern   string
	value     string
	suggestion string
}

// RecordError represents a record-related error
pub struct RecordError {
pub:
	message   string
	record_name string
	field_name string
	suggestion string
}

// BinaryError represents a binary/bitstring error
pub struct BinaryError {
pub:
	message   string
	expected_size int
	actual_size int
	suggestion string
}

// GuardError represents a guard expression error
pub struct GuardError {
pub:
	message   string
	expression string
	suggestion string
}

// DependencyError represents a module dependency error
pub struct DependencyError {
pub:
	message   string
	module_name string
	suggestion string
}

// ErrorKind represents different types of compilation errors using sum types
pub type ErrorKind = LexicalError | SyntaxError | TypeError | UnboundVariableError | PatternError | RecordError | BinaryError | GuardError | DependencyError

// str returns a string representation of ErrorKind
pub fn (e ErrorKind) str() string {
	return match e {
		LexicalError { 'LexicalError: ${e.message}' }
		SyntaxError { 'SyntaxError: ${e.message} (expected: ${e.expected}, found: ${e.found})' }
		TypeError { 'TypeError: ${e.message} (expected: ${e.expected}, actual: ${e.actual})' }
		UnboundVariableError { 'UnboundVariable: ${e.variable} (similar: ${e.similar.join(', ')})' }
		PatternError { 'PatternError: ${e.message} (pattern: ${e.pattern}, value: ${e.value})' }
		RecordError { 'RecordError: ${e.message} (record: ${e.record_name}, field: ${e.field_name})' }
		BinaryError { 'BinaryError: ${e.message} (expected: ${e.expected_size}, actual: ${e.actual_size})' }
		GuardError { 'GuardError: ${e.message} (expression: ${e.expression})' }
		DependencyError { 'DependencyError: ${e.message} (module: ${e.module_name})' }
	}
}

// get_error_category returns the category of an error
pub fn (e ErrorKind) get_error_category() string {
	return match e {
		LexicalError { 'Lexical' }
		SyntaxError { 'Syntax' }
		TypeError { 'Type' }
		UnboundVariableError { 'Variable' }
		PatternError { 'Pattern' }
		RecordError { 'Record' }
		BinaryError { 'Binary' }
		GuardError { 'Guard' }
		DependencyError { 'Dependency' }
	}
}

// ErrorSeverity represents the severity of an error
pub enum ErrorSeverity {
	info
	warning
	error
	fatal
}

// CompilationError represents a compilation error
pub struct CompilationError {
pub:
	kind     ErrorKind
	position ast.Position
	message  string
	severity ErrorSeverity
}

// new_compilation_error creates a new compilation error with default severity
pub fn new_compilation_error(kind ErrorKind, position ast.Position, message string) CompilationError {
	return CompilationError{
		kind:     kind
		position: position
		message:  message
		severity: .error
	}
}

// new_compilation_error_with_severity creates a new compilation error with specified severity
pub fn new_compilation_error_with_severity(kind ErrorKind, position ast.Position, message string, severity ErrorSeverity) CompilationError {
	return CompilationError{
		kind:     kind
		position: position
		message:  message
		severity: severity
	}
}

// ErrorCollection represents a collection of compilation errors
pub struct ErrorCollection {
mut:
	errors   []CompilationError
	warnings []CompilationError
}

// new_error_collection creates a new error collection
pub fn new_error_collection() ErrorCollection {
	return ErrorCollection{
		errors:   []
		warnings: []
	}
}

// add_error adds an error to the collection
pub fn (mut ec ErrorCollection) add_error(err CompilationError) {
	ec.errors << err
}

// add_warning adds a warning to the collection
pub fn (mut ec ErrorCollection) add_warning(err CompilationError) {
	ec.warnings << err
}

// has_errors checks if the collection has any errors
pub fn (ec ErrorCollection) has_errors() bool {
	return ec.errors.len > 0
}

// has_warnings checks if the collection has any warnings
pub fn (ec ErrorCollection) has_warnings() bool {
	return ec.warnings.len > 0
}

// has_fatal_errors checks if the collection has any fatal errors
pub fn (ec ErrorCollection) has_fatal_errors() bool {
	for error in ec.errors {
		if error.severity == .fatal {
			return true
		}
	}
	return false
}

// get_all_errors returns all errors and warnings
pub fn (ec ErrorCollection) get_all_errors() []CompilationError {
	mut all := []CompilationError{}
	all << ec.errors
	all << ec.warnings
	return all
}

// get_errors returns only errors (not warnings)
pub fn (ec ErrorCollection) get_errors() []CompilationError {
	return ec.errors
}

// get_warnings returns only warnings
pub fn (ec ErrorCollection) get_warnings() []CompilationError {
	return ec.warnings
}

// clear clears all errors and warnings
pub fn (mut ec ErrorCollection) clear() {
	ec.errors.clear()
	ec.warnings.clear()
}
