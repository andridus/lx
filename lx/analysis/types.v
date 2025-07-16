module analysis

import errors

// TypeCheckResult represents the result of type checking
pub struct TypeCheckResult {
pub:
	context  &TypeContext
	errors   []errors.CompilationError
	warnings []errors.CompilationError
}
