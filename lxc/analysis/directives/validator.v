module directives

import ast
import errors

// DirectiveValidator validates directive usage
pub struct DirectiveValidator {
	registry DirectiveRegistry
}

// new_validator creates a new directive validator
pub fn new_validator() DirectiveValidator {
	return DirectiveValidator{
		registry: new_registry()
	}
}

// validate_function validates directives on a function
pub fn (v DirectiveValidator) validate_function(func_stmt ast.FunctionStmt) []errors.CompilationError {
	mut directive_errors := []errors.CompilationError{}

	for directive in func_stmt.directives {
		if !v.registry.is_valid(directive) {
			error_msg := 'Unknown directive: @${directive}'
			available_directives := v.registry.get_available_directives().join(', ')
			suggestion := 'Available directives: ${available_directives}'

			compilation_error := errors.new_compilation_error(
				errors.SyntaxError{
					message:  error_msg
					expected: suggestion
					found:    '@${directive}'
				},
				func_stmt.position,
				error_msg
			)

			directive_errors << compilation_error
		}
	}

	return directive_errors
}

// validate_module validates all directives in a module
pub fn (v DirectiveValidator) validate_module(module_stmt ast.ModuleStmt) []errors.CompilationError {
	mut all_errors := []errors.CompilationError{}

	for stmt in module_stmt.statements {
		if stmt is ast.FunctionStmt {
			func_stmt := stmt as ast.FunctionStmt
			directive_errors := v.validate_function(func_stmt)
			all_errors << directive_errors
		}
	}

	return all_errors
}