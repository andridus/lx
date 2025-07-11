module analysis1

import ast
import errors

pub struct Manager {
pub mut:
	registry  DirectiveRegistry
	validator DirectiveValidator
}

pub fn new_manager() Manager {
	return Manager{
		registry:  new_directive_registry()
		validator: new_directive_validator()
	}
}

pub fn (mut dm Manager) process_directives(module_stmt ast.ModuleStmt) []errors.CompilationError {
	mut all_errors := []errors.CompilationError{}

	// First, validate all directives
	validation_errors := dm.validator.validate_module(module_stmt)
	all_errors << validation_errors

	// If validation fails, return early
	if validation_errors.len > 0 {
		return all_errors
	}

	// Process valid directives
	for stmt in module_stmt.statements {
		if stmt is ast.FunctionStmt {
			func_stmt := stmt as ast.FunctionStmt
			directive_errors := dm.process_function_directives(func_stmt)
			all_errors << directive_errors
		}
	}

	return all_errors
}

fn (mut dm Manager) process_function_directives(func_stmt ast.FunctionStmt) []errors.CompilationError {
	mut directive_errors := []errors.CompilationError{}

	for directive in func_stmt.directives {
		directive_name := dm.normalize_directive_name(directive)

		if handler := dm.registry.get_handler(directive_name) {
			// Execute the directive handler
			handler(func_stmt)
		} else {
			// This should not happen since validation should catch this
			error_msg := 'Unknown directive: @${directive}'
			available_directives := dm.registry.get_available_directives().join(', ')
			suggestion := 'Available directives: ${available_directives}'

			compilation_error := errors.new_compilation_error(errors.SyntaxError{
				message:  error_msg
				expected: suggestion
				found:    '@${directive}'
			}, func_stmt.position, error_msg)

			directive_errors << compilation_error
		}
	}

	return directive_errors
}

fn (dm &Manager) normalize_directive_name(directive string) string {
	return if directive.starts_with('@') {
		directive[1..]
	} else {
		directive
	}
}

pub struct DirectiveRegistry {
pub mut:
	handlers map[string]DirectiveHandler
}

pub type DirectiveHandler = fn (ast.FunctionStmt)

pub fn new_directive_registry() DirectiveRegistry {
	mut registry := DirectiveRegistry{
		handlers: map[string]DirectiveHandler{}
	}

	// Register built-in directives
	registry.register_handler('reflection', handle_reflection_directive)
	registry.register_handler('inline', handle_inline_directive)
	registry.register_handler('deprecated', handle_deprecated_directive)

	return registry
}

pub fn (mut dr DirectiveRegistry) register_handler(name string, handler DirectiveHandler) {
	dr.handlers[name] = handler
}

pub fn (dr &DirectiveRegistry) get_handler(name string) ?DirectiveHandler {
	return if name in dr.handlers {
		dr.handlers[name]
	} else {
		none
	}
}

pub fn (dr &DirectiveRegistry) get_available_directives() []string {
	return dr.handlers.keys()
}

// Directive handlers
fn handle_reflection_directive(func_stmt ast.FunctionStmt) {
	// Implementation for @reflection directive
	// This could enable reflection capabilities for the function
}

fn handle_inline_directive(func_stmt ast.FunctionStmt) {
	// Implementation for @inline directive
	// This could mark the function for inlining during optimization
}

fn handle_deprecated_directive(func_stmt ast.FunctionStmt) {
	// Implementation for @deprecated directive
	// This could mark the function as deprecated
}

pub struct DirectiveValidator {
pub mut:
	valid_directives []string
}

pub fn new_directive_validator() DirectiveValidator {
	return DirectiveValidator{
		valid_directives: ['reflection', 'inline', 'deprecated']
	}
}

pub fn (dv &DirectiveValidator) validate_module(module_stmt ast.ModuleStmt) []errors.CompilationError {
	mut directive_errors := []errors.CompilationError{}

	for stmt in module_stmt.statements {
		if stmt is ast.FunctionStmt {
			func_stmt := stmt as ast.FunctionStmt
			for directive in func_stmt.directives {
				normalized_directive := if directive.starts_with('@') {
					directive[1..]
				} else {
					directive
				}

				if !dv.valid_directives.contains(normalized_directive) {
					error_msg := 'Unknown directive: @${directive}'
					suggestion := 'Available directives: ${dv.valid_directives.join(', ')}'

					compilation_error := errors.new_compilation_error(errors.SyntaxError{
						message:  error_msg
						expected: suggestion
						found:    '@${directive}'
					}, func_stmt.position, error_msg)

					directive_errors << compilation_error
				}
			}
		}
	}

	return directive_errors
}
