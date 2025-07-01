module directives

import ast
import typechecker

// DirectiveHandler represents a directive handler function
pub type DirectiveHandler = fn(ast.FunctionStmt, &typechecker.TypeChecker)

// DirectiveRegistry manages all available directives
pub struct DirectiveRegistry {
mut:
	handlers map[string]DirectiveHandler
}

// new_registry creates a new directive registry
pub fn new_registry() DirectiveRegistry {
	mut registry := DirectiveRegistry{
		handlers: map[string]DirectiveHandler{}
	}

	// Register built-in directives
	registry.register('reflection', handle_reflection)
	registry.register('inline', handle_inline)
	registry.register('deprecated', handle_deprecated)

	return registry
}

// register adds a new directive handler
pub fn (mut r DirectiveRegistry) register(name string, handler DirectiveHandler) {
	r.handlers[name] = handler
}

// is_valid checks if a directive is recognized
pub fn (r DirectiveRegistry) is_valid(name string) bool {
	return name in r.handlers
}

// get_handler returns the handler for a directive
pub fn (r DirectiveRegistry) get_handler(name string) ?DirectiveHandler {
	return r.handlers[name] or { none }
}

// get_available_directives returns a list of all available directive names
pub fn (r DirectiveRegistry) get_available_directives() []string {
	return r.handlers.keys()
}