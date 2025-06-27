module generate

import ast
import typechecker.types
import strings

// CodegenContext holds the state and configuration for code generation
pub struct CodegenContext {
pub mut:
	// Current scope and variable management
	scope_stack   []Scope
	current_scope &Scope = unsafe { nil }

	// Module information
	module_name string
	module_path string

	// Generated code buffer
	code_buffer strings.Builder

	// Configuration
	indent_level  int    = 0
	indent_string string = '  '

	// Error handling
	errors []CodegenError

	// Type information from typechecker
	type_context &types.Context = unsafe { nil }
}

// Scope represents a lexical scope with variable bindings
pub struct Scope {
pub mut:
	parent    &Scope = unsafe { nil }
	variables map[string]Variable
	depth     int
}

// Variable represents a variable in the current scope
pub struct Variable {
pub mut:
	name           string
	original_name  string // Original LX name
	generated_name string // Generated name in target language
	typ            types.Type
	is_mutable     bool
	scope_depth    int
}

// CodegenError represents an error during code generation
pub struct CodegenError {
pub mut:
	message  string
	location ast.Location
	severity ErrorSeverity
}

// ErrorSeverity indicates the severity of a codegen error
pub enum ErrorSeverity {
	error
	warning
	info
}

// new_context creates a new code generation context
pub fn new_context(module_name string, module_path string, type_ctx &types.Context) CodegenContext {
	mut ctx := CodegenContext{
		module_name:  module_name
		module_path:  module_path
		type_context: type_ctx
	}

	// Initialize with global scope
	global_scope := &Scope{
		parent:    unsafe { nil }
		variables: map[string]Variable{}
		depth:     0
	}

	ctx.scope_stack = [global_scope]
	ctx.current_scope = global_scope

	return ctx
}

// enter_scope creates a new nested scope
pub fn (mut ctx CodegenContext) enter_scope() {
	new_scope := &Scope{
		parent:    ctx.current_scope
		variables: map[string]Variable{}
		depth:     ctx.current_scope.depth + 1
	}

	ctx.scope_stack << new_scope
	ctx.current_scope = new_scope
}

// exit_scope exits the current scope and returns to parent
pub fn (mut ctx CodegenContext) exit_scope() {
	if ctx.scope_stack.len > 1 {
		ctx.scope_stack.delete_last()
		ctx.current_scope = ctx.scope_stack.last()
	}
}

// add_variable adds a variable to the current scope
pub fn (mut ctx CodegenContext) add_variable(name string, typ types.Type, is_mutable bool) {
	generated_name := ctx.generate_variable_name(name)

	variable := Variable{
		name:           name
		original_name:  name
		generated_name: generated_name
		typ:            typ
		is_mutable:     is_mutable
		scope_depth:    ctx.current_scope.depth
	}

	ctx.current_scope.variables[name] = variable
}

// find_variable looks for a variable in the current scope and parent scopes
pub fn (ctx CodegenContext) find_variable(name string) ?Variable {
	mut current := ctx.current_scope

	for current != unsafe { nil } {
		if variable := current.variables[name] {
			return variable
		}
		current = current.parent
	}

	return none
}

// generate_variable_name generates a target-language specific variable name
pub fn (ctx CodegenContext) generate_variable_name(lx_name string) string {
	// For Erlang, capitalize variable names
	// This is a simple implementation - can be enhanced for uniqueness
	if lx_name.len == 0 {
		return 'Var'
	}

	// Handle special cases
	match lx_name {
		'_' {
			return '_'
		}
		'__MODULE__' {
			return '?MODULE'
		}
		else {
			// Capitalize first letter for Erlang
			return lx_name[0].str().to_upper() + lx_name[1..]
		}
	}
}

// write adds text to the code buffer
pub fn (mut ctx CodegenContext) write(text string) {
	ctx.code_buffer.write_string(text)
}

// writeln adds text with newline to the code buffer
pub fn (mut ctx CodegenContext) writeln(text string) {
	ctx.write(text)
	ctx.write('\n')
}

// indent increases the current indent level
pub fn (mut ctx CodegenContext) indent() {
	ctx.indent_level++
}

// dedent decreases the current indent level
pub fn (mut ctx CodegenContext) dedent() {
	if ctx.indent_level > 0 {
		ctx.indent_level--
	}
}

// write_indent writes the current indent to the buffer
pub fn (mut ctx CodegenContext) write_indent() {
	for i := 0; i < ctx.indent_level; i++ {
		ctx.write(ctx.indent_string)
	}
}

// writeln_indent writes indented text with newline
pub fn (mut ctx CodegenContext) writeln_indent(text string) {
	ctx.write_indent()
	ctx.writeln(text)
}

// add_error adds an error to the context
pub fn (mut ctx CodegenContext) add_error(message string, location ast.Location, severity ErrorSeverity) {
	ctx.errors << CodegenError{
		message:  message
		location: location
		severity: severity
	}
}

// has_errors returns true if there are any errors
pub fn (ctx CodegenContext) has_errors() bool {
	return ctx.errors.len > 0
}

// get_code returns the generated code as a string
pub fn (ctx CodegenContext) get_code() string {
	return ctx.code_buffer.str()
}

// reset_code clears the code buffer
pub fn (mut ctx CodegenContext) reset_code() {
	ctx.code_buffer = strings.new_builder(1024)
}
