module erlang

import ast
import analysis.typechecker
import codegen { CodegenResult }

// ErlangGenerator generates Erlang code from LX AST
pub struct ErlangGenerator {
mut:
	defined_types map[string]ast.TypeAliasStmt // Map of type name to type definition
	var_scopes    []map[string]string          // Stack of variable scopes: original name -> hashed name
	next_hash     int                       // Counter for unique hashes
	type_context  ?&typechecker.TypeContext // Type context for record type information
}

// new_erlang_generator creates a new Erlang code generator
pub fn new_erlang_generator() ErlangGenerator {
	return ErlangGenerator{
		defined_types: map[string]ast.TypeAliasStmt{}
		var_scopes:    [map[string]string{}]
		next_hash:     0
		type_context:  none
	}
}

// generate_module generates a complete Erlang module (implements CodeGenerator interface)
pub fn (mut gen ErlangGenerator) generate_module(module_stmt ast.ModuleStmt, type_ctx &typechecker.TypeContext) CodegenResult {
	// Store the type context for use in expression generation
	gen.type_context = unsafe { type_ctx }
	// Collect all type definitions first
	gen.collect_type_definitions(module_stmt.statements)

	// Generate module header
	mut code := gen.get_module_header(module_stmt.name)

	// Generate exports
	exports := gen.generate_exports(module_stmt.statements)
	if exports.len > 0 {
		code += '-export([${exports.join(', ')}]).\n'
	}
	code += '\n'

	// Generate statements
	for stmt in module_stmt.statements {
		stmt_code := gen.generate_statement(stmt)
		code += stmt_code + '\n'
	}

	// Add module footer
	code += gen.get_module_footer()

	return CodegenResult{
		success: true
		errors:  []
		code:    code
	}
}

// collect_type_definitions collects all type alias definitions from module statements
fn (mut gen ErlangGenerator) collect_type_definitions(statements []ast.Stmt) {
	for stmt in statements {
		match stmt {
			ast.TypeAliasStmt {
				gen.defined_types[stmt.name] = stmt
			}
			else {
				// Skip non-type statements
			}
		}
	}
}

// generate_exports generates export list from module statements
fn (gen ErlangGenerator) generate_exports(statements []ast.Stmt) []string {
	mut exports := []string{}
	mut seen_exports := map[string]bool{}

	for stmt in statements {
		match stmt {
			ast.FunctionStmt {
				// Only export public functions (not private ones)
				if !stmt.is_private {
					// Generate exports for all clauses with different arities
					for clause in stmt.clauses {
						param_count := clause.parameters.len
						export_name := '${stmt.name}/${param_count}'
						if !seen_exports[export_name] {
							exports << export_name
							seen_exports[export_name] = true
						}
					}
				}
			}
			else {
				// Skip non-function statements
			}
		}
	}

	return exports
}

// Interface implementations
pub fn (gen ErlangGenerator) get_file_extension() string {
	return '.erl'
}

pub fn (gen ErlangGenerator) get_module_header(module_name string) string {
	return '-module(${module_name}).\n'
}

pub fn (gen ErlangGenerator) get_module_footer() string {
	return ''
}

// Scope management for variable hashing
// Call this when entering a new block scope
fn (mut gen ErlangGenerator) enter_scope() {
	gen.var_scopes << map[string]string{}
}

// Call this when exiting a block scope
fn (mut gen ErlangGenerator) exit_scope() {
	if gen.var_scopes.len > 1 {
		gen.var_scopes.delete_last()
	}
}

// Generates and binds a unique hashed variable name for a new variable binding
// If is_param is true, do not add hash (for function parameters only)
fn (mut gen ErlangGenerator) bind_variable(name string, is_param bool) string {
	if is_param {
		// For function parameters, do not add hash
		simple_name := gen.capitalize_variable(name)
		gen.var_scopes.last()[name] = simple_name
		return simple_name
	} else {
		// For all other variables, always add hash
		hash := gen.generate_alphanumeric_hash()
		gen.next_hash++
		hashed := gen.capitalize_variable(name) + '_' + hash
		gen.var_scopes.last()[name] = hashed
		return hashed
	}
}

// generate_alphanumeric_hash generates a 4-character lowercase alphanumeric hash
fn (mut gen ErlangGenerator) generate_alphanumeric_hash() string {
	// Use a simple algorithm to generate consistent 4-char alphanumeric strings
	chars := 'abcdefghijklmnopqrstuvwxyz0123456789'
	mut hash := ''
	mut num := gen.next_hash

	for _ in 0 .. 4 {
		index := num % chars.len
		hash += chars[index].ascii_str()
		num = num / chars.len
	}

	return hash
}

// Looks up the hashed variable name for a given original name, searching from innermost to outermost scope
fn (gen ErlangGenerator) lookup_variable(name string) string {
	for i := gen.var_scopes.len - 1; i >= 0; i-- {
		if name in gen.var_scopes[i] {
			return gen.var_scopes[i][name]
		}
	}
	return gen.capitalize_variable(name) // fallback (should not happen)
}
