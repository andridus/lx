module erlang

import ast
import analysis.typechecker
import codegen { CodegenResult }

// ErlangGenerator generates Erlang code from LX AST
pub struct ErlangGenerator {}

// new_erlang_generator creates a new Erlang code generator
pub fn new_erlang_generator() ErlangGenerator {
	return ErlangGenerator{}
}

// generate_module generates a complete Erlang module (implements CodeGenerator interface)
pub fn (gen ErlangGenerator) generate_module(module_stmt ast.ModuleStmt, type_ctx typechecker.TypeContext) CodegenResult {
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
