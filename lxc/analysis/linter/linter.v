module linter

import ast
import errors

// LinterResult represents the result of linting
pub struct LinterResult {
pub:
	success bool
	errors  []errors.CompilationError
}

// Linter represents the linter for LX language
pub struct Linter {}

// new_linter creates a new linter instance
pub fn new_linter() Linter {
	return Linter{}
}



// lint_module lints a module and returns any errors found
pub fn (l Linter) lint_module(module_stmt ast.ModuleStmt) LinterResult {
	mut lint_errors := []errors.CompilationError{}

	// Check function arity ordering
	arity_errors := l.check_function_arity_ordering(module_stmt.statements)
	lint_errors << arity_errors



	return LinterResult{
		success: lint_errors.len == 0
		errors:  lint_errors
	}
}

// check_function_arity_ordering checks if functions are ordered by arity (smallest to largest)
fn (l Linter) check_function_arity_ordering(statements []ast.Stmt) []errors.CompilationError {
	mut lint_errors := []errors.CompilationError{}
	mut function_arities := map[string][]int{} // function_name -> list of arities

	// Collect all function arities
	for stmt in statements {
		match stmt {
			ast.FunctionStmt {
				func_stmt := stmt as ast.FunctionStmt
				if func_stmt.name !in function_arities {
					function_arities[func_stmt.name] = []
				}

				for clause in func_stmt.clauses {
					arity := clause.parameters.len
					if arity !in function_arities[func_stmt.name] {
						function_arities[func_stmt.name] << arity
					}
				}
			}
			else {
				// Skip non-function statements
			}
		}
	}

	// Check ordering for each function
	for function_name, arities in function_arities {
		if arities.len > 1 {
			// Sort arities to check if they're already ordered
			mut sorted_arities := arities.clone()
			sorted_arities.sort()

			if arities != sorted_arities {
				// Functions are not ordered by arity
				arities_str := arities.map(it.str()).join(', ')
				error_msg := 'Function "${function_name}" has clauses with arities [${arities_str}] but they should be ordered from smallest to largest arity'
				syntax_error := errors.SyntaxError{
					message:  error_msg
					expected: 'Functions ordered by arity (smallest to largest)'
					found:    'Functions with arities [${arities_str}] in wrong order'
				}
				lint_errors << errors.new_compilation_error(syntax_error, ast.new_position(1,
					1, 'unknown'), error_msg)
			}
		}
	}

	return lint_errors
}
