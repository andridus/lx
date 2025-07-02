module directives

import ast
import typechecker

// handle_reflection prints type information for functions
pub fn handle_reflection(func_stmt ast.FunctionStmt, tc &typechecker.TypeChecker) {
	println('=== REFLECTION INFO for function: ${func_stmt.name} ===')

	for i, clause in func_stmt.clauses {
		println('Clause ${i + 1}:')

		// Print parameter types
		println('  Parameters:')
		for j, param in clause.parameters {
			param_type := tc.infer_pattern_type(param)
			println('    ${j + 1}. ${param.str()} :: ${param_type.str()}')
		}

		// Print guard type
		guard_type := tc.infer_expression_type(clause.guard)
		println('  Guard: ${clause.guard.str()} :: ${guard_type.str()}')

		// Print body variable types
		println('  Body variables:')
		for stmt in clause.body {
			tc.print_statement_types(stmt, '    ')
		}
		println('')
	}
	println('=== END REFLECTION INFO ===')
}

// handle_inline marks function for inlining optimization
pub fn handle_inline(func_stmt ast.FunctionStmt, tc &typechecker.TypeChecker) {
	// TODO: Implement inlining logic
	println('Function ${func_stmt.name} marked for inlining')
}

// handle_deprecated marks function as deprecated
pub fn handle_deprecated(func_stmt ast.FunctionStmt, tc &typechecker.TypeChecker) {
	println('WARNING: Function ${func_stmt.name} is deprecated')
}
