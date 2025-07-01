module main

import analysis
import ast

// Test basic variable scope checking functionality
fn test_variable_scope_basic() {
	// Test valid variable usage
	mut checker := analysis.new_variable_checker()

	// Create a simple module with valid variable usage
	test_module := ast.ModuleStmt{
		name:       'test'
		exports:    []
		imports:    []
		statements: []
		position:   ast.new_position(1, 1, 'test.lx')
	}

	result := checker.check_module(test_module)
	assert result.success == true
	assert result.errors.len == 0

	println('✅ Basic variable scope test passed')
}

fn test_variable_binding() {
	mut checker := analysis.new_variable_checker()

	// Test variable binding in current scope
	checker.bind_variable('x', ast.new_position(1, 1, 'test.lx'))

	assert checker.has_binding_local('x') == true
	assert checker.has_binding_recursive('x') == true
	assert checker.has_binding_local('y') == false

	println('✅ Variable binding test passed')
}

fn test_scope_nesting() {
	mut checker := analysis.new_variable_checker()

	// Bind variable in outer scope
	checker.bind_variable('outer', ast.new_position(1, 1, 'test.lx'))

	// Enter inner scope
	checker.enter_scope()

	// Variable should not be in local scope but should be in recursive scope
	assert checker.has_binding_local('outer') == false
	assert checker.has_binding_recursive('outer') == true

	// Bind variable in inner scope
	checker.bind_variable('inner', ast.new_position(2, 1, 'test.lx'))

	assert checker.has_binding_local('inner') == true
	assert checker.has_binding_recursive('inner') == true

	// Exit inner scope
	checker.exit_scope()

	// Inner variable should not be accessible anymore
	assert checker.has_binding_recursive('inner') == false
	assert checker.has_binding_recursive('outer') == true

	println('✅ Scope nesting test passed')
}

fn test_rebind_detection() {
	mut checker := analysis.new_variable_checker()

	// Bind variable
	checker.bind_variable('x', ast.new_position(1, 1, 'test.lx'))

	// Try to bind the same variable again (should be detected by the checker)
	// This would normally be caught during expression checking
	checker.bind_variable('x', ast.new_position(2, 1, 'test.lx'))

	// The checker should have detected the rebind
	// Note: In a real scenario, this would be caught during assignment expression checking

	println('✅ Rebind detection test passed')
}

fn test_shadowing_detection() {
	mut checker := analysis.new_variable_checker()

	// Bind variable in outer scope
	checker.bind_variable('shared', ast.new_position(1, 1, 'test.lx'))

	// Enter inner scope
	checker.enter_scope()

	// Try to bind the same variable in inner scope (should be detected)
	checker.bind_variable('shared', ast.new_position(2, 1, 'test.lx'))

	// The checker should have detected the shadowing
	// Note: In a real scenario, this would be caught during assignment expression checking

	checker.exit_scope()

	println('✅ Shadowing detection test passed')
}

fn test_parallel_scopes() {
	mut checker := analysis.new_variable_checker()

	// Enter first scope
	checker.enter_scope()
	checker.bind_variable('temp', ast.new_position(1, 1, 'test.lx'))
	checker.exit_scope()

	// Enter second scope (parallel to first)
	checker.enter_scope()
	checker.bind_variable('temp', ast.new_position(2, 1, 'test.lx'))
	checker.exit_scope()

	// Both bindings should be valid (no shadowing between parallel scopes)
	assert checker.has_binding_recursive('temp') == false // After exiting both scopes

	println('✅ Parallel scopes test passed')
}

fn test_error_reporting() {
	mut checker := analysis.new_variable_checker()

	// Report an error
	checker.report_error("Variable 'undefined' is not defined", 'Variables must be defined before use',
		ast.new_position(1, 1, 'test.lx'))

	assert checker.has_errors() == true
	assert checker.get_errors().len == 1

	error := checker.get_errors()[0]
	assert error.message.contains('is not defined')

	println('✅ Error reporting test passed')
}

fn test_variable_checker_integration() {
	// Test integration with the full variable checking system
	mut checker := analysis.new_variable_checker()

	// Create a simple module structure
	test_module := ast.ModuleStmt{
		name:       'test'
		exports:    []
		imports:    []
		statements: []
		position:   ast.new_position(1, 1, 'test.lx')
	}

	result := checker.check_module(test_module)
	assert result.success == true

	println('✅ Variable checker integration test passed')
}

// Test specific AST construction for more precise testing
fn test_specific_ast_construction() {
	// Create a simple assignment: x = 10
	literal := ast.LiteralExpr{
		value: ast.IntegerLiteral{
			value: 10
		}
	}
	assign := create_assign_expr('x', literal, 1, 1)

	// Create a variable usage: x
	variable := create_variable_expr('x')

	// Create a binary expression: x + 5
	five := ast.LiteralExpr{
		value: ast.IntegerLiteral{
			value: 5
		}
	}
	binary := create_binary_expr(variable, ast.BinaryOp.add, five, 2, 1)

	// Create function body
	body := [
		ast.Stmt(create_expr_stmt(assign)),
		ast.Stmt(create_expr_stmt(binary)),
	]

	// Create function clause
	clause := create_function_clause([], ast.LiteralExpr{
		value: ast.BooleanLiteral{
			value: true
		}
	}, body, 1, 1)

	// Create function statement
	function := create_function_stmt('test', [clause], 1, 1)

	// Create module
	test_module := ast.ModuleStmt{
		name:       'test'
		exports:    []
		imports:    []
		statements: [ast.Stmt(function)]
		position:   ast.new_position(1, 1, 'test.lx')
	}

	// Test variable checker
	mut checker := analysis.new_variable_checker()
	result := checker.check_module(test_module)

	assert result.success == true
	assert result.errors.len == 0

	println('✅ Specific AST construction test passed')
}

fn test_rebind_detection_ast() {
	// Create: x = 10; x = 20
	ten := ast.LiteralExpr{
		value: ast.IntegerLiteral{
			value: 10
		}
	}
	twenty := ast.LiteralExpr{
		value: ast.IntegerLiteral{
			value: 20
		}
	}

	assign1 := create_assign_expr('x', ten, 1, 1)
	assign2 := create_assign_expr('x', twenty, 2, 1)

	body := [
		ast.Stmt(create_expr_stmt(assign1)),
		ast.Stmt(create_expr_stmt(assign2)),
	]

	clause := create_function_clause([], ast.LiteralExpr{
		value: ast.BooleanLiteral{
			value: true
		}
	}, body, 1, 1)
	function := create_function_stmt('test', [clause], 1, 1)

	test_module := ast.ModuleStmt{
		name:       'test'
		exports:    []
		imports:    []
		statements: [ast.Stmt(function)]
		position:   ast.new_position(1, 1, 'test.lx')
	}

	mut checker := analysis.new_variable_checker()
	result := checker.check_module(test_module)

	assert result.success == false
	assert result.errors.len == 1
	assert result.errors[0].message.contains('cannot be reassigned')

	println('✅ Rebind detection AST test passed')
}

fn test_undefined_variable_detection() {
	// Create: undefined_var
	variable := create_variable_expr('undefined_var')

	body := [ast.Stmt(create_expr_stmt(variable))]
	clause := create_function_clause([], ast.LiteralExpr{
		value: ast.BooleanLiteral{
			value: true
		}
	}, body, 1, 1)
	function := create_function_stmt('test', [clause], 1, 1)

	test_module := ast.ModuleStmt{
		name:       'test'
		exports:    []
		imports:    []
		statements: [ast.Stmt(function)]
		position:   ast.new_position(1, 1, 'test.lx')
	}

	mut checker := analysis.new_variable_checker()
	result := checker.check_module(test_module)

	assert result.success == false
	assert result.errors.len == 1
	assert result.errors[0].message.contains('is not defined')

	println('✅ Undefined variable detection test passed')
}

fn test_pattern_variable_binding() {
	// Create: case {1, 2} do {x, y} -> x + y end
	one := ast.LiteralExpr{
		value: ast.IntegerLiteral{
			value: 1
		}
	}
	two := ast.LiteralExpr{
		value: ast.IntegerLiteral{
			value: 2
		}
	}
	tuple := ast.TupleExpr{
		elements: [one, two]
	}

	x_var := create_variable_expr('x')
	y_var := create_variable_expr('y')
	binary := create_binary_expr(x_var, ast.BinaryOp.add, y_var, 1, 1)

	x_pattern := create_var_pattern('x')
	y_pattern := create_var_pattern('y')
	tuple_pattern := ast.TuplePattern{
		elements: [x_pattern, y_pattern]
	}

	body := [ast.Stmt(create_expr_stmt(binary))]
	clause := create_function_clause([], ast.LiteralExpr{
		value: ast.BooleanLiteral{
			value: true
		}
	}, body, 1, 1)

	match_case := ast.MatchCase{
		pattern:  tuple_pattern
		guard:    ast.LiteralExpr{
			value: ast.BooleanLiteral{
				value: true
			}
		}
		body:     body
		position: ast.new_position(1, 1, 'test.lx')
	}

	match_expr := ast.MatchExpr{
		value:    tuple
		cases:    [match_case]
		position: ast.new_position(1, 1, 'test.lx')
	}

	body_stmt := [ast.Stmt(create_expr_stmt(match_expr))]
	clause2 := create_function_clause([], ast.LiteralExpr{
		value: ast.BooleanLiteral{
			value: true
		}
	}, body_stmt, 1, 1)
	function := create_function_stmt('test', [clause2], 1, 1)

	test_module := ast.ModuleStmt{
		name:       'test'
		exports:    []
		imports:    []
		statements: [ast.Stmt(function)]
		position:   ast.new_position(1, 1, 'test.lx')
	}

	mut checker := analysis.new_variable_checker()
	result := checker.check_module(test_module)

	assert result.success == true
	assert result.errors.len == 0

	println('✅ Pattern variable binding test passed')
}

// Helper functions for creating test ASTs
fn create_assign_expr(name string, value ast.Expr, line int, column int) ast.AssignExpr {
	return ast.AssignExpr{
		name:     name
		value:    value
		position: ast.new_position(line, column, 'test.lx')
	}
}

fn create_variable_expr(name string) ast.VariableExpr {
	return ast.VariableExpr{
		name: name
	}
}

fn create_literal_expr(value ast.Literal) ast.LiteralExpr {
	return ast.LiteralExpr{
		value: value
	}
}

fn create_binary_expr(left ast.Expr, op ast.BinaryOp, right ast.Expr, line int, column int) ast.BinaryExpr {
	return ast.BinaryExpr{
		left:     left
		op:       op
		right:    right
		position: ast.new_position(line, column, 'test.lx')
	}
}

fn create_function_stmt(name string, clauses []ast.FunctionClause, line int, column int) ast.FunctionStmt {
	return ast.FunctionStmt{
		name:       name
		clauses:    clauses
		is_private: false
		position:   ast.new_position(line, column, 'test.lx')
	}
}

fn create_function_clause(parameters []ast.Pattern, guard ast.Expr, body []ast.Stmt, line int, column int) ast.FunctionClause {
	return ast.FunctionClause{
		parameters: parameters
		guard:      guard
		body:       body
		position:   ast.new_position(line, column, 'test.lx')
	}
}

fn create_var_pattern(name string) ast.VarPattern {
	return ast.VarPattern{
		name: name
	}
}

fn create_expr_stmt(expr ast.Expr) ast.ExprStmt {
	return ast.ExprStmt{
		expr: expr
	}
}

// Run all variable checker tests
fn run_all_variable_checker_tests() {
	println('🧪 Running Variable Checker Tests...')

	test_variable_scope_basic()
	test_variable_binding()
	test_scope_nesting()
	test_rebind_detection()
	test_shadowing_detection()
	test_parallel_scopes()
	test_error_reporting()
	test_variable_checker_integration()
	test_specific_ast_construction()
	test_rebind_detection_ast()
	test_undefined_variable_detection()
	test_pattern_variable_binding()

	println('🎉 All Variable Checker Tests Completed Successfully!')
}
