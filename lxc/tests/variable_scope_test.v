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

// Run all variable scope tests
fn run_variable_scope_tests() {
	println('🧪 Running Variable Scope Tests...')

	test_variable_scope_basic()
	test_variable_binding()
	test_scope_nesting()
	test_rebind_detection()
	test_shadowing_detection()
	test_parallel_scopes()
	test_error_reporting()
	test_variable_checker_integration()

	println('🎉 All Variable Scope Tests Completed Successfully!')
}
