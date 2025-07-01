module typechecker

import ast
import errors
import analysis

// TypeCheckResult represents the result of type checking
pub struct TypeCheckResult {
pub:
	success bool
	errors  []errors.CompilationError
}

// TypeChecker performs type checking and variable scope checking
pub struct TypeChecker {
pub mut:
	environment      &TypeEnvironment
	context          &TypeContext
	variable_checker analysis.VariableChecker
	errors           []errors.CompilationError
}

// new_type_checker creates a new type checker
pub fn new_type_checker() TypeChecker {
	return TypeChecker{
		environment:      new_environment()
		context:          new_context()
		variable_checker: analysis.new_variable_checker()
		errors:           []
	}
}

// check_module performs both type checking and variable scope checking on a module
pub fn (mut tc TypeChecker) check_module(mod ast.ModuleStmt) TypeCheckResult {
	// First, perform variable scope checking
	variable_result := tc.variable_checker.check_module(mod)

	// Then perform type checking
	type_result := tc.perform_type_checking(mod)

	// Combine results
	mut all_errors := []errors.CompilationError{}
	all_errors << variable_result.errors
	all_errors << type_result.errors

	return TypeCheckResult{
		success: all_errors.len == 0
		errors:  all_errors
	}
}

// perform_type_checking performs type checking on a module
fn (mut tc TypeChecker) perform_type_checking(mod ast.ModuleStmt) TypeCheckResult {
	// This is a placeholder for the actual type checking logic
	// In a real implementation, this would perform Hindley-Milner type inference

	for stmt in mod.statements {
		tc.check_statement(stmt)
	}

	return TypeCheckResult{
		success: tc.errors.len == 0
		errors:  tc.errors.clone()
	}
}

// check_statement performs type checking on a statement
fn (mut tc TypeChecker) check_statement(stmt ast.Stmt) {
	match stmt {
		ast.ExprStmt {
			tc.check_expression(stmt.expr)
		}
		ast.ModuleStmt {
			for stmt_ in stmt.statements {
				tc.check_statement(stmt_)
			}
		}
		ast.FunctionStmt {
			tc.check_function_statement(stmt)
		}
		ast.RecordDefStmt {
			tc.check_record_definition(stmt)
		}
		ast.TypeDefStmt {
			tc.check_type_definition(stmt)
		}
	}
}

// check_expression performs type checking on an expression
fn (mut tc TypeChecker) check_expression(expr ast.Expr) {
	match expr {
		ast.VariableExpr {
			tc.check_variable_expression(expr)
		}
		ast.AssignExpr {
			tc.check_assignment_expression(expr)
		}
		ast.BinaryExpr {
			tc.check_binary_expression(expr)
		}
		ast.CallExpr {
			tc.check_call_expression(expr)
		}
		ast.MatchExpr {
			tc.check_match_expression(expr)
		}
		ast.ListConsExpr {
			tc.check_list_cons_expression(expr)
		}
		ast.ListLiteralExpr {
			tc.check_list_literal_expression(expr)
		}
		ast.TupleExpr {
			tc.check_tuple_expression(expr)
		}
		ast.MapLiteralExpr {
			tc.check_map_literal_expression(expr)
		}
		ast.RecordLiteralExpr {
			tc.check_record_literal_expression(expr)
		}
		ast.RecordAccessExpr {
			tc.check_record_access_expression(expr)
		}
		ast.FunExpr {
			tc.check_fun_expression(expr)
		}
		ast.SendExpr {
			tc.check_send_expression(expr)
		}
		ast.ReceiveExpr {
			tc.check_receive_expression(expr)
		}
		ast.GuardExpr {
			tc.check_guard_expression(expr)
		}
		ast.UnaryExpr {
			tc.check_unary_expression(expr)
		}
		ast.MapAccessExpr {
			tc.check_map_access_expression(expr)
		}
		ast.IfExpr {
			tc.check_if_expression(expr)
		}
		ast.CaseExpr {
			tc.check_case_expression(expr)
		}
		ast.WithExpr {
			tc.check_with_expression(expr)
		}
		ast.ForExpr {
			tc.check_for_expression(expr)
		}
		ast.LiteralExpr {
			tc.check_literal_expression(expr)
		}
		ast.ListEmptyExpr {
			tc.check_list_empty_expression(expr)
		}
	}
}

// check_variable_expression performs type checking on a variable expression
fn (mut tc TypeChecker) check_variable_expression(expr ast.VariableExpr) {
	// Check if variable is bound in type context
	if _ := tc.context.lookup(expr.name) {
		// Variable is bound, type checking passes
	} else {
		// This should have been caught by variable checker, but just in case
		tc.report_error(
			"Variable '${expr.name}' is not defined",
			"Variables must be defined before use",
			ast.new_position(0, 0, 'unknown')
		)
	}
}

// check_assignment_expression performs type checking on an assignment expression
fn (mut tc TypeChecker) check_assignment_expression(expr ast.AssignExpr) {
	// Check the value expression first
	tc.check_expression(expr.value)

	// For now, we'll use a simple type inference
	// In a real implementation, this would perform proper type inference
	value_type := tc.infer_expression_type(expr.value)

	// Bind the variable to the inferred type
	tc.context.bind(expr.name, value_type, expr.position)
}

// check_binary_expression performs type checking on a binary expression
fn (mut tc TypeChecker) check_binary_expression(expr ast.BinaryExpr) {
	tc.check_expression(expr.left)
	tc.check_expression(expr.right)

	// Check operator compatibility
	left_type := tc.infer_expression_type(expr.left)
	right_type := tc.infer_expression_type(expr.right)

	// For now, we'll just check that both operands are numeric for arithmetic operators
	if expr.op in [.add, .subtract, .multiply, .divide, .modulo, .power] {
		if !tc.is_numeric_type(left_type) || !tc.is_numeric_type(right_type) {
			tc.report_error(
				"Arithmetic operator '${expr.op.str()}' requires numeric operands",
				"Both operands must be integers or floats",
				expr.position
			)
		}
	}
}

// check_call_expression performs type checking on a function call expression
fn (mut tc TypeChecker) check_call_expression(expr ast.CallExpr) {
	tc.check_expression(expr.function)

	for arg in expr.arguments {
		tc.check_expression(arg)
	}
}

// check_match_expression performs type checking on a match expression
fn (mut tc TypeChecker) check_match_expression(expr ast.MatchExpr) {
	tc.check_expression(expr.value)

	for case_ in expr.cases {
		tc.check_pattern(case_.pattern)
		tc.check_expression(case_.guard)

		for stmt in case_.body {
			tc.check_statement(stmt)
		}
	}
}

// check_list_cons_expression performs type checking on a list cons expression
fn (mut tc TypeChecker) check_list_cons_expression(expr ast.ListConsExpr) {
	tc.check_expression(expr.head)
	tc.check_expression(expr.tail)
}

// check_list_literal_expression performs type checking on a list literal expression
fn (mut tc TypeChecker) check_list_literal_expression(expr ast.ListLiteralExpr) {
	for element in expr.elements {
		tc.check_expression(element)
	}
}

// check_tuple_expression performs type checking on a tuple expression
fn (mut tc TypeChecker) check_tuple_expression(expr ast.TupleExpr) {
	for element in expr.elements {
		tc.check_expression(element)
	}
}

// check_map_literal_expression performs type checking on a map literal expression
fn (mut tc TypeChecker) check_map_literal_expression(expr ast.MapLiteralExpr) {
	for entry in expr.entries {
		tc.check_expression(entry.key)
		tc.check_expression(entry.value)
	}
}

// check_record_literal_expression performs type checking on a record literal expression
fn (mut tc TypeChecker) check_record_literal_expression(expr ast.RecordLiteralExpr) {
	for field in expr.fields {
		tc.check_expression(field.value)
	}
}

// check_record_access_expression performs type checking on a record access expression
fn (mut tc TypeChecker) check_record_access_expression(expr ast.RecordAccessExpr) {
	tc.check_expression(expr.record)
}

// check_fun_expression performs type checking on a fun expression
fn (mut tc TypeChecker) check_fun_expression(expr ast.FunExpr) {
	tc.context = tc.context.new_child_context()

	for param in expr.parameters {
		tc.check_pattern(param)
	}

	for stmt in expr.body {
		tc.check_statement(stmt)
	}

	// Restore parent context
	if parent := tc.context.parent {
		tc.context = parent
	}
}

// check_send_expression performs type checking on a send expression
fn (mut tc TypeChecker) check_send_expression(expr ast.SendExpr) {
	tc.check_expression(expr.pid)
	tc.check_expression(expr.message)
}

// check_receive_expression performs type checking on a receive expression
fn (mut tc TypeChecker) check_receive_expression(expr ast.ReceiveExpr) {
	for case_ in expr.cases {
		tc.check_pattern(case_.pattern)
		tc.check_expression(case_.guard)

		for stmt in case_.body {
			tc.check_statement(stmt)
		}
	}

	tc.check_expression(expr.timeout)
}

// check_guard_expression performs type checking on a guard expression
fn (mut tc TypeChecker) check_guard_expression(expr ast.GuardExpr) {
	tc.check_expression(expr.condition)
}

// check_unary_expression performs type checking on a unary expression
fn (mut tc TypeChecker) check_unary_expression(expr ast.UnaryExpr) {
	tc.check_expression(expr.operand)
}

// check_map_access_expression performs type checking on a map access expression
fn (mut tc TypeChecker) check_map_access_expression(expr ast.MapAccessExpr) {
	tc.check_expression(expr.map_expr)
	tc.check_expression(expr.key)
}

// check_if_expression performs type checking on an if expression
fn (mut tc TypeChecker) check_if_expression(expr ast.IfExpr) {
	tc.check_expression(expr.condition)

	tc.context = tc.context.new_child_context()
	for stmt in expr.then_body {
		tc.check_statement(stmt)
	}
	if parent := tc.context.parent {
		tc.context = parent
	}

	tc.context = tc.context.new_child_context()
	for stmt in expr.else_body {
		tc.check_statement(stmt)
	}
	if parent := tc.context.parent {
		tc.context = parent
	}
}

// check_case_expression performs type checking on a case expression
fn (mut tc TypeChecker) check_case_expression(expr ast.CaseExpr) {
	tc.check_expression(expr.value)

	for case_ in expr.cases {
		tc.check_pattern(case_.pattern)
		tc.check_expression(case_.guard)

		for stmt in case_.body {
			tc.check_statement(stmt)
		}
	}
}

// check_with_expression performs type checking on a with expression
fn (mut tc TypeChecker) check_with_expression(expr ast.WithExpr) {
	for binding in expr.bindings {
		tc.check_pattern(binding.pattern)
		tc.check_expression(binding.value)
	}

	tc.context = tc.context.new_child_context()
	for stmt in expr.body {
		tc.check_statement(stmt)
	}
	if parent := tc.context.parent {
		tc.context = parent
	}

	tc.context = tc.context.new_child_context()
	for stmt in expr.else_body {
		tc.check_statement(stmt)
	}
	if parent := tc.context.parent {
		tc.context = parent
	}
}

// check_for_expression performs type checking on a for expression
fn (mut tc TypeChecker) check_for_expression(expr ast.ForExpr) {
	tc.check_pattern(expr.pattern)
	tc.check_expression(expr.collection)
	tc.check_expression(expr.guard)

	tc.context = tc.context.new_child_context()
	for stmt in expr.body {
		tc.check_statement(stmt)
	}
	if parent := tc.context.parent {
		tc.context = parent
	}
}

// check_literal_expression performs type checking on a literal expression
fn (mut tc TypeChecker) check_literal_expression(expr ast.LiteralExpr) {
	// Literals are always well-typed
}

// check_list_empty_expression performs type checking on an empty list expression
fn (mut tc TypeChecker) check_list_empty_expression(expr ast.ListEmptyExpr) {
	// Empty list is always well-typed
}

// check_pattern performs type checking on a pattern
fn (mut tc TypeChecker) check_pattern(pattern ast.Pattern) {
	match pattern {
		ast.VarPattern {
			// Bind variable with unknown type for now
			tc.context.bind(pattern.name, typechecker.make_type_var('a'), ast.new_position(0, 0, 'unknown'))
		}
		ast.WildcardPattern, ast.LiteralPattern, ast.AtomPattern, ast.ListEmptyPattern {
			// These patterns don't bind variables
		}
		ast.ListConsPattern {
			tc.check_pattern(pattern.head)
			tc.check_pattern(pattern.tail)
		}
		ast.ListLiteralPattern {
			for element in pattern.elements {
				tc.check_pattern(element)
			}
		}
		ast.TuplePattern {
			for element in pattern.elements {
				tc.check_pattern(element)
			}
		}
		ast.MapPattern {
			for entry in pattern.entries {
				tc.check_pattern(entry.key)
				tc.check_pattern(entry.value)
			}
		}
		ast.RecordPattern {
			for field in pattern.fields {
				tc.check_pattern(field.pattern)
			}
		}
		ast.BinaryPattern {
			// Binary patterns don't bind variables
		}
	}
}

// check_function_statement performs type checking on a function statement
fn (mut tc TypeChecker) check_function_statement(stmt ast.FunctionStmt) {
	tc.context = tc.context.new_child_context()

	for clause in stmt.clauses {
		for param in clause.parameters {
			tc.check_pattern(param)
		}

		tc.check_expression(clause.guard)

		for body_stmt in clause.body {
			tc.check_statement(body_stmt)
		}
	}

	// Restore parent context
	if parent := tc.context.parent {
		tc.context = parent
	}
}

// check_record_definition performs type checking on a record definition
fn (mut tc TypeChecker) check_record_definition(stmt ast.RecordDefStmt) {
	// Record definitions are type declarations, not expressions
	// They don't need type checking, just registration
}

// check_type_definition performs type checking on a type definition
fn (mut tc TypeChecker) check_type_definition(stmt ast.TypeDefStmt) {
	// Type definitions are type declarations, not expressions
	// They don't need type checking, just registration
}

// infer_expression_type infers the type of an expression
fn (tc &TypeChecker) infer_expression_type(expr ast.Expr) TypeExpr {
	match expr {
		ast.VariableExpr {
			if binding := tc.context.lookup(expr.name) {
				return binding.type_expr
			}
			return typechecker.make_type_var('a')
		}
		ast.LiteralExpr {
			return tc.infer_literal_type(expr.value)
		}
		ast.AssignExpr {
			return tc.infer_expression_type(expr.value)
		}
		ast.BinaryExpr {
			return tc.infer_binary_expression_type(expr)
		}
		ast.CallExpr {
			return typechecker.make_type_var('a') // Placeholder
		}
		ast.MatchExpr {
			return typechecker.make_type_var('a') // Placeholder
		}
		ast.ListConsExpr {
			return typechecker.make_list_type(typechecker.make_type_var('a'))
		}
		ast.ListEmptyExpr {
			return typechecker.make_list_type(typechecker.make_type_var('a'))
		}
		ast.ListLiteralExpr {
			if expr.elements.len == 0 {
				return typechecker.make_list_type(typechecker.make_type_var('a'))
			}
			element_type := tc.infer_expression_type(expr.elements[0])
			return typechecker.make_list_type(element_type)
		}
		ast.TupleExpr {
			mut element_types := []TypeExpr{}
			for element in expr.elements {
				element_types << tc.infer_expression_type(element)
			}
			return typechecker.make_tuple_type(element_types)
		}
		ast.MapLiteralExpr {
			return typechecker.make_map_type(typechecker.make_type_var('k'), typechecker.make_type_var('v'))
		}
		ast.RecordLiteralExpr {
			return typechecker.make_type_constructor('record', [])
		}
		ast.RecordAccessExpr {
			return typechecker.make_type_var('a')
		}
		ast.FunExpr {
			return typechecker.make_function_type([], typechecker.make_type_var('a'))
		}
		ast.SendExpr {
			return typechecker.make_type_constructor('atom', [])
		}
		ast.ReceiveExpr {
			return typechecker.make_type_var('a')
		}
		ast.GuardExpr {
			return typechecker.boolean_type
		}
		ast.UnaryExpr {
			return tc.infer_expression_type(expr.operand)
		}
		ast.MapAccessExpr {
			return typechecker.make_type_var('v')
		}
		ast.IfExpr {
			return typechecker.make_type_var('a')
		}
		ast.CaseExpr {
			return typechecker.make_type_var('a')
		}
		ast.WithExpr {
			return typechecker.make_type_var('a')
		}
		ast.ForExpr {
			return typechecker.make_list_type(typechecker.make_type_var('a'))
		}
	}
}

// infer_literal_type infers the type of a literal
fn (tc &TypeChecker) infer_literal_type(literal ast.Literal) TypeExpr {
	match literal {
		ast.IntegerLiteral {
			return typechecker.integer_type
		}
		ast.FloatLiteral {
			return typechecker.float_type
		}
		ast.StringLiteral {
			return typechecker.string_type
		}
		ast.BooleanLiteral {
			return typechecker.boolean_type
		}
		ast.AtomLiteral {
			return atom_type
		}
		ast.NilLiteral {
			return nil_type
		}
	}
}

// infer_binary_expression_type infers the type of a binary expression
fn (tc &TypeChecker) infer_binary_expression_type(expr ast.BinaryExpr) TypeExpr {
	left_type := tc.infer_expression_type(expr.left)
	right_type := tc.infer_expression_type(expr.right)

	match expr.op {
		.add, .subtract, .multiply, .divide, .modulo, .power {
			// Arithmetic operators return the type of the operands
			if tc.is_numeric_type(left_type) && tc.is_numeric_type(right_type) {
				return left_type
			}
			return make_type_var('a')
		}
		.equal, .not_equal, .less_than, .less_equal, .greater_than, .greater_equal {
			return boolean_type
		}
		.and, .or, .andalso, .orelse {
			return boolean_type
		}
		.cons {
			return make_list_type(left_type)
		}
		.append {
			return left_type
		}
	}
}

// is_numeric_type checks if a type is numeric
fn (tc &TypeChecker) is_numeric_type(type_expr TypeExpr) bool {
	return type_expr.str() == 'integer' || type_expr.str() == 'float'
}

// report_error adds an error to the type checker
fn (mut tc TypeChecker) report_error(message string, suggestion string, position ast.Position) {
	error := errors.new_compilation_error(
		errors.TypeError{
			message:    message
			expected:   ''
			actual:     ''
			suggestion: suggestion
		},
		position,
		message
	)
	tc.errors << error
}