module analysis

import ast
import errors

pub struct VariableChecker {
pub mut:
	scope_manager ScopeManager
	errors        []errors.CompilationError
}

pub fn new_variable_checker() VariableChecker {
	return VariableChecker{
		scope_manager: new_scope_manager()
		errors:        []
	}
}

pub fn (mut vc VariableChecker) check_module(module_stmt ast.ModuleStmt) []errors.CompilationError {
	vc.scope_manager.enter_scope()
	defer { vc.scope_manager.exit_scope() }

	for stmt in module_stmt.statements {
		vc.check_statement(stmt)
	}

	return vc.errors
}

fn (mut vc VariableChecker) check_statement(stmt ast.Stmt) {
	match stmt {
		ast.ExprStmt { vc.check_expression_statement(stmt) }
		ast.FunctionStmt { vc.check_function_statement(stmt) }
		ast.RecordDefStmt {}
		ast.TypeDefStmt {}
		ast.TypeAliasStmt {}
		ast.ModuleStmt {}
	}
}

fn (mut vc VariableChecker) check_expression_statement(stmt ast.ExprStmt) {
	vc.check_expression(stmt.expr)
}

fn (mut vc VariableChecker) check_assignment_expression(expr ast.AssignExpr) {
	// Check for rebind in current scope
	if vc.scope_manager.has_binding_local(expr.name) {
		vc.report_error("Variable '${expr.name}' cannot be reassigned", 'Variables in LX are immutable and cannot be reassigned. Use a different variable name or restructure your code.',
			expr.position)
		return
	}

	// Check for shadowing from parent scopes
	if vc.scope_manager.has_binding_in_parent(expr.name) {
		vc.report_error("Variable '${expr.name}' shadows variable from outer scope", 'Shadowing is not allowed in LX. Use a different variable name: ${expr.name}_inner',
			expr.position)
		return
	}

	// Check the value expression
	vc.check_expression(expr.value)

	// Bind the variable to current scope
	vc.scope_manager.bind_variable(expr.name, expr.position)
}

fn (mut vc VariableChecker) check_function_statement(stmt ast.FunctionStmt) {
	// Enter function scope
	vc.scope_manager.enter_scope()
	defer { vc.scope_manager.exit_scope() }

	// Bind function parameters
	for clause in stmt.clauses {
		for param in clause.parameters {
			vc.check_pattern(param)
		}
		vc.check_expression(clause.body)
	}
}

fn (mut vc VariableChecker) check_expression(expr ast.Expr) {
	match expr {
		ast.BinaryExpr { vc.check_binary_expression(expr) }
		ast.CallExpr { vc.check_call_expression(expr) }
		ast.VariableExpr { vc.check_variable_expression(expr) }
		ast.AssignExpr { vc.check_assignment_expression(expr) }
		ast.IfExpr { vc.check_if_expression(expr) }
		ast.CaseExpr { vc.check_case_expression(expr) }
		ast.ReceiveExpr { vc.check_receive_expression(expr) }
		ast.FunExpr { vc.check_fun_expression(expr) }
		ast.ListConsExpr { vc.check_list_cons_expression(expr) }
		ast.ListLiteralExpr { vc.check_list_literal_expression(expr) }
		ast.TupleExpr { vc.check_tuple_expression(expr) }
		ast.MapLiteralExpr { vc.check_map_literal_expression(expr) }
		ast.MapAccessExpr { vc.check_map_access_expression(expr) }
		ast.MapUpdateExpr { vc.check_map_update_expression(expr) }
		ast.RecordLiteralExpr { vc.check_record_literal_expression(expr) }
		ast.RecordAccessExpr { vc.check_record_access_expression(expr) }
		ast.RecordUpdateExpr { vc.check_record_update_expression(expr) }
		ast.LiteralExpr { vc.check_literal_expression(expr) }
		ast.BlockExpr { vc.check_block_expression(expr) }
		ast.WithExpr { vc.check_with_expression(expr) }
		ast.ForExpr { vc.check_for_expression(expr) }
		ast.SendExpr { vc.check_send_expression(expr) }
		ast.MatchExpr { vc.check_match_expression(expr) }
		ast.GuardExpr { vc.check_guard_expression(expr) }
		ast.UnaryExpr { vc.check_unary_expression(expr) }
		ast.ListEmptyExpr {}
		ast.SimpleMatchExpr {}
		ast.MatchRescueExpr {}
	}
}

fn (mut vc VariableChecker) check_binary_expression(expr ast.BinaryExpr) {
	vc.check_expression(expr.left)
	vc.check_expression(expr.right)
}

fn (mut vc VariableChecker) check_call_expression(expr ast.CallExpr) {
	vc.check_expression(expr.function)
	for arg in expr.arguments {
		vc.check_expression(arg)
	}
}

fn (mut vc VariableChecker) check_variable_expression(expr ast.VariableExpr) {
	// Check if variable is bound
	if !vc.scope_manager.has_binding(expr.name) {
		vc.report_error("Unbound variable '${expr.name}'", 'Variable must be defined before use. Check spelling or define the variable first.',
			expr.position)
		return
	}

	// Mark variable as used
	vc.scope_manager.use_variable(expr.name, expr.position)
}

fn (mut vc VariableChecker) check_if_expression(expr ast.IfExpr) {
	vc.check_expression(expr.condition)
	vc.check_expression(expr.then_body)
	if expr.else_body.body.len > 0 {
		vc.check_expression(expr.else_body)
	}
}

fn (mut vc VariableChecker) check_case_expression(expr ast.CaseExpr) {
	vc.check_expression(expr.value)
	for case in expr.cases {
		vc.check_pattern(case.pattern)
		vc.check_expression(case.body)
	}
}

fn (mut vc VariableChecker) check_match_expression(expr ast.MatchExpr) {
	vc.check_expression(expr.value)
	for case in expr.cases {
		vc.check_pattern(case.pattern)
		vc.check_expression(case.body)
	}
}

fn (mut vc VariableChecker) check_receive_expression(expr ast.ReceiveExpr) {
	for case in expr.cases {
		vc.check_pattern(case.pattern)
		vc.check_expression(case.body)
	}
	if timeout := expr.timeout {
		vc.check_expression(timeout.body)
	}
}

fn (mut vc VariableChecker) check_fun_expression(expr ast.FunExpr) {
	for param in expr.parameters {
		vc.check_pattern(param)
	}
	vc.check_expression(expr.body)
}

fn (mut vc VariableChecker) check_list_literal_expression(expr ast.ListLiteralExpr) {
	for element in expr.elements {
		vc.check_expression(element)
	}
}

fn (mut vc VariableChecker) check_tuple_expression(expr ast.TupleExpr) {
	for element in expr.elements {
		vc.check_expression(element)
	}
}

fn (mut vc VariableChecker) check_literal_expression(expr ast.LiteralExpr) {
	// Literals don't introduce variables
}

fn (mut vc VariableChecker) check_block_expression(expr ast.BlockExpr) {
	vc.scope_manager.enter_scope()
	defer { vc.scope_manager.exit_scope() }

	for stmt in expr.body {
		vc.check_statement(stmt)
	}
}

fn (mut vc VariableChecker) check_with_expression(expr ast.WithExpr) {
	for binding in expr.bindings {
		vc.check_expression(binding.value)
		vc.check_pattern(binding.pattern)
	}
	vc.check_expression(expr.body)
	if expr.else_body.body.len > 0 {
		vc.check_expression(expr.else_body)
	}
}

fn (mut vc VariableChecker) check_for_expression(expr ast.ForExpr) {
	vc.check_expression(expr.collection)
	vc.check_pattern(expr.pattern)
	if expr.guard != ast.Expr{} {
		vc.check_expression(expr.guard)
	}
	vc.check_expression(expr.body)
}

fn (mut vc VariableChecker) check_send_expression(expr ast.SendExpr) {
	vc.check_expression(expr.pid)
	vc.check_expression(expr.message)
}

fn (mut vc VariableChecker) check_pattern(pattern ast.Pattern) {
	match pattern {
		ast.VarPattern { vc.check_variable_pattern(pattern) }
		ast.LiteralPattern { vc.check_literal_pattern(pattern) }
		ast.TuplePattern { vc.check_tuple_pattern(pattern) }
		ast.ListConsPattern { vc.check_list_cons_pattern(pattern) }
		ast.ListEmptyPattern {}
		ast.ListLiteralPattern { vc.check_list_literal_pattern(pattern) }
		ast.MapPattern { vc.check_map_pattern(pattern) }
		ast.RecordPattern { vc.check_record_pattern(pattern) }
		ast.BinaryPattern { vc.check_binary_pattern(pattern) }
		ast.WildcardPattern { vc.check_wildcard_pattern(pattern) }
		ast.AtomPattern { vc.check_atom_pattern(pattern) }
	}
}

fn (mut vc VariableChecker) check_variable_pattern(pattern ast.VarPattern) {
	// Bind pattern variable to current scope
	vc.scope_manager.bind_variable(pattern.name, pattern.position)
}

fn (mut vc VariableChecker) check_literal_pattern(pattern ast.LiteralPattern) {
	// Literal patterns don't introduce variables
}

fn (mut vc VariableChecker) check_tuple_pattern(pattern ast.TuplePattern) {
	for element in pattern.elements {
		vc.check_pattern(element)
	}
}

fn (mut vc VariableChecker) check_list_literal_pattern(pattern ast.ListLiteralPattern) {
	for element in pattern.elements {
		vc.check_pattern(element)
	}
}

fn (mut vc VariableChecker) check_map_pattern(pattern ast.MapPattern) {
	for entry in pattern.entries {
		vc.check_pattern(entry.key)
		vc.check_pattern(entry.value)
	}
}

fn (mut vc VariableChecker) check_record_pattern(pattern ast.RecordPattern) {
	for field in pattern.fields {
		vc.check_pattern(field.pattern)
	}
}

fn (mut vc VariableChecker) check_binary_pattern(pattern ast.BinaryPattern) {
	// Binary patterns don't introduce variables
}

fn (mut vc VariableChecker) check_wildcard_pattern(pattern ast.WildcardPattern) {
	// Wildcard patterns don't introduce variables
}

fn (mut vc VariableChecker) check_atom_pattern(pattern ast.AtomPattern) {
	// Atom patterns don't introduce variables
}

fn (mut vc VariableChecker) check_map_literal_expression(expr ast.MapLiteralExpr) {
	for entry in expr.entries {
		vc.check_expression(entry.key)
		vc.check_expression(entry.value)
	}
}

fn (mut vc VariableChecker) check_map_access_expression(expr ast.MapAccessExpr) {
	vc.check_expression(expr.map_expr)
	vc.check_expression(expr.key)
}

fn (mut vc VariableChecker) check_map_update_expression(expr ast.MapUpdateExpr) {
	vc.check_expression(expr.base_map)
	for entry in expr.entries {
		vc.check_expression(entry.key)
		vc.check_expression(entry.value)
	}
}

fn (mut vc VariableChecker) check_record_literal_expression(expr ast.RecordLiteralExpr) {
	for field in expr.fields {
		vc.check_expression(field.value)
	}
}

fn (mut vc VariableChecker) check_record_access_expression(expr ast.RecordAccessExpr) {
	vc.check_expression(expr.record)
}

fn (mut vc VariableChecker) check_record_update_expression(expr ast.RecordUpdateExpr) {
	vc.check_expression(expr.base_record)
	for field in expr.fields {
		vc.check_expression(field.value)
	}
}

fn (mut vc VariableChecker) check_guard_expression(expr ast.GuardExpr) {
	vc.check_expression(expr.condition)
}

fn (mut vc VariableChecker) check_unary_expression(expr ast.UnaryExpr) {
	vc.check_expression(expr.operand)
}

fn (mut vc VariableChecker) check_list_cons_expression(expr ast.ListConsExpr) {
	vc.check_expression(expr.head)
	vc.check_expression(expr.tail)
}

fn (mut vc VariableChecker) check_list_cons_pattern(pattern ast.ListConsPattern) {
	vc.check_pattern(pattern.head)
	vc.check_pattern(pattern.tail)
}

fn (mut vc VariableChecker) report_error(message string, suggestion string, position ast.Position) {
	error := errors.new_compilation_error(errors.UnboundVariableError{
		variable:   ''
		similar:    []
		suggestion: suggestion
	}, position, message)
	vc.errors << error
}
