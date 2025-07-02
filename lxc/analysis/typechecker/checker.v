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

// TypeChecker performs type checking on LX code
pub struct TypeChecker {
mut:
	context          &TypeContext
	environment      &TypeEnvironment
	unifier          Unifier
	errors           []errors.CompilationError
	variable_checker analysis.VariableChecker
}

// new_type_checker creates a new type checker
pub fn new_type_checker() TypeChecker {
	return TypeChecker{
		context:          new_type_context()
		environment:      new_environment()
		unifier:          new_unifier()
		errors:           []
		variable_checker: analysis.new_variable_checker()
	}
}

// check_module performs type checking on a module
pub fn (mut tc TypeChecker) check_module(module_stmt ast.ModuleStmt) TypeCheckResult {
	// Validate directives first
	directive_errors := tc.validate_directives(module_stmt)
	if directive_errors.len > 0 {
		tc.errors << directive_errors
		return TypeCheckResult{
			success: false
			errors:  tc.errors
		}
	}

	// Perform type checking
	for stmt in module_stmt.statements {
		tc.check_statement(stmt)
	}

	return TypeCheckResult{
		success: tc.errors.len == 0
		errors:  tc.errors
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
		ast.TypeAliasStmt {
			tc.check_type_alias_statement(stmt)
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
		tc.report_error("Variable '${expr.name}' is not defined", 'Variables must be defined before use',
			expr.position)
	}
}

// check_assignment_expression performs type checking on an assignment expression
fn (mut tc TypeChecker) check_assignment_expression(expr ast.AssignExpr) {
	// Check the value expression first
	tc.check_expression(expr.value)

	// Infer the type of the value
	value_type := tc.infer_expression_type(expr.value)

	// Check type annotation if present
	if type_annotation := expr.type_annotation {
		tc.check_type_annotation(expr.value, type_annotation)
		// Use the annotated type for binding
		annotated_type := tc.convert_type_expression_to_type_expr(type_annotation)
		tc.context.bind(expr.name, annotated_type, expr.position)
	} else {
		// Bind the variable to the inferred type
		tc.context.bind(expr.name, value_type, expr.position)
	}
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
			tc.report_error("Arithmetic operator '${expr.op.str()}' requires numeric operands",
				'Both operands must be integers or floats', expr.position)
		}
	}

	// Add new check for boolean operands in and/or
	if expr.op in [.and, .or] {
		if left_type.str() != 'boolean' {
			tc.report_error('Left operand of `${expr.op.str()}` must be boolean', 'Use only boolean expressions with `${expr.op.str()}`',
				tc.get_expression_position(expr.left))
		}
		if right_type.str() != 'boolean' {
			tc.report_error('Right operand of `${expr.op.str()}` must be boolean', 'Use only boolean expressions with `${expr.op.str()}`',
				tc.get_expression_position(expr.right))
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
			// Only bind if not already bound in context
			if _ := tc.context.lookup(pattern.name) {
				// Variable already bound, don't override
			} else {
				// Bind variable with inferred type
				param_type := tc.infer_pattern_type(pattern)
				tc.context.bind(pattern.name, param_type, pattern.position)
			}
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
		// Bind parameters with type annotations to the context
		for param in clause.parameters {
			match param {
				ast.VarPattern {
					param_type := tc.infer_pattern_type(param)
					tc.context.bind(param.name, param_type, param.position)
				}
				else {}
			}
		}

		// Validate type annotations in parameters
		for param in clause.parameters {
			tc.check_pattern(param)
			// Also validate type annotations specifically
			tc.validate_pattern_type_annotations(param)
		}

		tc.check_expression(clause.guard)

		for body_stmt in clause.body {
			tc.check_statement(body_stmt)
		}
	}

	// Apply directives
	for directive in stmt.directives {
		tc.apply_directive(directive, stmt)
	}

	// Restore parent context
	if parent := tc.context.parent {
		tc.context = parent
	}
}

// validate_pattern_type_annotations validates type annotations in patterns
fn (mut tc TypeChecker) validate_pattern_type_annotations(pattern ast.Pattern) {
	match pattern {
		ast.VarPattern {
			if type_annotation := pattern.type_annotation {
				// This will trigger validation and error reporting if type is invalid
				tc.convert_type_expression_to_type_expr(type_annotation)
			}
		}
		ast.ListConsPattern {
			tc.validate_pattern_type_annotations(pattern.head)
			tc.validate_pattern_type_annotations(pattern.tail)
		}
		ast.ListLiteralPattern {
			for element in pattern.elements {
				tc.validate_pattern_type_annotations(element)
			}
		}
		ast.TuplePattern {
			for element in pattern.elements {
				tc.validate_pattern_type_annotations(element)
			}
		}
		ast.MapPattern {
			for entry in pattern.entries {
				tc.validate_pattern_type_annotations(entry.key)
				tc.validate_pattern_type_annotations(entry.value)
			}
		}
		ast.RecordPattern {
			for field in pattern.fields {
				tc.validate_pattern_type_annotations(field.pattern)
			}
		}
		else {
			// Other patterns don't have type annotations
		}
	}
}

// apply_directive applies a directive to a function
fn (mut tc TypeChecker) apply_directive(directive string, func_stmt ast.FunctionStmt) {
	// Remove @ prefix if present
	directive_name := if directive.starts_with('@') {
		directive[1..]
	} else {
		directive
	}

	match directive_name {
		'reflection' {
			tc.handle_reflection(func_stmt)
		}
		'inline' {
			tc.handle_inline(func_stmt)
		}
		'deprecated' {
			tc.handle_deprecated(func_stmt)
		}
		else {
			// Unknown directive - this should be caught by validation
		}
	}
}

// infer_function_return_type infers the return type of a function based on its body
fn (mut tc TypeChecker) infer_function_return_type(clause ast.FunctionClause) TypeExpr {
	if clause.body.len == 0 {
		return make_type_constructor('ok', [])
	}

	// Create a temporary child context for parameter bindings
	mut temp_context := tc.context.new_child_context()

	// Bind parameters to their types in the context
	for param in clause.parameters {
		match param {
			ast.VarPattern {
				param_type := tc.infer_pattern_type(param)
				temp_context.bind(param.name, param_type, ast.new_position(0, 0, 'unknown'))
			}
			else {}
		}
	}

	// Get the last statement in the body (the return value)
	last_stmt := clause.body[clause.body.len - 1]

	match last_stmt {
		ast.ExprStmt {
			mut temp_tc := TypeChecker{
				context:     temp_context
				unifier:     tc.unifier
				errors:      tc.errors
				environment: tc.environment
			}
			// Se for variável, tente encontrar o alias
			match last_stmt.expr {
				ast.VariableExpr {
					for param in clause.parameters {
						match param {
							ast.VarPattern {
								if param.name == last_stmt.expr.name {
									if type_ann := param.type_annotation {
										match type_ann {
											ast.SimpleTypeExpr {
												if tc.context.has_type_alias(type_ann.name) {
													println('DEBUG: Creating TypeConstructor with name: ${type_ann.name}')
													return make_type_constructor(type_ann.name,
														[])
												} else {
													return tc.convert_type_expression_to_type_expr(type_ann)
												}
											}
											else {
												return tc.convert_type_expression_to_type_expr(type_ann)
											}
										}
									}
								}
							}
							else {}
						}
					}
				}
				else {}
			}
			return temp_tc.infer_expression_type(last_stmt.expr)
		}
		else {
			return make_type_constructor('ok', [])
		}
	}
}

// handle_reflection prints type information for functions
fn (mut tc TypeChecker) handle_reflection(func_stmt ast.FunctionStmt) {
	println('=== REFLECTION INFO function: ${func_stmt.name} ===')

	for clause in func_stmt.clauses {
		// Build parameter string
		mut param_strs := []string{}
		for param in clause.parameters {
			// Show the original type annotation if available, otherwise the inferred type
			match param {
				ast.VarPattern {
					if type_ann := param.type_annotation {
						param_strs << '${param.name} :: ${type_ann.str()}'
					} else {
						param_type := tc.infer_pattern_type(param)
						param_strs << '${param.name} :: ${param_type.str()}'
					}
				}
				else {
					param_type := tc.infer_pattern_type(param)
					param_strs << '${param.str()} :: ${param_type.str()}'
				}
			}
		}
		params_str := param_strs.join(', ')

		// Infer return type
		return_type := tc.infer_function_return_type(clause)

		// Detect if return is a variable matching a parameter with annotation
		mut return_type_str := return_type.str()
		last_stmt := clause.body[clause.body.len - 1]
		if last_stmt is ast.ExprStmt {
			stmt := last_stmt as ast.ExprStmt
			if stmt.expr is ast.VariableExpr {
				vexpr := stmt.expr as ast.VariableExpr
				for param in clause.parameters {
					if param is ast.VarPattern {
						varp := param as ast.VarPattern
						if varp.name == vexpr.name {
							if type_ann := varp.type_annotation {
								return_type_str = type_ann.str()
							}
						}
					}
				}
			}
		}

		// Build guard string - format guard expression cleanly
		guard_str := tc.format_guard_for_reflection(clause.guard)

		// Print function signature
		if guard_str == 'nil' {
			println('  ${func_stmt.name}(${params_str}) :: ${return_type_str}')
		} else {
			println('  ${func_stmt.name}(${params_str}) :: ${return_type_str} [ ${guard_str}]')
		}

		// Create a temporary context with parameter bindings for body type inference
		mut temp_context := tc.context.new_child_context()
		for param in clause.parameters {
			match param {
				ast.VarPattern {
					param_type := tc.infer_pattern_type(param)
					temp_context.bind(param.name, param_type, ast.new_position(0, 0, 'unknown'))
				}
				else {}
			}
		}

		// Create temporary type checker with parameter context
		mut temp_tc := TypeChecker{
			context:     temp_context
			unifier:     tc.unifier
			errors:      tc.errors
			environment: tc.environment
		}

		// Print body expressions with proper type inference
		for stmt in clause.body {
			match stmt {
				ast.ExprStmt {
					expr_type := temp_tc.infer_expression_type(stmt.expr)

					// Check if this is a variable expression that matches a parameter with annotation
					mut type_str := expr_type.str()
					if stmt.expr is ast.VariableExpr {
						vexpr := stmt.expr as ast.VariableExpr
						for param in clause.parameters {
							if param is ast.VarPattern {
								varp := param as ast.VarPattern
								if varp.name == vexpr.name {
									if type_ann := varp.type_annotation {
										type_str = type_ann.str()
									}
								}
							}
						}
					}

					println('    ${stmt.expr.str()} :: ${type_str}')
				}
				else {
					println('    ${stmt.str()}')
				}
			}
		}
		println('')
	}
	println('=== END REFLECTION INFO ===')
}

// handle_inline marks function for inlining optimization
fn (tc &TypeChecker) handle_inline(func_stmt ast.FunctionStmt) {
	// TODO: Implement inlining logic
	println('Function ${func_stmt.name} marked for inlining')
}

// handle_deprecated marks function as deprecated
fn (tc &TypeChecker) handle_deprecated(func_stmt ast.FunctionStmt) {
	println('WARNING: Function ${func_stmt.name} is deprecated')
}

// format_guard_for_reflection formats guard expressions for clean reflection output
fn (tc &TypeChecker) format_guard_for_reflection(guard ast.Expr) string {
	return match guard {
		ast.LiteralExpr {
			match guard.value {
				ast.BooleanLiteral {
					if guard.value.value {
						'nil'
					} else {
						'false'
					}
				}
				else {
					tc.format_guard_operand(guard)
				}
			}
		}
		ast.BinaryExpr {
			left_str := tc.format_guard_for_reflection(guard.left)
			right_str := tc.format_guard_for_reflection(guard.right)
			op_str := tc.format_guard_operator(guard.op)
			'${left_str} ${op_str} ${right_str}'
		}
		ast.UnaryExpr {
			operand_str := tc.format_guard_for_reflection(guard.operand)
			op_str := tc.format_guard_operator(guard.op)
			'${op_str}${operand_str}'
		}
		else {
			tc.format_guard_operand(guard)
		}
	}
}

// format_guard_operand formats operands in guard expressions
fn (tc &TypeChecker) format_guard_operand(expr ast.Expr) string {
	return match expr {
		ast.VariableExpr {
			expr.name
		}
		ast.LiteralExpr {
			match expr.value {
				ast.IntegerLiteral {
					expr.value.value.str()
				}
				ast.FloatLiteral {
					expr.value.value.str()
				}
				ast.StringLiteral {
					'"${expr.value.value}"'
				}
				ast.BooleanLiteral {
					expr.value.value.str()
				}
				ast.AtomLiteral {
					':${expr.value.value}'
				}
				else {
					expr.str()
				}
			}
		}
		else {
			expr.str()
		}
	}
}

// GuardOperator represents operators that can appear in guard expressions
type GuardOperator = ast.BinaryOp | ast.UnaryOp

// format_guard_operator formats operators in guard expressions
fn (tc &TypeChecker) format_guard_operator(op GuardOperator) string {
	return match op {
		ast.BinaryOp {
			// In reflection, show the LX syntax (and/or) even in guards
			op.str()
		}
		ast.UnaryOp {
			op.str()
		}
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

// check_type_alias_statement performs type checking on type alias statements
fn (mut tc TypeChecker) check_type_alias_statement(stmt ast.TypeAliasStmt) {
	// Convert TypeExpression to TypeExpr for the type system
	type_expr := tc.convert_type_expression_to_type_expr(stmt.type_expr)

	// Register the type alias in the context
	tc.context.register_type_alias(stmt.name, type_expr)
}

// convert_type_expression_to_type_expr converts AST TypeExpression to typechecker TypeExpr
fn (mut tc TypeChecker) convert_type_expression_to_type_expr(type_expr ast.TypeExpression) TypeExpr {
	return match type_expr {
		ast.SimpleTypeExpr {
			match type_expr.name {
				'integer' {
					integer_type
				}
				'float' {
					float_type
				}
				'string' {
					string_type
				}
				'boolean' {
					boolean_type
				}
				'atom' {
					atom_type
				}
				'nil' {
					nil_type
				}
				'pid' {
					make_type_constructor('pid', [])
				}
				'reference' {
					make_type_constructor('reference', [])
				}
				'port' {
					make_type_constructor('port', [])
				}
				'binary' {
					BinaryType{
						unit_size: 0
					}
				}
				'bitstring' {
					BinaryType{
						unit_size: 1
					}
				}
				'any' {
					make_type_constructor('any', [])
				}
				else {
					// Check if it's a registered type alias
					if alias_type := tc.context.lookup_type_alias(type_expr.name) {
						alias_type
					} else {
						// Unknown type - report error
						error := errors.new_compilation_error(errors.TypeError{
							message:    'Undefined type \'${type_expr.name}\''
							expected:   'concrete type (integer, string, etc.) or defined type alias'
							actual:     'unknown type \'${type_expr.name}\''
							suggestion: 'You need to define this type first. Use: type ${type_expr.name} :: concrete_type (e.g., type ${type_expr.name} :: integer | string)'
						}, type_expr.position, 'Undefined type \'${type_expr.name}\'')
						tc.errors << error
						// Return unknown type as fallback
						make_type_constructor('unknown', [])
					}
				}
			}
		}
		ast.UnionTypeExpr {
			// For now, create a type variable representing the union
			// A full implementation would need proper union type support
			make_type_var('union_${type_expr.types.len}')
		}
		ast.ListTypeExpr {
			element_type := tc.convert_type_expression_to_type_expr(type_expr.element_type)
			make_list_type(element_type)
		}
		ast.TupleTypeExpr {
			mut element_types := []TypeExpr{}
			for elem_type in type_expr.element_types {
				element_types << tc.convert_type_expression_to_type_expr(elem_type)
			}
			make_tuple_type(element_types)
		}
		ast.MapTypeExpr {
			key_type := tc.convert_type_expression_to_type_expr(type_expr.key_type)
			value_type := tc.convert_type_expression_to_type_expr(type_expr.value_type)
			make_map_type(key_type, value_type)
		}
		ast.FunctionTypeExpr {
			mut param_types := []TypeExpr{}
			for param_type in type_expr.param_types {
				param_types << tc.convert_type_expression_to_type_expr(param_type)
			}
			return_type := tc.convert_type_expression_to_type_expr(type_expr.return_type)
			make_function_type(param_types, return_type)
		}
		ast.VariableTypeExpr {
			// In type annotations, type variables should not be allowed unless explicitly defined
			error := errors.new_compilation_error(errors.TypeError{
				message:    'Undefined type \'${type_expr.name}\' in type annotation'
				expected:   'concrete type (integer, string, etc.) or defined type alias'
				actual:     'type variable \'${type_expr.name}\''
				suggestion: 'You need to define this type first. Use: type ${type_expr.name} :: concrete_type (e.g., type ${type_expr.name} :: integer)'
			}, type_expr.position, 'Undefined type \'${type_expr.name}\' in type annotation')
			tc.errors << error
			// Return unknown type as fallback
			make_type_constructor('unknown', [])
		}
	}
}

// check_type_annotation validates type annotations in expressions
fn (mut tc TypeChecker) check_type_annotation(expr ast.Expr, annotation ast.TypeExpression) {
	inferred_type := tc.infer_expression_type(expr)
	annotated_type := tc.convert_type_expression_to_type_expr(annotation)

	// Try to unify the inferred type with the annotated type
	result := tc.unifier.unify(inferred_type, annotated_type, ast.new_position(0, 0, 'unknown'))
	if _ := result.error {
		tc.report_error('Type mismatch: expected ${annotated_type.str()}, got ${inferred_type.str()}',
			'Check the type annotation or the expression', ast.new_position(0, 0, 'unknown'))
	}
}

// infer_expression_type infers the type of an expression
fn (mut tc TypeChecker) infer_expression_type(expr ast.Expr) TypeExpr {
	match expr {
		ast.VariableExpr {
			if binding := tc.context.lookup(expr.name) {
				return binding.type_expr
			}
			return make_type_var('a')
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
			return make_type_var('a') // Placeholder
		}
		ast.MatchExpr {
			return make_type_var('a') // Placeholder
		}
		ast.ListConsExpr {
			return make_list_type(make_type_var('a'))
		}
		ast.ListEmptyExpr {
			return make_list_type(make_type_var('a'))
		}
		ast.ListLiteralExpr {
			if expr.elements.len == 0 {
				return make_list_type(make_type_var('a'))
			}
			element_type := tc.infer_expression_type(expr.elements[0])
			return make_list_type(element_type)
		}
		ast.TupleExpr {
			mut element_types := []TypeExpr{}
			for element in expr.elements {
				element_types << tc.infer_expression_type(element)
			}
			return make_tuple_type(element_types)
		}
		ast.MapLiteralExpr {
			return make_map_type(make_type_var('k'), make_type_var('v'))
		}
		ast.RecordLiteralExpr {
			return make_type_constructor('record', [])
		}
		ast.RecordAccessExpr {
			return make_type_var('a')
		}
		ast.FunExpr {
			return make_function_type([], make_type_var('a'))
		}
		ast.SendExpr {
			return make_type_constructor('atom', [])
		}
		ast.ReceiveExpr {
			return make_type_var('a')
		}
		ast.GuardExpr {
			return boolean_type
		}
		ast.UnaryExpr {
			return tc.infer_expression_type(expr.operand)
		}
		ast.MapAccessExpr {
			return make_type_var('v')
		}
		ast.IfExpr {
			return make_type_var('a')
		}
		ast.CaseExpr {
			return make_type_var('a')
		}
		ast.WithExpr {
			return make_type_var('a')
		}
		ast.ForExpr {
			return make_list_type(make_type_var('a'))
		}
	}
}

// infer_literal_type infers the type of a literal
fn (tc &TypeChecker) infer_literal_type(literal ast.Literal) TypeExpr {
	match literal {
		ast.IntegerLiteral {
			return integer_type
		}
		ast.FloatLiteral {
			return float_type
		}
		ast.StringLiteral {
			return string_type
		}
		ast.BooleanLiteral {
			return boolean_type
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
fn (mut tc TypeChecker) infer_binary_expression_type(expr ast.BinaryExpr) TypeExpr {
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
			// Check if operands are comparable
			if !tc.are_types_comparable(left_type, right_type) {
				tc.report_error('Cannot compare ${left_type.str()} with ${right_type.str()}',
					'Use operands of compatible types', expr.position)
			}
			return boolean_type
		}
		.and, .or {
			if left_type.str() != 'boolean' {
				tc.report_error('Left operand of `${expr.op.str()}` must be boolean',
					'Use only boolean expressions with `${expr.op.str()}`', tc.get_expression_position(expr.left))
			}
			if right_type.str() != 'boolean' {
				tc.report_error('Right operand of `${expr.op.str()}` must be boolean',
					'Use only boolean expressions with `${expr.op.str()}`', tc.get_expression_position(expr.right))
			}
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

// are_types_comparable checks if two types can be compared
fn (tc &TypeChecker) are_types_comparable(left_type TypeExpr, right_type TypeExpr) bool {
	left_str := left_type.str()
	right_str := right_type.str()

	// Same types are always comparable
	if left_str == right_str {
		return true
	}

	// Numeric types are comparable with each other
	if tc.is_numeric_type(left_type) && tc.is_numeric_type(right_type) {
		return true
	}

	// Generic types (type variables) are considered compatible for now
	if left_str.starts_with('a') || right_str.starts_with('a') {
		return true
	}

	// Different concrete types are not comparable
	return false
}

// report_error adds an error to the type checker
fn (mut tc TypeChecker) report_error(message string, suggestion string, position ast.Position) {
	error := errors.new_compilation_error(errors.TypeError{
		message:    message
		expected:   ''
		actual:     ''
		suggestion: suggestion
	}, position, message)
	tc.errors << error
}

// print_statement_types prints type information for statements in function body
fn (mut tc TypeChecker) print_statement_types(stmt ast.Stmt, indent string) {
	match stmt {
		ast.ExprStmt {
			expr_type := tc.infer_expression_type(stmt.expr)
			println('${indent}${stmt.expr.str()} :: ${expr_type.str()}')
		}
		else {
			println('${indent}${stmt.str()}')
		}
	}
}

// infer_pattern_type infers the type of a pattern
fn (mut tc TypeChecker) infer_pattern_type(pattern ast.Pattern) TypeExpr {
	return match pattern {
		ast.VarPattern {
			if type_annotation := pattern.type_annotation {
				tc.convert_type_expression_to_type_expr(type_annotation)
			} else {
				make_type_var('a')
			}
		}
		ast.WildcardPattern {
			make_type_var('_')
		}
		ast.LiteralPattern {
			tc.infer_literal_type(pattern.value)
		}
		ast.AtomPattern {
			atom_type
		}
		ast.ListEmptyPattern {
			make_list_type(make_type_var('a'))
		}
		ast.ListConsPattern {
			head_type := tc.infer_pattern_type(pattern.head)
			_ := tc.infer_pattern_type(pattern.tail)
			// For simplicity, assume tail is a list of the same type as head
			make_list_type(head_type)
		}
		ast.ListLiteralPattern {
			if pattern.elements.len == 0 {
				make_list_type(make_type_var('a'))
			} else {
				element_type := tc.infer_pattern_type(pattern.elements[0])
				make_list_type(element_type)
			}
		}
		ast.TuplePattern {
			mut element_types := []TypeExpr{}
			for element in pattern.elements {
				element_types << tc.infer_pattern_type(element)
			}
			make_tuple_type(element_types)
		}
		ast.MapPattern {
			make_map_type(make_type_var('k'), make_type_var('v'))
		}
		ast.RecordPattern {
			make_type_constructor('record', [])
		}
		ast.BinaryPattern {
			BinaryType{
				unit_size: 0
			}
		}
	}
}

// validate_directives validates all directives in a module
fn (tc &TypeChecker) validate_directives(module_stmt ast.ModuleStmt) []errors.CompilationError {
	mut directive_errors := []errors.CompilationError{}

	// Define valid directives
	valid_directives := ['reflection', 'inline', 'deprecated']

	for stmt in module_stmt.statements {
		if stmt is ast.FunctionStmt {
			func_stmt := stmt as ast.FunctionStmt
			for directive in func_stmt.directives {
				if !valid_directives.contains(directive) {
					error_msg := 'Unknown directive: @${directive}'
					suggestion := 'Available directives: ${valid_directives.join(', ')}'

					compilation_error := errors.new_compilation_error(errors.SyntaxError{
						message:  error_msg
						expected: suggestion
						found:    '@${directive}'
					}, func_stmt.position, error_msg)

					directive_errors << compilation_error
				}
			}
		}
	}

	return directive_errors
}

// get_expression_position safely gets the position of an expression
fn (tc &TypeChecker) get_expression_position(expr ast.Expr) ast.Position {
	return match expr {
		ast.VariableExpr { expr.position }
		ast.LiteralExpr { expr.position }
		ast.AssignExpr { expr.position }
		ast.BinaryExpr { expr.position }
		ast.CallExpr { expr.position }
		ast.MatchExpr { expr.position }
		ast.ListConsExpr { expr.position }
		ast.ListEmptyExpr { ast.new_position(0, 0, 'unknown') }
		ast.ListLiteralExpr { expr.position }
		ast.TupleExpr { expr.position }
		ast.MapLiteralExpr { expr.position }
		ast.RecordLiteralExpr { expr.position }
		ast.RecordAccessExpr { expr.position }
		ast.FunExpr { expr.position }
		ast.SendExpr { expr.position }
		ast.ReceiveExpr { expr.position }
		ast.GuardExpr { expr.position }
		ast.UnaryExpr { expr.position }
		ast.MapAccessExpr { expr.position }
		ast.IfExpr { expr.position }
		ast.CaseExpr { expr.position }
		ast.WithExpr { expr.position }
		ast.ForExpr { expr.position }
	}
}
