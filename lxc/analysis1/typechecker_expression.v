module analysis1

import ast
import errors

pub struct ExpressionChecker {
pub mut:
	context &TypeContext
	errors  []errors.CompilationError
	type_cache map[string]TypeInfo  // Cache for expression types
}

pub fn new_expression_checker(context &TypeContext) ExpressionChecker {
	return ExpressionChecker{
		context: unsafe { context }
		errors:  []
		type_cache: map[string]TypeInfo{}
	}
}

pub fn (mut ec ExpressionChecker) check_expression(expr ast.Expr) {
	// Check the expression first
	match expr {
		ast.BinaryExpr { ec.check_binary_expression(expr) }
		ast.CallExpr { ec.check_call_expression(expr) }
		ast.VariableExpr { ec.check_variable_expression(expr) }
		ast.AssignExpr { ec.check_assignment_expression(expr) }
		ast.IfExpr { ec.check_if_expression(expr) }
		ast.CaseExpr { ec.check_case_expression(expr) }
		ast.ReceiveExpr { ec.check_receive_expression(expr) }
		ast.FunExpr { ec.check_fun_expression(expr) }
		ast.ListConsExpr { ec.check_list_cons_expression(expr) }
		ast.ListLiteralExpr { ec.check_list_literal_expression(expr) }
		ast.TupleExpr { ec.check_tuple_expression(expr) }
		ast.MapLiteralExpr { ec.check_map_literal_expression(expr) }
		ast.MapAccessExpr { ec.check_map_access_expression(expr) }
		ast.MapUpdateExpr { ec.check_map_update_expression(expr) }
		ast.RecordLiteralExpr { ec.check_record_literal_expression(expr) }
		ast.RecordAccessExpr { ec.check_record_access_expression(expr) }
		ast.RecordUpdateExpr { ec.check_record_update_expression(expr) }
		ast.LiteralExpr { ec.check_literal_expression(expr) }
		ast.BlockExpr { ec.check_block_expression(expr) }
		ast.WithExpr { ec.check_with_expression(expr) }
		ast.ForExpr { ec.check_for_expression(expr) }
		ast.SendExpr { ec.check_send_expression(expr) }
		ast.MatchExpr { ec.check_match_expression(expr) }
		ast.GuardExpr { ec.check_guard_expression(expr) }
		ast.UnaryExpr { ec.check_unary_expression(expr) }
		ast.ListEmptyExpr { /* nothing to check */ }
		ast.SimpleMatchExpr { /* nothing to check */ }
		ast.MatchRescueExpr { /* nothing to check */ }
	}

	// Store the inferred type in the context
	type_info := ec.infer_expression_type_info(expr)
	ec.context.store_expression_type(expr, type_info)
}

fn (mut ec ExpressionChecker) check_binary_expression(expr ast.BinaryExpr) {
	ec.check_expression(expr.left)
	ec.check_expression(expr.right)

	left_type := ec.infer_expression_type_info(expr.left)
	right_type := ec.infer_expression_type_info(expr.right)

	ec.validate_binary_operator(expr.op, left_type, right_type, expr.position)
}

fn (mut ec ExpressionChecker) validate_binary_operator(op ast.BinaryOp, left_type TypeInfo, right_type TypeInfo, position ast.Position) {
	match op {
		.add, .subtract, .multiply, .divide, .modulo, .power {
			ec.validate_arithmetic_operator(op, left_type, right_type, position)
		}
		.and, .or {
			ec.validate_logical_operator(op, left_type, right_type, position)
		}
		.equal, .not_equal, .less_than, .less_equal, .greater_than, .greater_equal {
			ec.validate_comparison_operator(op, left_type, right_type, position)
		}
		else {
			// Handle other operators
		}
	}
}

fn (mut ec ExpressionChecker) validate_arithmetic_operator(op ast.BinaryOp, left_type TypeInfo, right_type TypeInfo, position ast.Position) {
	if !ec.is_numeric_type_info(left_type) || !ec.is_numeric_type_info(right_type) {
		ec.report_error("Arithmetic operator '${op.str()}' requires numeric operands",
			'Both operands must be integers or floats', position)
	}
}

fn (mut ec ExpressionChecker) validate_logical_operator(op ast.BinaryOp, left_type TypeInfo, right_type TypeInfo, position ast.Position) {
	if left_type.generic != 'boolean' {
		ec.report_error('Left operand of `${op.str()}` must be boolean',
			'Use only boolean expressions with `${op.str()}`', position)
	}
	if right_type.generic != 'boolean' {
		ec.report_error('Right operand of `${op.str()}` must be boolean',
			'Use only boolean expressions with `${op.str()}`', position)
	}
}

fn (mut ec ExpressionChecker) validate_comparison_operator(op ast.BinaryOp, left_type TypeInfo, right_type TypeInfo, position ast.Position) {
	// Comparison operators work with any types, but both operands should be compatible
	if !types_are_compatible(left_type, right_type) {
		ec.report_error('Cannot compare ${left_type.str()} with ${right_type.str()}',
			'Both operands must be of compatible types', position)
	}
}

fn (mut ec ExpressionChecker) check_call_expression(expr ast.CallExpr) {
	ec.check_expression(expr.function)
	for arg in expr.arguments {
		ec.check_expression(arg)
	}
}

fn (mut ec ExpressionChecker) check_variable_expression(expr ast.VariableExpr) {
	// Variable expressions are valid if the variable is bound
	// Type checking will be done during type inference
}

fn (mut ec ExpressionChecker) check_assignment_expression(expr ast.AssignExpr) {
	ec.check_expression(expr.value)
	// Infer the type of the value and bind the variable to the context
	value_type := ec.infer_expression_type_info(expr.value)
	ec.context.bind(expr.name, value_type, expr.position)
}

fn (mut ec ExpressionChecker) check_if_expression(expr ast.IfExpr) {
	ec.check_expression(expr.condition)
	ec.check_expression(expr.then_body)
	if expr.else_body.body.len > 0 {
		ec.check_expression(expr.else_body)
	}
}

fn (mut ec ExpressionChecker) check_case_expression(expr ast.CaseExpr) {
	ec.check_expression(expr.value)
	for case_expr in expr.cases {
		ec.check_pattern(case_expr.pattern)
		ec.check_expression(case_expr.body)
	}
}

fn (mut ec ExpressionChecker) check_receive_expression(expr ast.ReceiveExpr) {
	for case_expr in expr.cases {
		ec.check_pattern(case_expr.pattern)
		ec.check_expression(case_expr.body)
	}
	if timeout := expr.timeout {
		ec.check_expression(timeout.body)
	}
}

fn (mut ec ExpressionChecker) check_fun_expression(expr ast.FunExpr) {
	for param in expr.parameters {
		ec.check_pattern(param)
	}
	ec.check_expression(expr.body)
}

fn (mut ec ExpressionChecker) check_list_cons_expression(expr ast.ListConsExpr) {
	ec.check_expression(expr.head)
	ec.check_expression(expr.tail)
}

fn (mut ec ExpressionChecker) check_list_literal_expression(expr ast.ListLiteralExpr) {
	for element in expr.elements {
		ec.check_expression(element)
	}
}

fn (mut ec ExpressionChecker) check_tuple_expression(expr ast.TupleExpr) {
	for element in expr.elements {
		ec.check_expression(element)
	}
}

fn (mut ec ExpressionChecker) check_map_literal_expression(expr ast.MapLiteralExpr) {
	for entry in expr.entries {
		ec.check_expression(entry.key)
		ec.check_expression(entry.value)
	}
}

fn (mut ec ExpressionChecker) check_map_access_expression(expr ast.MapAccessExpr) {
	ec.check_expression(expr.map_expr)
	ec.check_expression(expr.key)
}

fn (mut ec ExpressionChecker) check_map_update_expression(expr ast.MapUpdateExpr) {
	ec.check_expression(expr.base_map)
	for entry in expr.entries {
		ec.check_expression(entry.key)
		ec.check_expression(entry.value)
	}
}

fn (mut ec ExpressionChecker) check_record_literal_expression(expr ast.RecordLiteralExpr) {
	for field in expr.fields {
		ec.check_expression(field.value)
	}
}

fn (mut ec ExpressionChecker) check_record_access_expression(expr ast.RecordAccessExpr) {
	ec.check_expression(expr.record)
}

fn (mut ec ExpressionChecker) check_record_update_expression(expr ast.RecordUpdateExpr) {
	ec.check_expression(expr.base_record)
	for field in expr.fields {
		ec.check_expression(field.value)
	}
}

fn (mut ec ExpressionChecker) check_literal_expression(expr ast.LiteralExpr) {
	// Literals are always valid
}

fn (mut ec ExpressionChecker) check_block_expression(expr ast.BlockExpr) {
	for stmt in expr.body {
		ec.check_statement(stmt)
	}
}

fn (mut ec ExpressionChecker) check_with_expression(expr ast.WithExpr) {
	for binding in expr.bindings {
		ec.check_expression(binding.value)
		ec.check_pattern(binding.pattern)
	}
	ec.check_expression(expr.body)
	if expr.else_body.body.len > 0 {
		ec.check_expression(expr.else_body)
	}
}

fn (mut ec ExpressionChecker) check_for_expression(expr ast.ForExpr) {
	ec.check_expression(expr.collection)
	// Infer the type of the collection to determine the pattern variable type
	collection_type := ec.infer_expression_type_info(expr.collection)
	// Check the pattern and bind variables with appropriate types
	ec.check_pattern_with_collection_type_info(expr.pattern, collection_type)
	if expr.guard != ast.Expr{} {
		ec.check_expression(expr.guard)
	}
	ec.check_expression(expr.body)
}

fn (mut ec ExpressionChecker) check_send_expression(expr ast.SendExpr) {
	ec.check_expression(expr.pid)
	ec.check_expression(expr.message)
}

fn (mut ec ExpressionChecker) check_match_expression(expr ast.MatchExpr) {
	ec.check_expression(expr.value)
	for case_expr in expr.cases {
		ec.check_pattern(case_expr.pattern)
		ec.check_expression(case_expr.body)
	}
}

fn (mut ec ExpressionChecker) check_guard_expression(expr ast.GuardExpr) {
	ec.check_expression(expr.condition)
}

fn (mut ec ExpressionChecker) check_unary_expression(expr ast.UnaryExpr) {
	ec.check_expression(expr.operand)
}

// Pattern checking methods
fn (mut ec ExpressionChecker) check_pattern(pattern ast.Pattern) {
	match pattern {
		ast.VarPattern { ec.check_var_pattern(pattern) }
		ast.LiteralPattern { ec.check_literal_pattern(pattern) }
		ast.TuplePattern { ec.check_tuple_pattern(pattern) }
		ast.ListConsPattern { ec.check_list_cons_pattern(pattern) }
		ast.ListEmptyPattern { ec.check_list_empty_pattern(pattern) }
		ast.ListLiteralPattern { ec.check_list_literal_pattern(pattern) }
		ast.MapPattern { ec.check_map_pattern(pattern) }
		ast.RecordPattern { ec.check_record_pattern(pattern) }
		ast.BinaryPattern { ec.check_binary_pattern(pattern) }
		ast.WildcardPattern { ec.check_wildcard_pattern(pattern) }
		ast.AtomPattern { ec.check_atom_pattern(pattern) }
	}
}

fn (mut ec ExpressionChecker) check_var_pattern(pattern ast.VarPattern) {
	// Variable patterns are always valid
	// But we should bind them to the context with a type
	type_info := TypeInfo{ generic: 'var', value: pattern.name }
	ec.context.bind(pattern.name, type_info, pattern.position)
}

fn (mut ec ExpressionChecker) check_literal_pattern(pattern ast.LiteralPattern) {
	// Literal patterns are always valid
}

fn (mut ec ExpressionChecker) check_tuple_pattern(pattern ast.TuplePattern) {
	for element in pattern.elements {
		ec.check_pattern(element)
	}
}

fn (mut ec ExpressionChecker) check_list_cons_pattern(pattern ast.ListConsPattern) {
	ec.check_pattern(pattern.head)
	ec.check_pattern(pattern.tail)
}

fn (mut ec ExpressionChecker) check_list_empty_pattern(pattern ast.ListEmptyPattern) {
	// Empty list patterns are always valid
}

fn (mut ec ExpressionChecker) check_list_literal_pattern(pattern ast.ListLiteralPattern) {
	for element in pattern.elements {
		ec.check_pattern(element)
	}
}

fn (mut ec ExpressionChecker) check_map_pattern(pattern ast.MapPattern) {
	for entry in pattern.entries {
		ec.check_pattern(entry.key)
		ec.check_pattern(entry.value)
	}
}

fn (mut ec ExpressionChecker) check_record_pattern(pattern ast.RecordPattern) {
	for field in pattern.fields {
		ec.check_pattern(field.pattern)
	}
}

fn (mut ec ExpressionChecker) check_binary_pattern(pattern ast.BinaryPattern) {
	// Binary patterns are always valid
}

fn (mut ec ExpressionChecker) check_wildcard_pattern(pattern ast.WildcardPattern) {
	// Wildcard patterns are always valid
}

fn (mut ec ExpressionChecker) check_atom_pattern(pattern ast.AtomPattern) {
	// Atom patterns are always valid
}

// New TypeInfo-based pattern checking with collection type
fn (mut ec ExpressionChecker) check_pattern_with_collection_type_info(pattern ast.Pattern, collection_type TypeInfo) {
	match pattern {
		ast.VarPattern { ec.check_var_pattern_with_type(pattern, collection_type) }
		ast.TuplePattern {
			if collection_type.generic == 'tuple' {
				// Extract tuple elements and match with pattern elements
				for elem_pat in pattern.elements {
					ec.check_pattern(elem_pat)
				}
			} else if collection_type.generic == 'list' {
				// For lists, check each pattern element
				for elem_pat in pattern.elements {
					ec.check_pattern(elem_pat)
				}
			} else {
				for elem_pat in pattern.elements {
					ec.check_pattern(elem_pat)
				}
			}
		}
		ast.LiteralPattern { ec.check_literal_pattern(pattern) }
		ast.ListConsPattern { ec.check_list_cons_pattern(pattern) }
		ast.ListEmptyPattern { ec.check_list_empty_pattern(pattern) }
		ast.ListLiteralPattern { ec.check_list_literal_pattern(pattern) }
		ast.MapPattern { ec.check_map_pattern(pattern) }
		ast.RecordPattern { ec.check_record_pattern(pattern) }
		ast.BinaryPattern { ec.check_binary_pattern(pattern) }
		ast.WildcardPattern { ec.check_wildcard_pattern(pattern) }
		ast.AtomPattern { ec.check_atom_pattern(pattern) }
	}
}

fn (mut ec ExpressionChecker) check_var_pattern_with_type(pattern ast.VarPattern, collection_type TypeInfo) {
	// Determine the variable type based on the collection type
	variable_type := ec.infer_variable_type_from_collection_info(collection_type)
	// Bind the variable with the inferred type
	ec.context.bind(pattern.name, variable_type, pattern.position)
}

fn (mut ec ExpressionChecker) check_statement(stmt ast.Stmt) {
	match stmt {
		ast.ExprStmt { ec.check_expression(stmt.expr) }
		ast.FunctionStmt { ec.check_function_statement(stmt) }
		ast.RecordDefStmt { /* Record definitions don't need expression checking */ }
		ast.TypeDefStmt { /* Type definitions don't need expression checking */ }
		ast.TypeAliasStmt { /* Type alias definitions don't need expression checking */ }
		ast.ModuleStmt { /* Module statements don't need expression checking */ }
	}
}

fn (mut ec ExpressionChecker) check_function_statement(stmt ast.FunctionStmt) {
	for clause in stmt.clauses {
		for param in clause.parameters {
			ec.check_pattern(param)
		}
		ec.check_expression(clause.body)
	}
}

fn (mut ec ExpressionChecker) check_record_statement(stmt ast.RecordDefStmt) {
	// Record statements are always valid
}

fn (mut ec ExpressionChecker) check_type_statement(stmt ast.TypeDefStmt) {
	// Type statements are always valid
}

fn (mut ec ExpressionChecker) check_type_alias_statement(stmt ast.TypeAliasStmt) {
	// Type alias statements are always valid
}

// New TypeInfo-based inference function
fn (mut ec ExpressionChecker) infer_expression_type_info(expr ast.Expr) TypeInfo {
	return match expr {
		ast.VariableExpr { ec.infer_variable_type_info(expr) }
		ast.LiteralExpr { typeinfo_from_literal(expr.value) }
		ast.BinaryExpr { ec.infer_binary_type_info(expr) }
		ast.CallExpr { ec.infer_call_type_info(expr) }
		ast.TupleExpr {
			mut element_types := []string{}
			for element in expr.elements {
				element_type := ec.infer_expression_type_info(element)
				element_types << element_type.str()
			}
			return TypeInfo{ generic: 'tuple', value: '{${element_types.join(', ')}}' }
		}
		ast.RecordLiteralExpr {
			return TypeInfo{ generic: 'record', value: expr.name }
		}
		ast.ListLiteralExpr {
			if expr.elements.len == 0 {
				return TypeInfo{ generic: 'list', value: 'list(any)' }
			}

			// Check if all elements have the same type
			mut element_types := []TypeInfo{}
			for element in expr.elements {
				element_types << ec.infer_expression_type_info(element)
			}

			// If all elements are the same type, use that
			if element_types.len > 0 {
				first_type := element_types[0]
				if element_types.all(types_are_compatible(first_type, it)) {
					return TypeInfo{ generic: 'list', value: 'list(${first_type.str()})' }
				}
			}

			// Mixed types - use any
			return TypeInfo{ generic: 'list', value: 'list(any)' }
		}
		ast.MapLiteralExpr {
			if expr.entries.len == 0 {
				return TypeInfo{ generic: 'map', value: 'map(any=>any)' }
			}

			// Infer key and value types from first entry
			first_entry := expr.entries[0]
			key_type := ec.infer_expression_type_info(first_entry.key)
			value_type := ec.infer_expression_type_info(first_entry.value)

			return TypeInfo{ generic: 'map', value: 'map(${key_type.str()}=>${value_type.str()})' }
		}
		ast.BlockExpr {
			// For block expressions, return the type of the last statement
			if expr.body.len == 0 {
				return TypeInfo{ generic: 'any', value: none }
			}

			last_stmt := expr.body[expr.body.len - 1]
			return match last_stmt {
				ast.ExprStmt {
					ec.infer_expression_type_info(last_stmt.expr)
				}
				else {
					TypeInfo{ generic: 'any', value: none }
				}
			}
		}
		ast.IfExpr {
			// For if expressions, we need to check both branches
			then_type := ec.infer_expression_type_info(expr.then_body)
			else_type := ec.infer_expression_type_info(expr.else_body)

			// If both branches have the same type, use that
			if types_are_compatible(then_type, else_type) {
				return then_type
			}

			// Otherwise, use any
			return TypeInfo{ generic: 'any', value: none }
		}
		ast.CaseExpr {
			// For case expressions, check all branches
			if expr.cases.len == 0 {
				return TypeInfo{ generic: 'any', value: none }
			}

			// Use the type of the first case as reference
			first_case_type := ec.infer_expression_type_info(expr.cases[0].body)

			// Check if all cases have compatible types
			for case_expr in expr.cases {
				case_type := ec.infer_expression_type_info(case_expr.body)
				if !types_are_compatible(first_case_type, case_type) {
					return TypeInfo{ generic: 'any', value: none }
				}
			}

			return first_case_type
		}
		ast.WithExpr {
			// For with expressions, return the type of the body
			return ec.infer_expression_type_info(expr.body)
		}
		ast.ForExpr {
			// For comprehensions return list type
			return TypeInfo{ generic: 'list', value: 'list(any)' }
		}
		ast.ReceiveExpr {
			// For receive expressions, check all cases
			if expr.cases.len == 0 {
				return TypeInfo{ generic: 'any', value: none }
			}

			first_case_type := ec.infer_expression_type_info(expr.cases[0].body)
			return first_case_type
		}
		ast.FunExpr {
			// For function expressions, return function type
			return TypeInfo{ generic: 'function', value: none }
		}
		ast.SendExpr {
			// Send expressions return the message type
			return ec.infer_expression_type_info(expr.message)
		}
		ast.UnaryExpr {
			// For unary expressions, return based on operator
			operand_type := ec.infer_expression_type_info(expr.operand)
			return match expr.op {
				.not { TypeInfo{ generic: 'boolean', value: none } }
				.minus { operand_type }
			}
		}
		ast.ListConsExpr {
			// List cons returns list type
			head_type := ec.infer_expression_type_info(expr.head)
			return TypeInfo{ generic: 'list', value: 'list(${head_type.str()})' }
		}
		ast.MapAccessExpr {
			// Map access returns value type - for now return any
			return TypeInfo{ generic: 'any', value: none }
		}
		ast.MapUpdateExpr {
			// Map update returns map type - for now return any
			return TypeInfo{ generic: 'map', value: 'map(any=>any)' }
		}
		ast.RecordAccessExpr {
			// Record access returns field type - for now return any
			return TypeInfo{ generic: 'any', value: none }
		}
		ast.RecordUpdateExpr {
			// Record update returns record type
			return TypeInfo{ generic: 'record', value: expr.record_name }
		}
		ast.AssignExpr {
			// Assignment returns the value type
			return ec.infer_expression_type_info(expr.value)
		}
		ast.MatchExpr {
			// Match expressions return any for now
			return TypeInfo{ generic: 'any', value: none }
		}
		ast.GuardExpr {
			// Guard expressions return boolean
			return TypeInfo{ generic: 'boolean', value: none }
		}
		ast.ListEmptyExpr {
			// Empty list
			return TypeInfo{ generic: 'list', value: 'list(any)' }
		}
		ast.SimpleMatchExpr {
			// Simple match returns any for now
			return TypeInfo{ generic: 'any', value: none }
		}
		ast.MatchRescueExpr {
			// Match rescue returns any for now
			return TypeInfo{ generic: 'any', value: none }
		}
	}
}

// Legacy function for backwards compatibility - simplified
fn (mut ec ExpressionChecker) infer_expression_type(expr ast.Expr) TypeExpr {
	type_info := ec.infer_expression_type_info(expr)
	// Create a simple TypeConstructor based on the TypeInfo
	return make_type_constructor(type_info.generic, [])
}

// New TypeInfo-based variable type inference
fn (mut ec ExpressionChecker) infer_variable_type_info(expr ast.VariableExpr) TypeInfo {
	if binding := ec.context.lookup(expr.name) {
		return binding.type_info
	}
	return TypeInfo{ generic: 'any', value: none }
}

// Legacy function for backwards compatibility - simplified
fn (mut ec ExpressionChecker) infer_variable_type(expr ast.VariableExpr) TypeExpr {
	type_info := ec.infer_variable_type_info(expr)
	return make_type_constructor(type_info.generic, [])
}

fn (mut ec ExpressionChecker) infer_literal_type(literal ast.Literal) TypeExpr {
	return match literal {
		ast.IntegerLiteral { integer_type }
		ast.FloatLiteral { float_type }
		ast.StringLiteral { string_type }
		ast.BooleanLiteral { boolean_type }
		ast.AtomLiteral { make_type_constructor(literal.value, []) }
		ast.NilLiteral { nil_type }
	}
}

// New TypeInfo-based binary type inference
fn (mut ec ExpressionChecker) infer_binary_type_info(expr ast.BinaryExpr) TypeInfo {
	left_type := ec.infer_expression_type_info(expr.left)
	right_type := ec.infer_expression_type_info(expr.right)

	match expr.op {
		.add, .subtract, .multiply, .divide, .modulo, .power {
			return ec.infer_arithmetic_result_type_info(left_type, right_type)
		}
		.and, .or {
			return TypeInfo{ generic: 'boolean', value: none }
		}
		.equal, .not_equal, .less_than, .less_equal, .greater_than, .greater_equal {
			return TypeInfo{ generic: 'boolean', value: none }
		}
		else {
			return TypeInfo{ generic: 'any', value: none }
		}
	}
}

// Legacy function for backwards compatibility - simplified
fn (mut ec ExpressionChecker) infer_binary_type(expr ast.BinaryExpr) TypeExpr {
	type_info := ec.infer_binary_type_info(expr)
	return make_type_constructor(type_info.generic, [])
}

// New TypeInfo-based call type inference
fn (mut ec ExpressionChecker) infer_call_type_info(expr ast.CallExpr) TypeInfo {
	// For now, return any for function calls
	// This should be enhanced with proper function type inference
	return TypeInfo{ generic: 'any', value: none }
}

// Legacy function for backwards compatibility - simplified
fn (mut ec ExpressionChecker) infer_call_type(expr ast.CallExpr) TypeExpr {
	type_info := ec.infer_call_type_info(expr)
	return make_type_constructor(type_info.generic, [])
}

// New TypeInfo-based arithmetic result type inference
fn (mut ec ExpressionChecker) infer_arithmetic_result_type_info(left_type TypeInfo, right_type TypeInfo) TypeInfo {
	// Simple arithmetic type inference
	if left_type.generic == 'float' || right_type.generic == 'float' {
		return TypeInfo{ generic: 'float', value: none }
	}
	return TypeInfo{ generic: 'integer', value: none }
}

// Legacy function for backwards compatibility - simplified
fn (mut ec ExpressionChecker) infer_arithmetic_result_type(left_type TypeExpr, right_type TypeExpr) TypeExpr {
	// Simple arithmetic type inference without external dependencies
	if left_type.str().contains('float') || right_type.str().contains('float') {
		return make_type_constructor('float', [])
	}
	return make_type_constructor('integer', [])
}

// New TypeInfo-based numeric type check
fn (mut ec ExpressionChecker) is_numeric_type_info(type_info TypeInfo) bool {
	return type_info.generic == 'integer' || type_info.generic == 'float'
}

// Legacy function for backwards compatibility - simplified
fn (mut ec ExpressionChecker) is_numeric_type(type_expr TypeExpr) bool {
	type_str := type_expr.str()
	return type_str.contains('integer') || type_str.contains('float')
}

fn (mut ec ExpressionChecker) types_are_compatible(type1 TypeExpr, type2 TypeExpr) bool {
	// Simple type compatibility check
	// This should be enhanced with proper type unification
	return type1.str() == type2.str() || type1.str() == 'any' || type2.str() == 'any'
}

// TypeInfo-based compatibility check
fn (mut ec ExpressionChecker) types_are_compatible_info(type1 TypeInfo, type2 TypeInfo) bool {
	return types_are_compatible(type1, type2)
}

fn (mut ec ExpressionChecker) report_error(message string, suggestion string, position ast.Position) {
	error := errors.new_compilation_error(errors.TypeError{
		message:    message
		suggestion: suggestion
	}, position, message)
	ec.errors << error
}

// New TypeInfo-based variable type inference from collection
fn (mut ec ExpressionChecker) infer_variable_type_from_collection_info(collection_type TypeInfo) TypeInfo {
	if collection_type.generic == 'list' {
		if value := collection_type.value {
			elem_type_str := extract_list_element_type(value)
			return typeinfo_from_str(elem_type_str)
		}
		return TypeInfo{ generic: 'any', value: none }
	}
	if collection_type.generic == 'tuple' {
		return collection_type
	}
	return TypeInfo{ generic: 'any', value: none }
}

// Legacy function for backwards compatibility - simplified
fn (mut ec ExpressionChecker) infer_variable_type_from_collection(collection_type TypeExpr) TypeExpr {
	// Simple collection type inference without external dependencies
	if collection_type.str().contains('list') {
		return make_type_constructor('any', [])
	}
	if collection_type.str().contains('tuple') {
		return collection_type
	}
	return make_type_constructor('any', [])
}