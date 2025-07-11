module analysis

import ast
import errors

pub struct ExpressionChecker {
pub mut:
	context    &TypeContext
	errors     []errors.CompilationError
	type_cache map[string]TypeInfo // Cache for expression types
}

pub fn new_expression_checker(context &TypeContext) ExpressionChecker {
	return ExpressionChecker{
		context:    unsafe { context }
		errors:     []
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
		ast.ListEmptyExpr {}
		ast.SimpleMatchExpr {}
		ast.MatchRescueExpr {}
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
		ec.report_error('Left operand of `${op.str()}` must be boolean', 'Use only boolean expressions with `${op.str()}`',
			position)
	}
	if right_type.generic != 'boolean' {
		ec.report_error('Right operand of `${op.str()}` must be boolean', 'Use only boolean expressions with `${op.str()}`',
			position)
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
		// Infer the type of the value to determine variable types in the pattern
		value_type := ec.infer_expression_type_info(binding.value)
		ec.check_pattern_with_value_type(binding.pattern, value_type)
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
	// Variable patterns should be bound with the correct type from context
	// For now, use any() as default, but this should be enhanced with proper type inference
	type_info := TypeInfo{
		generic: 'any'
		value:   none
	}
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
		ast.VarPattern {
			ec.check_var_pattern_with_type(pattern, collection_type)
		}
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
		ast.LiteralPattern {
			ec.check_literal_pattern(pattern)
		}
		ast.ListConsPattern {
			ec.check_list_cons_pattern(pattern)
		}
		ast.ListEmptyPattern {
			ec.check_list_empty_pattern(pattern)
		}
		ast.ListLiteralPattern {
			ec.check_list_literal_pattern(pattern)
		}
		ast.MapPattern {
			ec.check_map_pattern(pattern)
		}
		ast.RecordPattern {
			ec.check_record_pattern(pattern)
		}
		ast.BinaryPattern {
			ec.check_binary_pattern(pattern)
		}
		ast.WildcardPattern {
			ec.check_wildcard_pattern(pattern)
		}
		ast.AtomPattern {
			ec.check_atom_pattern(pattern)
		}
	}
}

fn (mut ec ExpressionChecker) check_var_pattern_with_type(pattern ast.VarPattern, value_type TypeInfo) {
	// For variable patterns, bind the variable with the value type
	ec.context.bind(pattern.name, value_type, pattern.position)
}

fn (mut ec ExpressionChecker) check_pattern_with_value_type(pattern ast.Pattern, value_type TypeInfo) {
	match pattern {
		ast.VarPattern {
			ec.check_var_pattern_with_type(pattern, value_type)
		}
		ast.TuplePattern {
			ec.check_tuple_pattern_with_value_type(pattern, value_type)
		}
		ast.LiteralPattern {
			ec.check_literal_pattern(pattern)
		}
		ast.ListConsPattern {
			ec.check_list_cons_pattern(pattern)
		}
		ast.ListEmptyPattern {
			ec.check_list_empty_pattern(pattern)
		}
		ast.ListLiteralPattern {
			ec.check_list_literal_pattern(pattern)
		}
		ast.MapPattern {
			ec.check_map_pattern(pattern)
		}
		ast.RecordPattern {
			ec.check_record_pattern(pattern)
		}
		ast.BinaryPattern {
			ec.check_binary_pattern(pattern)
		}
		ast.WildcardPattern {
			ec.check_wildcard_pattern(pattern)
		}
		ast.AtomPattern {
			ec.check_atom_pattern(pattern)
		}
	}
}

fn (mut ec ExpressionChecker) check_tuple_pattern_with_value_type(pattern ast.TuplePattern, value_type TypeInfo) {
	if value_type.generic == 'tuple' {
		// Extract tuple elements from the value type
		if value := value_type.value {
			// Parse the tuple value to get element types
			element_types := ec.extract_tuple_element_types(value)

			// Match pattern elements with value elements
			for i, elem_pat in pattern.elements {
				if i < element_types.len {
					// Bind variable patterns with the corresponding element type
					match elem_pat {
						ast.VarPattern {
							ec.context.bind(elem_pat.name, element_types[i], elem_pat.position)
						}
						else {
							ec.check_pattern(elem_pat)
						}
					}
				} else {
					ec.check_pattern(elem_pat)
				}
			}
		} else {
			// No specific tuple value, check patterns normally
			for elem_pat in pattern.elements {
				ec.check_pattern(elem_pat)
			}
		}
	} else {
		// Not a tuple, check patterns normally
		for elem_pat in pattern.elements {
			ec.check_pattern(elem_pat)
		}
	}
}

fn (mut ec ExpressionChecker) check_statement(stmt ast.Stmt) {
	match stmt {
		ast.ExprStmt { ec.check_expression(stmt.expr) }
		ast.FunctionStmt { ec.check_function_statement(stmt) }
		ast.RecordDefStmt {}
		ast.TypeDefStmt {}
		ast.TypeAliasStmt {}
		ast.ModuleStmt {}
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
		ast.VariableExpr {
			result := ec.infer_variable_type_info(expr)
			result
		}
		ast.LiteralExpr {
			typeinfo_from_literal(expr.value)
		}
		ast.BinaryExpr {
			ec.infer_binary_type_info(expr)
		}
		ast.CallExpr {
			ec.infer_call_type_info(expr)
		}
		ast.TupleExpr {
			mut element_types := []TypeInfo{}
			for element in expr.elements {
				element_type := ec.infer_expression_type_info(element)
				element_types << element_type
			}
			typeinfo_tuple(element_types)
		}
		ast.RecordLiteralExpr {
			typeinfo_record(expr.name)
		}
		ast.ListLiteralExpr {
			if expr.elements.len == 0 {
				return typeinfo_list(typeinfo_any())
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
					return typeinfo_list(first_type)
				}
			}

			// Mixed types - use any
			return typeinfo_list(typeinfo_any())
		}
		ast.MapLiteralExpr {
			if expr.entries.len == 0 {
				return typeinfo_map(typeinfo_any(), typeinfo_any())
			}

			// Infer key and value types from first entry
			first_entry := expr.entries[0]
			key_type := ec.infer_expression_type_info(first_entry.key)
			value_type := ec.infer_expression_type_info(first_entry.value)

			return typeinfo_map(key_type, value_type)
		}
		ast.BlockExpr {
			// For block expressions, return the type of the last statement
			if expr.body.len == 0 {
				return typeinfo_any()
			}

			last_stmt := expr.body[expr.body.len - 1]
			return match last_stmt {
				ast.ExprStmt {
					ec.infer_expression_type_info(last_stmt.expr)
				}
				else {
					typeinfo_any()
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
			return typeinfo_any()
		}
		ast.CaseExpr {
			if expr.cases.len == 0 {
				return typeinfo_any()
			}

			mut branch_types := []TypeInfo{}
			for case_expr in expr.cases {
				case_type := ec.infer_expression_type_info(case_expr.body)
				branch_types << case_type
			}

			all_same_generic := branch_types.all(it.generic == branch_types[0].generic)
			if all_same_generic {
				// Check if we have exactly 2 branches with different literal values
				if branch_types.len == 2 {
					if value0 := branch_types[0].value {
						if value1 := branch_types[1].value {
							if value0 != value1 {
								return typeinfo_union(branch_types)
							}
							return branch_types[0]
						}
					}
					return TypeInfo{
						generic: branch_types[0].generic
						value:   none
						values:  []
					}
				}

				// For more than 2 branches, check if all have literal values
				all_have_literals := branch_types.all(it.value != none)
				if all_have_literals {
					// Create union of all literal values
					return typeinfo_union(branch_types)
				}

				return TypeInfo{
					generic: branch_types[0].generic
					value:   none
					values:  []
				}
			}

			mut seen := map[string]bool{}
			mut unique_generics := []string{}
			for t in branch_types {
				if !seen[t.generic] {
					unique_generics << t.generic
					seen[t.generic] = true
				}
			}
			return typeinfo_union(branch_types)
		}
		ast.WithExpr {
			// For with expressions, check both body and else_body
			body_type := ec.infer_expression_type_info(expr.body)

			// Check if else_body has statements
			if expr.else_body.body.len > 0 {
				else_type := ec.infer_expression_type_info(expr.else_body)

				// If both have the same generic type
				if body_type.generic == else_type.generic {
					// Check if both have literal values
					if body_type.value != none && else_type.value != none {
						if body_type.value != else_type.value {
							// Different literal values, create union
							return typeinfo_union([body_type, else_type])
						}
						// Same literal value
						return body_type
					}
					// At least one is generic, return generic type
					return TypeInfo{
						generic: body_type.generic
						value:   none
						values:  []
					}
				} else {
					// Different generic types, create union
					return typeinfo_union([body_type, else_type])
				}
			}

			// No else_body, return body type
			return body_type
		}
		ast.ForExpr {
			// For comprehensions return list type
			return typeinfo_list(typeinfo_any())
		}
		ast.ReceiveExpr {
			// For receive expressions, check all cases
			if expr.cases.len == 0 {
				return typeinfo_any()
			}

			first_case_type := ec.infer_expression_type_info(expr.cases[0].body)
			return first_case_type
		}
		ast.FunExpr {
			// For function expressions, return function type
			return TypeInfo{
				generic: 'function'
				value:   none
				values:  []
			}
		}
		ast.SendExpr {
			// Send expressions return the message type
			return ec.infer_expression_type_info(expr.message)
		}
		ast.UnaryExpr {
			// For unary expressions, return based on operator
			operand_type := ec.infer_expression_type_info(expr.operand)
			return match expr.op {
				.not {
					typeinfo_boolean()
				}
				.minus {
					operand_type
				}
			}
		}
		ast.ListConsExpr {
			// List cons returns list type
			head_type := ec.infer_expression_type_info(expr.head)
			return typeinfo_list(head_type)
		}
		ast.MapAccessExpr {
			// Map access returns value type - for now return any
			return typeinfo_any()
		}
		ast.MapUpdateExpr {
			// Map update returns map type - for now return any
			return typeinfo_map(typeinfo_any(), typeinfo_any())
		}
		ast.RecordAccessExpr {
			// Record access returns field type - for now return any
			return typeinfo_any()
		}
		ast.RecordUpdateExpr {
			// Record update returns record type
			return typeinfo_record(expr.record_name)
		}
		ast.AssignExpr {
			// Assignment returns the value type
			return ec.infer_expression_type_info(expr.value)
		}
		ast.MatchExpr {
			// Match expressions return any for now
			return typeinfo_any()
		}
		ast.GuardExpr {
			// Guard expressions return boolean
			return typeinfo_boolean()
		}
		ast.ListEmptyExpr {
			// Empty list
			return typeinfo_list(typeinfo_any())
		}
		ast.SimpleMatchExpr {
			// Simple match returns any for now
			return typeinfo_any()
		}
		ast.MatchRescueExpr {
			// Match rescue returns any for now
			return typeinfo_any()
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
	return typeinfo_any()
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
			return typeinfo_boolean()
		}
		.equal, .not_equal, .less_than, .less_equal, .greater_than, .greater_equal {
			return typeinfo_boolean()
		}
		else {
			return typeinfo_any()
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
	// Try to get the function return type from the type context
	if expr.function is ast.VariableExpr {
		func_name := (expr.function as ast.VariableExpr).name
		arity := expr.arguments.len

		if return_type := ec.context.get_function_return_type(func_name, arity) {
			return return_type
		}
	} else if expr.function is ast.LiteralExpr {
		literal := expr.function as ast.LiteralExpr
		if literal.value is ast.AtomLiteral {
			atom := literal.value as ast.AtomLiteral
			func_name := atom.value
			arity := expr.arguments.len

			if return_type := ec.context.get_function_return_type(func_name, arity) {
				return return_type
			}
		}
	} else if expr.function is ast.BinaryExpr {
	} else if expr.function is ast.CallExpr {
	} else {
	}

	// Fallback to any for function calls
	return typeinfo_any()
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
		return typeinfo_float()
	}
	return typeinfo_integer()
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
	if collection_type.generic == 'list' && collection_type.values.len > 0 {
		return collection_type.values[0]
	}
	if collection_type.generic == 'tuple' {
		return collection_type
	}
	return typeinfo_any()
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

fn (mut ec ExpressionChecker) extract_tuple_element_types(tuple_value string) []TypeInfo {
	// Parse tuple value like "{integer(1), string("hello")}" to get element types
	if !tuple_value.starts_with('{') || !tuple_value.ends_with('}') {
		return []
	}

	inner_content := tuple_value[1..tuple_value.len - 1]
	if inner_content.len == 0 {
		return []
	}

	// Split by comma, but be careful with nested structures and quoted strings
	mut elements := []string{}
	mut current_element := ''
	mut paren_count := 0
	mut in_quotes := false
	mut quote_char := `"`

	for i := 0; i < inner_content.len; i++ {
		ch := inner_content[i]

		// Handle quotes
		if ch == `"` || ch == `'` {
			if !in_quotes {
				in_quotes = true
				quote_char = ch
			} else if ch == quote_char {
				in_quotes = false
			}
		}

		// Handle parentheses only when not in quotes
		if !in_quotes {
			if ch == `(` {
				paren_count++
			} else if ch == `)` {
				paren_count--
			} else if ch == `,` && paren_count == 0 {
				elements << current_element.trim_space()
				current_element = ''
				continue
			}
		}

		current_element += ch.ascii_str()
	}

	if current_element.len > 0 {
		elements << current_element.trim_space()
	}

	// Convert element strings to TypeInfo
	mut result := []TypeInfo{}
	for element in elements {
		type_info := typeinfo_from_basic_string(element)
		result << type_info
	}

	return result
}
