module erlang

import ast

// generate_statement generates code for a single statement
pub fn (mut gen ErlangGenerator) generate_statement(stmt ast.Stmt) string {
	match stmt {
		ast.ExprStmt {
			if stmt.expr is ast.CaseExpr || stmt.expr is ast.IfExpr {
				return gen.generate_expression(stmt.expr)
			}
			return gen.generate_expression(stmt.expr)
		}
		ast.FunctionStmt {
			return gen.generate_function(stmt)
		}
		ast.ModuleStmt {
			return '%% Module statement'
		}
		ast.RecordDefStmt {
			return gen.generate_record_definition(stmt)
		}
		ast.TypeDefStmt {
			return gen.generate_type_definition(stmt)
		}
		ast.TypeAliasStmt {
			return gen.generate_type_alias(stmt)
		}
	}
}

// generate_function_body generates code for function body with special handling for match rescue and block expressions
pub fn (mut gen ErlangGenerator) generate_function_body(statements []ast.Stmt) string {
	if statements.len == 0 {
		return 'ok'
	}

	// Enter a new scope for the function body
	gen.enter_scope()

	// Check for match rescue patterns and block expressions
	mut result := []string{}
	mut i := 0

	for i < statements.len {
		stmt := statements[i]
		if stmt is ast.ExprStmt {
			expr_stmt := stmt as ast.ExprStmt
			if expr_stmt.expr is ast.SimpleMatchExpr {
				// Found a simple match - collect subsequent expressions
				simple_match := expr_stmt.expr as ast.SimpleMatchExpr

				// Find all subsequent expressions to include in the success branch
				mut subsequent_exprs := []ast.Stmt{}
				mut j := i + 1
				for j < statements.len {
					subsequent_exprs << statements[j]
					j++
				}

				// Generate the simple match with subsequent expressions
				result << gen.generate_simple_match_with_continuation(simple_match, subsequent_exprs)

				// Skip all processed statements
				i = statements.len
				continue
			} else if expr_stmt.expr is ast.MatchRescueExpr {
				// Found a match rescue - collect subsequent expressions
				match_rescue := expr_stmt.expr as ast.MatchRescueExpr

				// Find all subsequent expressions to include in the success branch
				mut subsequent_exprs := []ast.Stmt{}
				mut j := i + 1
				for j < statements.len {
					subsequent_exprs << statements[j]
					j++
				}

				// Generate the match rescue with subsequent expressions
				result << gen.generate_match_rescue_with_continuation(match_rescue, subsequent_exprs)

				// Skip all processed statements
				i = statements.len
				continue
			} else if expr_stmt.expr is ast.AssignExpr {
				// Check if this is an assignment with a block expression
				assign_expr := expr_stmt.expr as ast.AssignExpr
				if assign_expr.value is ast.BlockExpr {
					// Found a block assignment - unfold it inline
					block_expr := assign_expr.value as ast.BlockExpr
					unfolded := gen.generate_block_assignment_inline(assign_expr.name,
						block_expr)
					result << unfolded
					i++
					continue
				}
			}
		}

		// Regular statement
		result << gen.generate_statement(stmt)
		i++
	}

	// Exit the function body scope
	gen.exit_scope()

	return result.join(',\n')
}

// generate_simple_match_with_continuation generates simple match with subsequent expressions in success branch
fn (mut gen ErlangGenerator) generate_simple_match_with_continuation(expr ast.SimpleMatchExpr, subsequent_exprs []ast.Stmt) string {
	pattern := gen.generate_pattern(expr.pattern)
	value := gen.generate_expression(expr.value)

	// Generate success body with subsequent expressions
	// Use the new function body generator to handle nested match expressions correctly
	success_code := if subsequent_exprs.len > 0 {
		gen.generate_function_body(subsequent_exprs)
	} else {
		'ok'
	}

	// Format the success code with proper indentation
	formatted_success_code := success_code.split('\n').map('        ${it}').join('\n')

	// Simple match returns the original value if pattern doesn't match
	return 'case ${value} of\n    ${pattern} ->\n${formatted_success_code};\n    Other ->\n        Other\nend'
}

// generate_match_rescue_with_continuation generates match rescue with subsequent expressions in success branch
fn (mut gen ErlangGenerator) generate_match_rescue_with_continuation(expr ast.MatchRescueExpr, subsequent_exprs []ast.Stmt) string {
	pattern := gen.generate_pattern(expr.pattern)
	value := gen.generate_expression(expr.value)
	rescue_var := gen.capitalize_variable(expr.rescue_var)

	// Generate rescue body
	rescue_body := gen.generate_block_expression(expr.rescue_body)

	// Generate success body with subsequent expressions
	// Use the new function body generator to handle nested match rescue correctly
	success_code := if subsequent_exprs.len > 0 {
		gen.generate_function_body(subsequent_exprs)
	} else {
		'ok'
	}

	// Format the success code with proper indentation
	formatted_success_code := success_code.split('\n').map('        ${it}').join('\n')

	return 'case ${value} of\n    ${pattern} ->\n${formatted_success_code};\n    ${rescue_var} ->\n        ${rescue_body}\nend'
}

// infer_function_return_type infers the return type of a function based on its body
fn (gen ErlangGenerator) infer_function_return_type(clause ast.FunctionClause) string {
	if clause.body.body.len == 0 {
		return 'ok'
	}

	// Get the last statement in the body (the return value)
	last_stmt := clause.body.body[clause.body.body.len - 1]

	match last_stmt {
		ast.ExprStmt {
			inferred_type := gen.infer_expression_return_type_with_context(last_stmt.expr,
				clause.parameters)
			// Check if the inferred type matches any defined type
			matching_type := gen.find_matching_defined_type(inferred_type)
			return if matching_type != '' { matching_type } else { inferred_type }
		}
		else {
			'ok'
		}
	}
	// fallback return (should never reach here)
	return 'ok'
}

// find_matching_defined_type checks if a type expression matches any defined type
fn (gen ErlangGenerator) find_matching_defined_type(type_expr string) string {
	// Check each defined type to see if it matches the inferred type
	for type_name, type_def in gen.defined_types {
		defined_type_str := gen.generate_type_expression(type_def.type_expr)
		if type_expr == defined_type_str {
			// Found a match - return the type name
			return '${type_name}()'
		}
	}
	return ''
}

// infer_expression_return_type infers the return type of an expression
fn (gen ErlangGenerator) infer_expression_return_type(expr ast.Expr) string {
	return gen.infer_expression_return_type_with_context(expr, [])
}

// infer_expression_return_type_with_context infers the return type of an expression with parameter context
fn (gen ErlangGenerator) infer_expression_return_type_with_context(expr ast.Expr, parameters []ast.Pattern) string {
	return match expr {
		ast.LiteralExpr {
			gen.infer_literal_return_type(expr.value)
		}
		ast.VariableExpr {
			// Try to find the variable in parameters to get its type
			for param in parameters {
				match param {
					ast.VarPattern {
						if param.name == expr.name {
							if type_ann := param.type_annotation {
								return gen.generate_type_expression(type_ann)
							}
						}
					}
					else {}
				}
			}
			// If not found in parameters, use generic type
			'${expr.name}()'
		}
		ast.TupleExpr {
			mut element_types := []string{}
			for element in expr.elements {
				element_types << gen.infer_expression_return_type_with_context(element,
					parameters)
			}
			'{${element_types.join(', ')}}'
		}
		ast.ListLiteralExpr {
			if expr.elements.len == 0 {
				'[]'
			} else {
				element_type := gen.infer_expression_return_type_with_context(expr.elements[0],
					parameters)
				'[${element_type}]'
			}
		}
		ast.BinaryExpr {
			gen.infer_binary_expression_return_type_with_context(expr, parameters)
		}
		ast.CallExpr {
			'${expr.function_name}()'
		}
		ast.IfExpr {
			gen.infer_if_expression_return_type(expr, parameters)
		}
		ast.CaseExpr {
			gen.infer_case_expression_return_type(expr, parameters)
		}
		ast.WithExpr {
			gen.infer_with_expression_return_type(expr, parameters)
		}
		else {
			'any()'
		}
	}
}

// extract_var_from_pattern extracts variable name from a pattern if it's a VarPattern
fn (gen ErlangGenerator) extract_var_from_pattern(pattern ast.Pattern) ?string {
	match pattern {
		ast.VarPattern {
			return pattern.name
		}
		else {
			return none
		}
	}
}

// infer_literal_return_type infers the return type of a literal
fn (gen ErlangGenerator) infer_literal_return_type(literal ast.Literal) string {
	return match literal {
		ast.IntegerLiteral {
			'integer()'
		}
		ast.FloatLiteral {
			'float()'
		}
		ast.StringLiteral {
			'string()'
		}
		ast.BooleanLiteral {
			'boolean()'
		}
		ast.AtomLiteral {
			'atom()'
		}
		ast.NilLiteral {
			'nil'
		}
	}
}

// infer_binary_expression_return_type infers the return type of a binary expression
fn (gen ErlangGenerator) infer_binary_expression_return_type(expr ast.BinaryExpr) string {
	return gen.infer_binary_expression_return_type_with_context(expr, [])
}

// infer_binary_expression_return_type_with_context infers the return type of a binary expression with parameter context
fn (gen ErlangGenerator) infer_binary_expression_return_type_with_context(expr ast.BinaryExpr, parameters []ast.Pattern) string {
	return match expr.op {
		.add, .subtract, .multiply, .divide, .modulo, .power {
			// Arithmetic operators - check if both operands are numeric
			left_type := gen.infer_expression_return_type_with_context(expr.left, parameters)
			right_type := gen.infer_expression_return_type_with_context(expr.right, parameters)

			if left_type == 'integer()' && right_type == 'integer()' {
				'integer()'
			} else if left_type == 'float()' || right_type == 'float()' {
				'float()'
			} else {
				'any()'
			}
		}
		.equal, .not_equal, .less_than, .less_equal, .greater_than, .greater_equal {
			'boolean()'
		}
		.and, .or {
			'boolean()'
		}
		.cons {
			element_type := gen.infer_expression_return_type_with_context(expr.left, parameters)
			'[${element_type}]'
		}
		.append {
			gen.infer_expression_return_type_with_context(expr.left, parameters)
		}
	}
}

// generate_function generates code for a function, opening a new scope for each clause
pub fn (mut gen ErlangGenerator) generate_function(func ast.FunctionStmt) string {
	// Group clauses by arity
	mut clauses_by_arity := map[int][]ast.FunctionClause{}

	for clause in func.clauses {
		arity := clause.parameters.len
		if arity !in clauses_by_arity {
			clauses_by_arity[arity] = []ast.FunctionClause{}
		}
		clauses_by_arity[arity] << clause
	}

	mut function_definitions := []string{}

	// Generate separate function definitions for each arity
	for clauses in clauses_by_arity.values() {
		mut clause_strings := []string{}

		// --- SPEC GENERATION ---
		// Use the first clause for the spec (all clauses must have same arity)
		first_clause := clauses[0]
		mut param_types := []string{}
		for param in first_clause.parameters {
			match param {
				ast.VarPattern {
					if type_ann := param.type_annotation {
						param_types << gen.generate_type_expression(type_ann)
					} else {
						param_types << 'any()'
					}
				}
				else {
					param_types << 'any()'
				}
			}
		}
		// Infer return type from function body
		return_type := gen.infer_function_return_type(first_clause)
		spec_line := '-spec ${func.name}(${param_types.join(', ')}) -> ${return_type}.\n'
		// --- END SPEC GENERATION ---

		for clause in clauses {
			gen.enter_scope()
			// Bind all parameters as non-hashed (is_param = true)
			for param in clause.parameters {
				match param {
					ast.VarPattern {
						_ := gen.bind_variable(param.name, true)
					}
					else {}
				}
			}
			parameters := clause.parameters.map(gen.generate_pattern(it))
			mut guard := ''
			if clause.guard is ast.LiteralExpr {
				literal := clause.guard as ast.LiteralExpr
				if literal.value is ast.BooleanLiteral {
					boolean := literal.value as ast.BooleanLiteral
					if !boolean.value {
						guard = ' when ' + gen.generate_expression_in_guard(clause.guard)
					}
				} else {
					guard = ' when ' + gen.generate_expression_in_guard(clause.guard)
				}
			} else {
				guard = ' when ' + gen.generate_expression_in_guard(clause.guard)
			}
			// Use the new function body generator that handles match rescue and block assignments
			body_code := gen.generate_function_body(clause.body.body)
			gen.exit_scope()
			clause_strings << '${func.name}(${parameters.join(', ')})${guard} ->\n${body_code}'
		}

		// Add spec line before function definition
		function_definitions << spec_line
		function_definitions << clause_strings.join(';\n') + '.'
		function_definitions << '\n'
	}
	return function_definitions.join('')
}

// generate_record_definition generates code for record definitions
fn (gen ErlangGenerator) generate_record_definition(record_def ast.RecordDefStmt) string {
	mut fields := []string{}
	for field in record_def.fields {
		fields << '${field.name}'
	}
	return '-record(${record_def.name}, {${fields.join(', ')}}).'
}

// generate_type_definition generates code for type definitions
fn (gen ErlangGenerator) generate_type_definition(type_def ast.TypeDefStmt) string {
	return '%% Type definition: ${type_def.name}'
}

// generate_type_alias generates code for type alias definitions
fn (gen ErlangGenerator) generate_type_alias(type_alias ast.TypeAliasStmt) string {
	type_str := gen.generate_type_expression(type_alias.type_expr)

	return match type_alias.alias_type {
		.regular { '-type ${type_alias.name}() :: ${type_str}.' }
		.opaque { '-opaque ${type_alias.name}() :: ${type_str}.' }
		.nominal { '-nominal ${type_alias.name}() :: ${type_str}.' }
	}
}

// generate_type_expression generates code for type expressions
fn (gen ErlangGenerator) generate_type_expression(type_expr ast.TypeExpression) string {
	return match type_expr {
		ast.SimpleTypeExpr {
			match type_expr.name {
				'integer' { 'integer()' }
				'float' { 'float()' }
				'string' { 'string()' }
				'boolean' { 'boolean()' }
				'atom' { 'atom()' }
				'nil' { 'nil' }
				'any' { 'any()' }
				else { '${type_expr.name}()' }
			}
		}
		ast.UnionTypeExpr {
			types := type_expr.types.map(gen.generate_type_expression(it))
			types.join(' | ')
		}
		ast.ListTypeExpr {
			element_type := gen.generate_type_expression(type_expr.element_type)
			'[${element_type}]'
		}
		ast.TupleTypeExpr {
			element_types := type_expr.element_types.map(gen.generate_type_expression(it))
			'{${element_types.join(', ')}}'
		}
		ast.MapTypeExpr {
			key_type := gen.generate_type_expression(type_expr.key_type)
			value_type := gen.generate_type_expression(type_expr.value_type)
			'#{${key_type} => ${value_type}}'
		}
		ast.FunctionTypeExpr {
			param_types := type_expr.param_types.map(gen.generate_type_expression(it))
			return_type := gen.generate_type_expression(type_expr.return_type)
			'fun((${param_types.join(', ')}) -> ${return_type})'
		}
		ast.VariableTypeExpr {
			type_expr.name
		}
	}
}

// infer_if_expression_return_type infers the return type of an if expression
fn (gen ErlangGenerator) infer_if_expression_return_type(expr ast.IfExpr, parameters []ast.Pattern) string {
	// Infer the type from the then branch
	then_type := gen.infer_block_return_type(expr.then_body, parameters)

	// Infer the type from the else branch
	else_type := gen.infer_block_return_type(expr.else_body, parameters)

	// If both branches have the same type, return that type
	if then_type == else_type {
		return then_type
	}

	// If one is nil and the other is concrete, return the concrete type
	if else_type == 'nil' && then_type != 'any()' {
		return then_type
	}

	if then_type == 'nil' && else_type != 'any()' {
		return else_type
	}

	// For different types, return the then type (first branch takes precedence)
	if then_type != 'any()' {
		return then_type
	}

	if else_type != 'any()' {
		return else_type
	}

	// If both are any(), return any()
	return 'any()'
}

// infer_case_expression_return_type infers the return type of a case expression
fn (gen ErlangGenerator) infer_case_expression_return_type(expr ast.CaseExpr, parameters []ast.Pattern) string {
	if expr.cases.len == 0 {
		return 'nil'
	}

	// Infer types from all case branches
	mut case_types := []string{}
	for case_clause in expr.cases {
		case_type := gen.infer_block_return_type(case_clause.body, parameters)
		case_types << case_type
	}

	// If all cases have the same type, return that type
	if case_types.len > 0 {
		first_type := case_types[0]
		mut all_same := true
		for case_type in case_types[1..] {
			if case_type != first_type {
				all_same = false
				break
			}
		}
		if all_same && first_type != 'any()' {
			return first_type
		}
	}

	// Find the most specific common type
	mut result_type := 'nil'
	for case_type in case_types {
		if case_type != 'nil' && case_type != 'any()' {
			if result_type == 'nil' {
				result_type = case_type
			} else if result_type != case_type {
				// Different types, use precedence: string() > integer() > float() > boolean() > atom()
				precedence_map := {
					'string()':  5
					'integer()': 4
					'float()':   3
					'boolean()': 2
					'atom()':    1
				}

				result_precedence := precedence_map[result_type] or { 0 }
				case_precedence := precedence_map[case_type] or { 0 }

				if case_precedence > result_precedence {
					result_type = case_type
				}
			}
		}
	}

	return if result_type == 'nil' { 'any()' } else { result_type }
}

// infer_with_expression_return_type infers the return type of a with expression
fn (gen ErlangGenerator) infer_with_expression_return_type(expr ast.WithExpr, parameters []ast.Pattern) string {
	// If there are no bindings, infer from the body
	if expr.bindings.len == 0 {
		return gen.infer_block_return_type(expr.body, parameters)
	}

	// Infer the type from the success body
	success_type := gen.infer_block_return_type(expr.body, parameters)

	// Infer the type from the else body
	else_type := gen.infer_block_return_type(expr.else_body, parameters)

	// If both branches have the same concrete type, return that type
	if success_type == else_type && success_type != 'any()' {
		return success_type
	}

	// If both are string types, return string
	if success_type == 'string()' && else_type == 'string()' {
		return 'string()'
	}

	// If one is string and the other is any(), return string
	if success_type == 'string()' || else_type == 'string()' {
		return 'string()'
	}

	// If one is specific and the other is any(), return the specific one
	if success_type != 'any()' && else_type == 'any()' {
		return success_type
	}

	if else_type != 'any()' && success_type == 'any()' {
		return else_type
	}

	// For different concrete types, use precedence: string() > integer() > float() > boolean() > atom()
	if success_type != 'any()' && else_type != 'any()' {
		precedence_map := {
			'string()':  5
			'integer()': 4
			'float()':   3
			'boolean()': 2
			'atom()':    1
		}

		success_precedence := precedence_map[success_type] or { 0 }
		else_precedence := precedence_map[else_type] or { 0 }

		if success_precedence >= else_precedence {
			return success_type
		} else {
			return else_type
		}
	}

	// For different types, return any()
	return 'any()'
}

// infer_block_return_type infers the return type of a block expression
fn (gen ErlangGenerator) infer_block_return_type(block ast.BlockExpr, parameters []ast.Pattern) string {
	if block.body.len == 0 {
		return 'nil'
	}

	// Get the type of the last statement in the block
	last_stmt := block.body[block.body.len - 1]
	if last_stmt is ast.ExprStmt {
		return gen.infer_expression_return_type_with_context(last_stmt.expr, parameters)
	}

	return 'nil'
}
