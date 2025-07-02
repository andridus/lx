module erlang

import ast

// generate_expression generates code for a single expression
pub fn (gen ErlangGenerator) generate_expression(expr ast.Expr) string {
	match expr {
		ast.VariableExpr {
			return gen.generate_variable(expr.name)
		}
		ast.LiteralExpr {
			return gen.generate_literal(expr.value)
		}
		ast.BinaryExpr {
			return gen.generate_binary_expression(expr)
		}
		ast.UnaryExpr {
			return '%% UnaryExpr not implemented'
		}
		ast.CallExpr {
			return gen.generate_function_call(expr)
		}
		ast.RecordAccessExpr {
			return gen.generate_record_access(expr)
		}
		ast.MapAccessExpr {
			return '%% MapAccessExpr not implemented'
		}
		ast.TupleExpr {
			return gen.generate_tuple(expr)
		}
		ast.ListConsExpr {
			return gen.generate_list_cons(expr)
		}
		ast.ListEmptyExpr {
			return '[]'
		}
		ast.ListLiteralExpr {
			return gen.generate_list_literal(expr)
		}
		ast.MapLiteralExpr {
			return gen.generate_map_literal(expr)
		}
		ast.RecordLiteralExpr {
			return gen.generate_record_literal(expr)
		}
		ast.IfExpr {
			return gen.generate_if_expression(expr)
		}
		ast.CaseExpr {
			return gen.generate_case(expr)
		}
		ast.WithExpr {
			return gen.generate_with_expression(expr)
		}
		ast.ForExpr {
			return '%% ForExpr not implemented'
		}
		ast.ReceiveExpr {
			return gen.generate_receive(expr)
		}
		ast.GuardExpr {
			return gen.generate_guard(expr)
		}
		ast.AssignExpr {
			return gen.generate_assignment(expr)
		}
		ast.MatchExpr {
			return gen.generate_match(expr)
		}
		ast.FunExpr {
			return gen.generate_fun_expression(expr)
		}
		ast.SendExpr {
			return gen.generate_send(expr)
		}
	}
}

// generate_expression_in_guard generates code for expressions in guard context
pub fn (gen ErlangGenerator) generate_expression_in_guard(expr ast.Expr) string {
	match expr {
		ast.VariableExpr {
			return gen.generate_variable(expr.name)
		}
		ast.LiteralExpr {
			return gen.generate_literal(expr.value)
		}
		ast.BinaryExpr {
			return gen.generate_binary_expression_in_guard(expr)
		}
		ast.UnaryExpr {
			return '%% UnaryExpr not implemented'
		}
		ast.CallExpr {
			return gen.generate_function_call(expr)
		}
		ast.RecordAccessExpr {
			return gen.generate_record_access(expr)
		}
		ast.MapAccessExpr {
			return '%% MapAccessExpr not implemented'
		}
		ast.TupleExpr {
			return gen.generate_tuple(expr)
		}
		ast.ListConsExpr {
			return gen.generate_list_cons(expr)
		}
		ast.ListEmptyExpr {
			return '[]'
		}
		ast.ListLiteralExpr {
			return gen.generate_list_literal(expr)
		}
		ast.MapLiteralExpr {
			return gen.generate_map_literal(expr)
		}
		ast.RecordLiteralExpr {
			return gen.generate_record_literal(expr)
		}
		ast.IfExpr {
			return gen.generate_if_expression(expr)
		}
		ast.CaseExpr {
			return gen.generate_case(expr)
		}
		ast.WithExpr {
			return gen.generate_with_expression(expr)
		}
		ast.ForExpr {
			return '%% ForExpr not implemented'
		}
		ast.ReceiveExpr {
			return gen.generate_receive(expr)
		}
		ast.GuardExpr {
			return gen.generate_guard(expr)
		}
		ast.AssignExpr {
			return gen.generate_assignment(expr)
		}
		ast.MatchExpr {
			return gen.generate_match(expr)
		}
		ast.FunExpr {
			return gen.generate_fun_expression(expr)
		}
		ast.SendExpr {
			return gen.generate_send(expr)
		}
	}
}

// generate_variable generates code for variables
fn (gen ErlangGenerator) generate_variable(name string) string {
	return gen.capitalize_variable(name)
}

// generate_literal generates code for literal values
fn (gen ErlangGenerator) generate_literal(literal ast.Literal) string {
	match literal {
		ast.IntegerLiteral {
			return literal.value.str()
		}
		ast.FloatLiteral {
			return literal.value.str()
		}
		ast.StringLiteral {
			// Escape quotes and special characters for Erlang
			escaped := literal.value.replace('\\', '\\\\').replace('"', '\\"')
			return '"${escaped}"'
		}
		ast.BooleanLiteral {
			return if literal.value { 'true' } else { 'false' }
		}
		ast.AtomLiteral {
			// Erlang atoms don't need quotes unless they contain special characters
			if literal.value.contains(' ') || literal.value.contains('-') {
				return "'${literal.value}'"
			}
			return literal.value
		}
		ast.NilLiteral {
			return 'nil'
		}
	}
}

// capitalize_variable capitalizes the first letter for Erlang variables
fn (gen ErlangGenerator) capitalize_variable(name string) string {
	if name.len == 0 {
		return 'Var'
	}

	match name {
		'_' {
			return '_'
		}
		'__MODULE__' {
			return '?MODULE'
		}
		else {
			return name[0].ascii_str().to_upper() + name[1..]
		}
	}
}

// generate_binary_expression generates code for binary expressions
fn (gen ErlangGenerator) generate_binary_expression(expr ast.BinaryExpr) string {
	left := gen.generate_expression(expr.left)
	right := gen.generate_expression(expr.right)
	op := gen.translate_operator(expr.op, false)
	return '${left} ${op} ${right}'
}

// generate_binary_expression_in_guard generates code for binary expressions in guards
fn (gen ErlangGenerator) generate_binary_expression_in_guard(expr ast.BinaryExpr) string {
	left := gen.generate_expression_in_guard(expr.left)
	right := gen.generate_expression_in_guard(expr.right)
	op := gen.translate_operator(expr.op, true)
	return '${left} ${op} ${right}'
}

// generate_unary_expression_in_guard generates code for unary expressions in guards
fn (gen ErlangGenerator) generate_unary_expression_in_guard(expr ast.UnaryExpr) string {
	operand := gen.generate_expression_in_guard(expr.operand)
	op := gen.translate_unary_operator(expr.op, true)
	return '${op}${operand}'
}

// translate_unary_operator translates LX unary operators to Erlang operators
// is_guard indicates whether we're in a guard context
fn (gen ErlangGenerator) translate_unary_operator(operator ast.UnaryOp, is_guard bool) string {
	match operator {
		.not { return 'not' }
		.minus { return '-' }
	}
}

// translate_operator translates LX operators to Erlang operators
// is_guard indicates whether we're in a guard context
fn (gen ErlangGenerator) translate_operator(operator ast.BinaryOp, is_guard bool) string {
	match operator {
		.add {
			return '+'
		}
		.subtract {
			return '-'
		}
		.multiply {
			return '*'
		}
		.divide {
			return '/'
		}
		.modulo {
			return 'rem'
		}
		.power {
			return '**'
		}
		.equal {
			return '=:='
		}
		.not_equal {
			return '=/='
		}
		.less_than {
			return '<'
		}
		.greater_than {
			return '>'
		}
		.less_equal {
			return '=<'
		}
		.greater_equal {
			return '>='
		}
		.and {
			return if is_guard { 'andalso' } else { 'and' }
		}
		.or {
			return if is_guard { 'orelse' } else { 'or' }
		}
		.append {
			return '++'
		}
		.cons {
			return '|'
		}
	}
}

// generate_function_call generates code for function calls
fn (gen ErlangGenerator) generate_function_call(call ast.CallExpr) string {
	args := call.arguments.map(gen.generate_expression(it))
	if call.external {
		return '${call.module}:${call.function_name}(${args.join(', ')})'
	} else {
		// For internal function calls, the function name should not be capitalized
		if call.function is ast.VariableExpr {
			func_name := (call.function as ast.VariableExpr).name
			return '${func_name}(${args.join(', ')})'
		} else {
			function := gen.generate_expression(call.function)
			return '${function}(${args.join(', ')})'
		}
	}
}

// generate_assignment generates code for assignments
fn (gen ErlangGenerator) generate_assignment(assign ast.AssignExpr) string {
	value := gen.generate_expression(assign.value)
	return '${gen.capitalize_variable(assign.name)} = ${value}'
}

// generate_match generates code for pattern matching
fn (gen ErlangGenerator) generate_match(match_expr ast.MatchExpr) string {
	value := gen.generate_expression(match_expr.value)
	mut cases := []string{}

	for case_item in match_expr.cases {
		pattern := gen.generate_pattern(case_item.pattern)
		guard := if case_item.guard != ast.Expr(ast.GuardExpr{}) {
			' when ' + gen.generate_expression(case_item.guard)
		} else {
			''
		}
		body := case_item.body.map(gen.generate_statement(it))
		cases << '${pattern}${guard} ->\n${body.join(';\n')}'
	}

	return 'case ${value} of\n${cases.join(';\n')}\nend'
}

// generate_case generates code for case expressions
fn (gen ErlangGenerator) generate_case(case_expr ast.CaseExpr) string {
	subject := gen.generate_expression(case_expr.value)
	mut cases := []string{}

	for clause in case_expr.cases {
		pattern := gen.generate_pattern(clause.pattern)

		// Check if guard is a default "true" literal - if so, don't generate it
		mut guard := ''
		if clause.guard != ast.Expr(ast.GuardExpr{}) {
			if clause.guard is ast.LiteralExpr {
				lit_expr := clause.guard as ast.LiteralExpr
				if lit_expr.value is ast.BooleanLiteral {
					bool_lit := lit_expr.value as ast.BooleanLiteral
					// Only generate guard if it's not the default "true"
					if !bool_lit.value {
						guard = ' when ' + gen.generate_expression(clause.guard)
					}
				} else {
					guard = ' when ' + gen.generate_expression(clause.guard)
				}
			} else {
				guard = ' when ' + gen.generate_expression(clause.guard)
			}
		}

		body := clause.body.map(gen.generate_statement(it))
		cases << '    ${pattern}${guard} -> ${body.join('; ')}'
	}

	return 'case ${subject} of\n${cases.join(';\n')}\nend'
}

// generate_list_cons generates code for list cons
fn (gen ErlangGenerator) generate_list_cons(expr ast.ListConsExpr) string {
	head := gen.generate_expression(expr.head)
	tail := gen.generate_expression(expr.tail)
	return '[${head} | ${tail}]'
}

// generate_list_literal generates code for list literals
fn (gen ErlangGenerator) generate_list_literal(expr ast.ListLiteralExpr) string {
	elements := expr.elements.map(gen.generate_expression(it))
	return '[${elements.join(', ')}]'
}

// generate_tuple generates code for tuples
fn (gen ErlangGenerator) generate_tuple(expr ast.TupleExpr) string {
	elements := expr.elements.map(gen.generate_expression(it))
	return '{${elements.join(', ')}}'
}

// generate_map_literal generates code for map literals
fn (gen ErlangGenerator) generate_map_literal(expr ast.MapLiteralExpr) string {
	mut entries := []string{}
	for entry in expr.entries {
		key := gen.generate_expression(entry.key)
		value := gen.generate_expression(entry.value)
		entries << '${key} => ${value}'
	}
	return '#{${entries.join(', ')}}'
}

// generate_record_literal generates code for record literals
fn (gen ErlangGenerator) generate_record_literal(expr ast.RecordLiteralExpr) string {
	mut fields := []string{}
	for field in expr.fields {
		value := gen.generate_expression(field.value)
		fields << '${field.name}: ${value}'
	}
	return '#${expr.name}{${fields.join(', ')}}'
}

// generate_record_access generates code for record access
fn (gen ErlangGenerator) generate_record_access(expr ast.RecordAccessExpr) string {
	record := gen.generate_expression(expr.record)
	return '${record}#${expr.field}'
}

// generate_fun_expression generates code for fun expressions
fn (gen ErlangGenerator) generate_fun_expression(expr ast.FunExpr) string {
	parameters := expr.parameters.map(gen.generate_pattern(it))
	body := expr.body.map(gen.generate_statement(it))
	return 'fun(${parameters.join(', ')}) ->\n${body.join(';\n')}\nend'
}

// generate_send generates code for message sending
fn (gen ErlangGenerator) generate_send(expr ast.SendExpr) string {
	pid := gen.generate_expression(expr.pid)
	message := gen.generate_expression(expr.message)
	return '${pid} ! ${message}'
}

// generate_receive generates code for receive expressions
fn (gen ErlangGenerator) generate_receive(expr ast.ReceiveExpr) string {
	mut cases := []string{}
	for case_item in expr.cases {
		pattern := gen.generate_pattern(case_item.pattern)

		// Check if guard is a default "true" literal - if so, don't generate it
		mut guard := ''
		if case_item.guard != ast.Expr(ast.GuardExpr{}) {
			if case_item.guard is ast.LiteralExpr {
				lit_expr := case_item.guard as ast.LiteralExpr
				if lit_expr.value is ast.BooleanLiteral {
					bool_lit := lit_expr.value as ast.BooleanLiteral
					// Only generate guard if it's not the default "true"
					if !bool_lit.value {
						guard = ' when ' + gen.generate_expression(case_item.guard)
					}
				} else {
					guard = ' when ' + gen.generate_expression(case_item.guard)
				}
			} else {
				guard = ' when ' + gen.generate_expression(case_item.guard)
			}
		}

		body := case_item.body.map(gen.generate_statement(it))
		cases << '    ${pattern}${guard} -> ${body.join('; ')}'
	}

	timeout := if expr.timeout != ast.Expr(ast.GuardExpr{}) {
		' after ${gen.generate_expression(expr.timeout)} ->\ntimeout'
	} else {
		''
	}

	return 'receive\n${cases.join(';\n')}${timeout}\nend'
}

// generate_guard generates code for guard expressions
fn (gen ErlangGenerator) generate_guard(expr ast.GuardExpr) string {
	return gen.generate_expression_in_guard(expr.condition)
}

// generate_if_expression generates code for if expressions
fn (gen ErlangGenerator) generate_if_expression(expr ast.IfExpr) string {
	condition := gen.generate_expression(expr.condition)

	// Generate then body - get the last expression as the return value
	mut then_statements := []string{}
	for i, stmt in expr.then_body {
		if i == expr.then_body.len - 1 {
			// Last statement is the return value
			then_statements << gen.generate_statement(stmt)
		} else {
			// Non-last statements need comma separation
			then_statements << gen.generate_statement(stmt) + ','
		}
	}
	then_body := then_statements.join('\n    ')

	// Generate else body - get the last expression as the return value
	mut else_statements := []string{}
	if expr.else_body.len > 0 {
		for i, stmt in expr.else_body {
			if i == expr.else_body.len - 1 {
				// Last statement is the return value
				else_statements << gen.generate_statement(stmt)
			} else {
				// Non-last statements need comma separation
				else_statements << gen.generate_statement(stmt) + ','
			}
		}
	} else {
		// If no else body, return nil
		else_statements << 'nil'
	}
	else_body := else_statements.join('\n    ')

	// In Erlang, we use case expression to implement if-else
	return 'case ${condition} of\n    true ->\n        ${then_body};\n    false ->\n        ${else_body}\nend'
}

// generate_with_expression generates code for with expressions
fn (gen ErlangGenerator) generate_with_expression(expr ast.WithExpr) string {
	// If there are no bindings, just return the body
	if expr.bindings.len == 0 {
		body := expr.body.map(gen.generate_statement(it))
		return body.join(',\n')
	}

	// Generate nested case expressions for each binding
	return gen.generate_with_bindings(expr.bindings, expr.body, expr.else_body, 0)
}

// generate_with_bindings generates nested case expressions recursively
fn (gen ErlangGenerator) generate_with_bindings(bindings []ast.WithBinding, body []ast.Stmt, else_body []ast.Stmt, index int) string {
	if index >= bindings.len {
		// We've reached the end of bindings, generate the main body
		if body.len > 0 {
			body_code := body.map(gen.generate_statement(it))
			return body_code.join(',\n')
		}
		return 'ok'
	}

	current_binding := bindings[index]
	pattern := gen.generate_pattern(current_binding.pattern)
	value := gen.generate_expression(current_binding.value)

	// Generate the next level of bindings or the final body
	next_code := gen.generate_with_bindings(bindings, body, else_body, index + 1)

	// Generate the failure branch
	failure_branch := if else_body.len > 0 {
		else_code := else_body.map(gen.generate_statement(it))
		else_code.join(',\n')
	} else {
		// If no else_body, propagate the failed value
		'Other'
	}

	// Format the case expression with proper indentation for nested cases
	if next_code.contains('case') {
		// This is a nested case, format it properly
		indented_next := next_code.split('\n').map('        ${it}').join('\n')
		return 'case ${value} of\n    ${pattern} ->\n${indented_next};\n    Other ->\n        ${failure_branch}\nend'
	} else {
		// This is the final case, format normally
		return 'case ${value} of\n    ${pattern} ->\n        ${next_code};\n    Other ->\n        ${failure_branch}\nend'
	}
}

// indent_code adds proper indentation to code
fn (gen ErlangGenerator) indent_code(code string, level int) string {
	if code.trim_space().len == 0 {
		return code
	}

	indent := '    '.repeat(level)
	lines := code.split('\n')
	mut indented_lines := []string{}

	for line in lines {
		trimmed := line.trim_space()
		if trimmed.len > 0 {
			indented_lines << '${indent}${trimmed}'
		} else {
			indented_lines << ''
		}
	}

	return indented_lines.join('\n')
}
