module erlang

import ast

// Erlang operator precedence levels (higher number = higher precedence)
enum ErlangPrecedence {
	or_       = 1  // or, orelse
	and_      = 2  // and, andalso
	equality  = 3  // ==, /=, =:=, =/=, <, >, =<, >=
	list_cons = 4  // |
	add_sub   = 5  // +, -
	mul_div   = 6  // *, /, rem
	unary     = 7  // -, not
	send      = 8  // !
	call      = 9  // function calls
	primary   = 10 // literals, variables, parentheses
}

// get_expression_precedence returns the Erlang precedence level for an expression
fn get_expression_precedence(expr ast.Expr) ErlangPrecedence {
	match expr {
		ast.BinaryExpr {
			match expr.op {
				.or { return .or_ }
				.and { return .and_ }
				.equal, .not_equal, .less_than, .greater_than, .less_equal, .greater_equal { return .equality }
				.cons { return .list_cons }
				.add, .subtract { return .add_sub }
				.multiply, .divide, .modulo, .power { return .mul_div }
				.append { return .add_sub }
				.bitwise_and, .bitwise_or, .bitwise_xor, .bitwise_not, .lshift, .rshift { return .mul_div }
			}
		}
		ast.UnaryExpr {
			return .unary
		}
		ast.SendExpr {
			return .send
		}
		ast.CallExpr {
			return .call
		}
		ast.LiteralExpr, ast.VariableExpr, ast.TupleExpr, ast.ListLiteralExpr, ast.MapLiteralExpr {
			return .primary
		}
		ast.MapAccessExpr, ast.MapUpdateExpr {
			return .primary
		}
		ast.RecordLiteralExpr, ast.RecordAccessExpr {
			return .primary
		}
		ast.IfExpr {
			return .primary
		}
		ast.CaseExpr {
			return .primary
		}
		ast.WithExpr {
			return .primary
		}
		ast.ForExpr {
			return .primary
		}
		ast.ReceiveExpr {
			return .primary
		}
		ast.GuardExpr {
			return .primary
		}
		ast.AssignExpr {
			return .primary
		}
		ast.MatchExpr {
			return .primary
		}
		ast.FunExpr {
			return .primary
		}
		ast.SimpleMatchExpr {
			return .primary
		}
		ast.MatchRescueExpr {
			return .primary
		}
		ast.BlockExpr {
			return .primary
		}
		ast.RecordUpdateExpr {
			return .primary
		}
		else {
			return .primary
		}
	}
}

// generate_expression_with_precedence generates code for an expression, adding parentheses if needed
pub fn (mut gen ErlangGenerator) generate_expression_with_precedence(expr ast.Expr, parent_precedence ErlangPrecedence) string {
	expr_precedence := get_expression_precedence(expr)
	code := gen.generate_expression(expr)

	// Add parentheses if this expression has lower precedence than the parent
	if int(expr_precedence) < int(parent_precedence) {
		return '(${code})'
	}

	return code
}

// generate_expression generates code for a single expression
pub fn (mut gen ErlangGenerator) generate_expression(expr ast.Expr) string {
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
			return gen.generate_unary_expression(expr)
		}
		ast.CallExpr {
			return gen.generate_function_call(expr)
		}
		ast.RecordAccessExpr {
			return gen.generate_record_access(expr)
		}
		ast.MapAccessExpr {
			return gen.generate_map_access(expr)
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
		ast.MapUpdateExpr {
			return gen.generate_map_update(expr)
		}
		ast.RecordLiteralExpr {
			return gen.generate_record_literal(expr)
		}
		ast.RecordUpdateExpr {
			return gen.generate_record_update(expr)
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
			return gen.generate_for_expression(expr)
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
		ast.BinaryPatternExpr {
			return gen.generate_binary_pattern_expression(expr)
		}
		ast.SendExpr {
			return gen.generate_send(expr)
		}
		ast.SimpleMatchExpr {
			return gen.generate_simple_match(expr)
		}
		ast.MatchRescueExpr {
			return gen.generate_match_rescue(expr)
		}
		ast.BlockExpr {
			return gen.generate_block_expression(expr)
		}
	}
}

// generate_expression_in_guard generates code for expressions in guard context
pub fn (mut gen ErlangGenerator) generate_expression_in_guard(expr ast.Expr) string {
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
			return gen.generate_unary_expression_in_guard(expr)
		}
		ast.CallExpr {
			return gen.generate_function_call(expr)
		}
		ast.RecordAccessExpr {
			return gen.generate_record_access(expr)
		}
		ast.MapAccessExpr {
			return gen.generate_map_access(expr)
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
		ast.MapUpdateExpr {
			return gen.generate_map_update(expr)
		}
		ast.RecordLiteralExpr {
			return gen.generate_record_literal(expr)
		}
		ast.RecordUpdateExpr {
			return gen.generate_record_update(expr)
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
		ast.SimpleMatchExpr {
			return gen.generate_simple_match(expr)
		}
		ast.MatchRescueExpr {
			return gen.generate_match_rescue(expr)
		}
		ast.BlockExpr {
			return gen.generate_block_expression(expr)
		}
		ast.BinaryPatternExpr {
			return gen.generate_binary_pattern_expression(expr)
		}
	}
}

// generate_variable generates code for variables (reads)
fn (gen ErlangGenerator) generate_variable(name string) string {
	// Always look up the hashed name for this variable
	return gen.lookup_variable(name)
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
			return '<<"${escaped}"/utf8>>'
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
fn (mut gen ErlangGenerator) generate_binary_expression(expr ast.BinaryExpr) string {
	if expr.op == .append {
		left_code := gen.generate_expression(expr.left)
		right_code := gen.generate_expression(expr.right)
		is_left_empty := left_code == '<<""/utf8>>'
		is_right_empty := right_code == '<<""/utf8>>'

		if is_left_empty && is_right_empty {
			return '<<>>'
		} else if is_left_empty {
			if right_code.starts_with('<<') && right_code.ends_with('/utf8>>') {
				return right_code
			}
			return '<<${right_code}/binary>>'
		} else if is_right_empty {
			if left_code.starts_with('<<') && left_code.ends_with('/utf8>>') {
				return left_code
			}
			return '<<${left_code}/binary>>'
		}

		final_left := if left_code.starts_with('<<') && left_code.ends_with('/utf8>>') {
			left_code // mantém o literal completo, ex: <<"hello "/utf8>>
		} else {
			'${left_code}/binary'
		}
		final_right := if right_code.starts_with('<<') && right_code.ends_with('/utf8>>') {
			right_code
		} else {
			'${right_code}/binary'
		}
		// Remove os "<<" e ">>" dos literais para montar corretamente
		left_inner := if final_left.starts_with('<<') && final_left.ends_with('>>') {
			final_left[2..final_left.len - 2]
		} else {
			final_left
		}
		right_inner := if final_right.starts_with('<<') && final_right.ends_with('>>') {
			final_right[2..final_right.len - 2]
		} else {
			final_right
		}
		return '<<${left_inner}, ${right_inner}>>'
	}
	left := gen.generate_expression(expr.left)
	right := gen.generate_expression(expr.right)
	op := gen.translate_operator(expr.op, false)
	return '${left} ${op} ${right}'
}

// generate_binary_expression_in_guard generates code for binary expressions in guard context
fn (mut gen ErlangGenerator) generate_binary_expression_in_guard(expr ast.BinaryExpr) string {
	left := gen.generate_expression_in_guard(expr.left)
	right := gen.generate_expression_in_guard(expr.right)
	op := gen.translate_operator(expr.op, true)
	return '${left} ${op} ${right}'
}

// generate_unary_expression generates code for unary expressions
fn (mut gen ErlangGenerator) generate_unary_expression(expr ast.UnaryExpr) string {
	operand := gen.generate_expression(expr.operand)
	op := gen.translate_unary_operator(expr.op, false)
	return '${op}${operand}'
}

// generate_unary_expression_in_guard generates code for unary expressions in guards
fn (mut gen ErlangGenerator) generate_unary_expression_in_guard(expr ast.UnaryExpr) string {
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
		.bitwise_not { return 'bnot' }
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
		.bitwise_and {
			return 'band'
		}
		.bitwise_or {
			return 'bor'
		}
		.bitwise_xor {
			return 'bxor'
		}
		.bitwise_not {
			return 'bnot'
		}
		.lshift {
			return 'bsl'
		}
		.rshift {
			return 'bsr'
		}
	}
}

// generate_function_call generates code for function calls
fn (mut gen ErlangGenerator) generate_function_call(call ast.CallExpr) string {
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
fn (mut gen ErlangGenerator) generate_assignment(assign ast.AssignExpr) string {
	// On assignment, generate and bind a unique hashed name (not a parameter)
	hashed := gen.bind_variable(assign.name, false)
	value := gen.generate_expression(assign.value)
	return '${hashed} = ${value}'
}

// generate_match generates code for pattern matching
fn (mut gen ErlangGenerator) generate_match(match_expr ast.MatchExpr) string {
	value := gen.generate_expression(match_expr.value)
	mut cases := []string{}

	for case_item in match_expr.cases {
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

		mut success_body := ''
		if match_expr.expr is ast.MatchExpr {
			next_match := match_expr.expr as ast.MatchExpr
			success_body = gen.generate_match(next_match)
		} else {
			success_body = gen.generate_expression(match_expr.expr)
		}

		cases << '    ${pattern}${guard} ->\n        ${success_body}'
	}

	// Handle rescue clause if present
	mut rescue_case := ''
	if match_expr.rescue != none {
		rescue_expr := match_expr.rescue
		if rescue_expr is ast.MatchRescueExpr {
			match_rescue := rescue_expr as ast.MatchRescueExpr
			rescue_var := gen.capitalize_variable(match_rescue.rescue_var)
			rescue_body := gen.generate_block_expression(match_rescue.rescue_body)
			rescue_case = ';\n    ${rescue_var} ->\n        ${rescue_body}'
		}
	} else {
		// Add default error clause only when no rescue is present
		rescue_case = ';\n    Error ->\n        Error'
	}

	result := 'case ${value} of\n${cases.join(';\n')}${rescue_case}\nend'
	return result
}

// generate_case generates code for case expressions
fn (mut gen ErlangGenerator) generate_case(case_expr ast.CaseExpr) string {
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

		body := gen.generate_block_expression(clause.body)
		cases << '    ${pattern}${guard} -> ${body}'
	}

	return 'case ${subject} of\n${cases.join(';\n')}\nend'
}

// generate_list_cons generates code for list cons
fn (mut gen ErlangGenerator) generate_list_cons(expr ast.ListConsExpr) string {
	head := gen.generate_expression(expr.head)
	tail := gen.generate_expression(expr.tail)
	return '[${head} | ${tail}]'
}

// generate_list_literal generates code for list literals
fn (mut gen ErlangGenerator) generate_list_literal(expr ast.ListLiteralExpr) string {
	elements := expr.elements.map(gen.generate_expression(it))
	return '[${elements.join(', ')}]'
}

// generate_tuple generates code for tuples
fn (mut gen ErlangGenerator) generate_tuple(expr ast.TupleExpr) string {
	elements := expr.elements.map(gen.generate_expression(it))
	return '{${elements.join(', ')}}'
}

// generate_map_literal generates code for map literals
fn (mut gen ErlangGenerator) generate_map_literal(expr ast.MapLiteralExpr) string {
	mut entries := []string{}
	for entry in expr.entries {
		key := gen.generate_expression(entry.key)
		value := gen.generate_expression(entry.value)
		entries << '${key} => ${value}'
	}
	return '#{${entries.join(', ')}}'
}

// generate_map_access generates code for map access
fn (mut gen ErlangGenerator) generate_map_access(expr ast.MapAccessExpr) string {
	map_expr := gen.generate_expression(expr.map_expr)
	key := gen.generate_expression(expr.key)
	return 'maps:get(${key}, ${map_expr})'
}

// generate_map_update generates code for map updates
fn (mut gen ErlangGenerator) generate_map_update(expr ast.MapUpdateExpr) string {
	entries := expr.entries.map('${gen.generate_expression(it.key)} => ${gen.generate_expression(it.value)}').join(', ')

	if expr.base_map != ast.Expr(ast.LiteralExpr{}) {
		base := gen.generate_expression(expr.base_map)
		return '${base}#{${entries}}'
	} else {
		return '#{${entries}}'
	}
}

// generate_record_literal generates code for record literals
fn (mut gen ErlangGenerator) generate_record_literal(expr ast.RecordLiteralExpr) string {
	mut fields := []string{}
	for field in expr.fields {
		value := gen.generate_expression(field.value)
		fields << '${field.name} = ${value}'
	}
	return '#${expr.name.to_lower()}{${fields.join(', ')}}'
}

// generate_record_access generates code for record access
fn (mut gen ErlangGenerator) generate_record_access(expr ast.RecordAccessExpr) string {
	record := gen.generate_expression(expr.record)

	// Try to get the record type from the type table
	mut record_name := 'record' // fallback

	if type_table := gen.type_table {
		// Try to get type information for the record expression
		record_ast_id := ast.get_expr_ast_id(expr.record)

		if record_ast_id != -1 {
			if record_type := type_table.get_type(record_ast_id) {
				// Check if it's a record type with value
				if record_type.generic == 'record' {
					if type_str := record_type.value {
						record_name = type_str.to_lower()
					}
				}
				// Handle qualified types like example.User (generic: named)
				else if record_type.generic == 'named' {
					if type_str := record_type.value {
						if type_str.contains('.') {
							parts := type_str.split('.')
							if parts.len == 2 {
								record_name = parts[1].to_lower()
							}
						} else {
							record_name = type_str.to_lower()
						}
					}
				}
				// Handle qualified types like example.User in generic field
				else if record_type.generic.contains('.') {
					parts := record_type.generic.split('.')
					if parts.len == 2 {
						record_name = parts[1].to_lower()
					}
				}
			}
		}
	}

	// In Erlang, record access uses: Record#record_name.field
	return '${record}#${record_name}.${expr.field}'
}

// generate_fun_expression generates code for function expressions
fn (mut gen ErlangGenerator) generate_fun_expression(expr ast.FunExpr) string {
	parameters := expr.parameters.map(gen.generate_pattern(it))
	body := gen.generate_block_expression(expr.body)
	return 'fun(${parameters.join(', ')}) ->\n${body}\nend'
}

// generate_send generates code for message sending
fn (mut gen ErlangGenerator) generate_send(expr ast.SendExpr) string {
	pid := gen.generate_expression(expr.pid)
	message := gen.generate_expression_with_precedence(expr.message, .send)
	return '${pid} ! ${message}'
}

// generate_receive generates code for receive expressions
fn (mut gen ErlangGenerator) generate_receive(expr ast.ReceiveExpr) string {
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

		body := gen.generate_block_expression(case_item.body)
		cases << '    ${pattern}${guard} -> ${body}'
	}

	// Handle timeout clause
	timeout_clause := if timeout_opt := expr.timeout {
		timeout_value := gen.generate_expression(timeout_opt.timeout)
		timeout_body := gen.generate_block_expression(timeout_opt.body)
		'\nafter ${timeout_value} ->\n    ${timeout_body}'
	} else {
		''
	}

	return 'receive\n${cases.join(';\n')}${timeout_clause}\nend'
}

// generate_guard generates code for guard expressions
fn (mut gen ErlangGenerator) generate_guard(expr ast.GuardExpr) string {
	return gen.generate_expression_in_guard(expr.condition)
}

// generate_if_expression generates code for if expressions
fn (mut gen ErlangGenerator) generate_if_expression(expr ast.IfExpr) string {
	condition := gen.generate_expression(expr.condition)

	// Generate then body
	then_body := gen.generate_block_expression(expr.then_body)

	// Generate else body
	else_body := gen.generate_block_expression(expr.else_body)

	// In Erlang, we use case expression to implement if-else
	return 'case ${condition} of\n    true ->\n        ${then_body};\n    false ->\n        ${else_body}\nend'
}

// generate_with_expression generates code for with expressions
fn (mut gen ErlangGenerator) generate_with_expression(expr ast.WithExpr) string {
	// If there are no bindings, just return the body
	if expr.bindings.len == 0 {
		return gen.generate_block_expression(expr.body)
	}

	// Generate nested case expressions for each binding
	return gen.generate_with_bindings(expr.bindings, expr.body, expr.else_body, 0)
}

// generate_with_bindings generates nested case expressions recursively
fn (mut gen ErlangGenerator) generate_with_bindings(bindings []ast.WithBinding, body ast.BlockExpr, else_body ast.BlockExpr, index int) string {
	if index >= bindings.len {
		// We've reached the end of bindings, generate the main body
		return gen.generate_block_expression(body)
	}

	current_binding := bindings[index]
	// Use generate_pattern_with_binding only if there's an explicit assign_variable
	pattern := if current_binding.pattern is ast.RecordPattern {
		record_pattern := current_binding.pattern as ast.RecordPattern
		if record_pattern.assign_variable != none {
			gen.generate_pattern_with_binding(current_binding.pattern)
		} else {
			gen.generate_pattern(current_binding.pattern)
		}
	} else {
		gen.generate_pattern(current_binding.pattern)
	}
	value := gen.generate_expression(current_binding.value)

	// Generate guard clause if present
	guard_clause := match current_binding.guard {
		ast.LiteralExpr {
			// Check if it's the default true guardz
			match current_binding.guard.value {
				ast.BooleanLiteral {
					if current_binding.guard.value.value {
						// Default true guard, no need to generate when clause
						''
					} else {
						' when ${gen.generate_expression_in_guard(current_binding.guard)}'
					}
				}
				else {
					' when ${gen.generate_expression_in_guard(current_binding.guard)}'
				}
			}
		}
		else {
			' when ${gen.generate_expression_in_guard(current_binding.guard)}'
		}
	}

	// Generate the next level of bindings or the final body
	next_code := gen.generate_with_bindings(bindings, body, else_body, index + 1)

	// Generate the failure branch - if else_body is empty, use "Other"
	failure_branch := if else_body.body.len == 0 {
		'Other'
	} else {
		gen.generate_block_expression(else_body)
	}

	// Format the case expression with proper indentation for nested cases
	if next_code.contains('case') {
		// This is a nested case, format it properly
		indented_next := next_code.split('\n').map('        ${it}').join('\n')
		return 'case ${value} of\n    ${pattern}${guard_clause} ->\n${indented_next};\n    Other ->\n        ${failure_branch}\nend'
	} else {
		// This is the final case, format normally
		return 'case ${value} of\n    ${pattern}${guard_clause} ->\n        ${next_code};\n    Other ->\n        ${failure_branch}\nend'
	}
}

// indent_code adds proper indentation to code
fn (mut gen ErlangGenerator) indent_code(code string, level int) string {
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

// generate_simple_match generates code for simple match expressions
fn (mut gen ErlangGenerator) generate_simple_match(expr ast.SimpleMatchExpr) string {
	gen.enter_scope()
	pattern := gen.generate_pattern_with_binding(expr.pattern)

	// Generate guard clause if present
	mut guard_clause := ''
	if expr.guard != ast.Expr(ast.GuardExpr{}) {
		if expr.guard is ast.LiteralExpr {
			lit_expr := expr.guard as ast.LiteralExpr
			if lit_expr.value is ast.BooleanLiteral {
				bool_lit := lit_expr.value as ast.BooleanLiteral
				// Only generate guard if it's not the default "true"
				if !bool_lit.value {
					guard_clause = ' when ' + gen.generate_expression_in_guard(expr.guard)
				}
			} else {
				guard_clause = ' when ' + gen.generate_expression_in_guard(expr.guard)
			}
		} else {
			guard_clause = ' when ' + gen.generate_expression_in_guard(expr.guard)
		}
	}
	gen.exit_scope()

	value := gen.generate_expression(expr.value)

	// Simple match returns the original value if pattern doesn't match
	// Note: This é uma versão simplificada. O correto seria tratar no nível de statements.
	return 'case ${value} of\n    ${pattern}${guard_clause} ->\n        ${value};\n    Other ->\n        Other\nend'
}

// generate_match_rescue generates code for match rescue expressions
fn (mut gen ErlangGenerator) generate_match_rescue(expr ast.MatchRescueExpr) string {
	pattern := gen.generate_pattern(expr.pattern)
	value := gen.generate_expression(expr.value)
	rescue_var := gen.capitalize_variable(expr.rescue_var)

	// Generate rescue body
	rescue_body := gen.generate_block_expression(expr.rescue_body)

	// Note: This is a simplified version. The proper implementation should be handled
	// at the statement level to include subsequent expressions in the success branch
	return 'case ${value} of\n    ${pattern} ->\n        ok;\n    ${rescue_var} ->\n        ${rescue_body}\nend'
}

// generate_block_expression generates code for block expressions (do...end)
fn (mut gen ErlangGenerator) generate_block_expression(expr ast.BlockExpr) string {
	if expr.body.len == 0 {
		return 'nil'
	}

	// If there's only one statement, just return it without wrapping
	if expr.body.len == 1 {
		stmt := expr.body[0]
		return gen.generate_statement(stmt)
	}

	// For multiple statements, generate them separated by commas
	mut statements := []string{}
	for i, stmt in expr.body {
		stmt_code := gen.generate_statement(stmt)
		if i == expr.body.len - 1 {
			// Last statement doesn't need comma
			statements << stmt_code
		} else {
			// Add comma after each statement except the last
			statements << stmt_code
		}
	}

	// Join with commas and newlines for proper Erlang syntax
	return statements.join(',\n    ')
}

// generate_block_assignment_inline generates inline code for block assignments
fn (mut gen ErlangGenerator) generate_block_assignment_inline(var_name string, block ast.BlockExpr) string {
	if block.body.len == 0 {
		hashed := gen.bind_variable(var_name, false)
		return '${hashed} = nil'
	}

	mut result := []string{}

	// Add comment to mark the beginning of the block
	result << '% Block of ${gen.capitalize_variable(var_name)}'

	// Enter new scope for block
	gen.enter_scope()

	// Generate all statements in the block except the last one
	for i in 0 .. block.body.len - 1 {
		stmt := block.body[i]
		result << gen.generate_statement(stmt) + ','
	}

	// Generate the last statement and assign it to the variable
	last_stmt := block.body[block.body.len - 1]
	last_expr := gen.generate_statement(last_stmt)
	// Bind the block variable in this scope (not a parameter)
	hashed := gen.bind_variable(var_name, false)
	result << '${hashed} = ${last_expr}'

	// Exit block scope
	gen.exit_scope()

	// Add comment to mark the end of the block
	result << '% End Block of ${gen.capitalize_variable(var_name)}'

	return result.join('\n')
}

// generate_record_update generates code for record update expressions
fn (mut gen ErlangGenerator) generate_record_update(expr ast.RecordUpdateExpr) string {
	// Generate the base record expression
	base_record := gen.generate_expression(expr.base_record)

	// Generate the updated fields
	mut field_updates := []string{}
	for field in expr.fields {
		field_value := gen.generate_expression(field.value)
		field_updates << '${field.name} => ${field_value}'
	}

	// In modern Erlang, records are implemented as maps, so we use map update syntax

	// In modern Erlang, records are implemented as maps, so we use map update syntax
	result := '${base_record}#{${field_updates.join(', ')}}'
	return result
}

// generate_for_expression generates code for for expressions (list comprehensions)
fn (mut gen ErlangGenerator) generate_for_expression(expr ast.ForExpr) string {
	pattern := gen.generate_pattern(expr.pattern)
	collection := gen.generate_expression(expr.collection)
	body := gen.generate_block_expression(expr.body)

	// Handle guard if present
	guard_str := if expr.guard is ast.LiteralExpr {
		lit_expr := expr.guard as ast.LiteralExpr
		if lit_expr.value is ast.BooleanLiteral {
			bool_lit := lit_expr.value as ast.BooleanLiteral
			if bool_lit.value {
				// Default true guard, no filter needed
				''
			} else {
				', ${gen.generate_expression(expr.guard)}'
			}
		} else {
			', ${gen.generate_expression(expr.guard)}'
		}
	} else {
		', ${gen.generate_expression(expr.guard)}'
	}

	// Generate list comprehension
	return '[${body} || ${pattern} <- ${collection}${guard_str}]'
}

// generate_binary_pattern_expression generates code for binary pattern expressions
fn (mut gen ErlangGenerator) generate_binary_pattern_expression(expr ast.BinaryPatternExpr) string {
	mut segments := []string{}
	for segment in expr.segments {
		mut seg_code := gen.generate_expression(segment.value)
		if segment.size != none {
			seg_code += '::' + gen.generate_expression(segment.size)
		}
		if segment.options.len > 0 {
			seg_code += '/' + segment.options.join('-')
		}
		segments << seg_code
	}
	return '<<' + segments.join(', ') + '>>'
}
