module erlang

import ast
import analysis1

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
	return 'case ${value} of\n    ${pattern}${guard_clause} ->\n${formatted_success_code};\n    Other ->\n        Other\nend'
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

// get_function_return_type gets the return type from the type context
fn (gen ErlangGenerator) get_function_return_type(clause ast.FunctionClause, func_name string, arity int) string {
	if clause.return_type != none {
		return gen.generate_type_expression(clause.return_type)
	}

	// Try to get the inferred type from the type context
	if type_ctx := gen.type_context {
		if expr_type := type_ctx.get_function_return_type(func_name, arity) {
			erlang_type := gen.typeinfo_to_erlang_type(expr_type)
			return erlang_type
		}
	}

	// Fallback to 'any()' if no explicit return type and no inferred type
	return 'any()'
}

// Get function parameter types with unions
fn (gen ErlangGenerator) get_function_param_types(func_name string, arity int) []string {
	if type_ctx := gen.type_context {
		if param_types := type_ctx.get_function_param_types(func_name, arity) {
			erlang_types := param_types.map(gen.typeinfo_to_erlang_type(it))
			return erlang_types
		}
	}

	// Fallback: return any() for each parameter
	mut fallback_types := []string{}
	for _ in 0 .. arity {
		fallback_types << 'any()'
	}
	return fallback_types
}

// get_expression_type gets the type from the type context
fn (gen ErlangGenerator) get_expression_type(expr ast.Expr) string {
	// Get type from the type context
	if type_ctx := gen.type_context {
		if expr_type := type_ctx.get_expression_type(expr) {
			// Convert TypeInfo to string representation
			return expr_type.str()
		}
	}

	// If no type information available, this is an error - type checker should have caught this
	panic('No type found for expression')
}

// get_literal_type gets the type of a literal
fn (gen ErlangGenerator) get_literal_type(literal ast.Literal) string {
	return match literal {
		ast.IntegerLiteral { 'integer()' }
		ast.FloatLiteral { 'float()' }
		ast.StringLiteral { 'string()' }
		ast.BooleanLiteral { 'boolean()' }
		ast.AtomLiteral { 'atom()' }
		ast.NilLiteral { 'nil' }
	}
}

// get_binary_expression_type gets the type of a binary expression
fn (gen ErlangGenerator) get_binary_expression_type(expr ast.BinaryExpr) string {
	left_type := gen.get_expression_type(expr.left)
	right_type := gen.get_expression_type(expr.right)

	match expr.op {
		.add, .subtract, .multiply, .divide, .modulo, .power {
			// Arithmetic operations
			if left_type == 'float()' || right_type == 'float()' {
				return 'float()'
			}
			return 'integer()'
		}
		.and, .or {
			return 'boolean()'
		}
		.equal, .not_equal, .less_than, .less_equal, .greater_than, .greater_equal {
			return 'boolean()'
		}
		else {
			panic('Unknown binary operator')
		}
	}
}

// generate_function generates code for a function, opening a new scope for each clause
pub fn (mut gen ErlangGenerator) generate_function(func ast.FunctionStmt) string {
	// Store the current function ID for type lookups
	original_function_id := gen.current_function_id
	gen.current_function_id = func.id

	// Use the current type context (no function-specific context for now)
	original_type_context := gen.type_context

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
		arity := first_clause.parameters.len

		// Get parameter types with unions
		param_types := gen.get_function_param_types(func.name, arity)

		// Use explicit return type if defined, otherwise use type do contexto
		mut return_type := ''
		if first_clause.return_type != none {
			return_type = gen.generate_type_expression(first_clause.return_type)
		} else {
			return_type = gen.get_function_return_type(first_clause, func.name, arity)
		}
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

	// Restore the original type context and function ID
	gen.type_context = original_type_context
	gen.current_function_id = original_function_id

	return function_definitions.join('')
}

// generate_record_definition generates code for record definitions
fn (gen ErlangGenerator) generate_record_definition(record_def ast.RecordDefStmt) string {
	mut fields := []string{}
	for field in record_def.fields {
		fields << '${field.name}'
	}
	return '-record(${record_def.name.to_lower()}, {${fields.join(', ')}}).'
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
				'integer' {
					'integer()'
				}
				'float' {
					'float()'
				}
				'string' {
					'binary()'  // Lx strings são binaries UTF-8
				}
				'boolean' {
					'boolean()'
				}
				'atom' {
					'atom()'
				}
				'nil' {
					'nil'
				}
				'any' {
					'any()'
				}
				else {
					if type_expr.name.len > 0 && type_expr.name[0].is_capital() {
						'#${type_expr.name.to_lower()}{}'
					} else {
						type_expr.name // átomo puro
					}
				}
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
		ast.RecordTypeExpr {
			// For record types, we generate the record name as the type
			// The actual record definition will be generated separately
			'#${type_expr.name}{}'
		}
	}
}

// get_if_expression_type gets the type from the type context
fn (gen ErlangGenerator) get_if_expression_type(expr ast.IfExpr) string {
	// Get type from the type context
	if type_ctx := gen.type_context {
		if expr_type := type_ctx.get_expression_type(expr) {
			return expr_type.str()
		}
	}

	// If no type information available, this is an error - type checker should have caught this
	panic('No type found for if expression')
}

// get_case_expression_type gets the type from the type context
fn (gen ErlangGenerator) get_case_expression_type(expr ast.CaseExpr) string {
	// Get type from the type context
	if type_ctx := gen.type_context {
		if expr_type := type_ctx.get_expression_type(expr) {
			return expr_type.str()
		}
	}

	// If no type information available, this is an error - type checker should have caught this
	panic('No type found for case expression')
}

// get_with_expression_type gets the type from the type context
fn (gen ErlangGenerator) get_with_expression_type(expr ast.WithExpr) string {
	// Get type from the type context
	if type_ctx := gen.type_context {
		if expr_type := type_ctx.get_expression_type(expr) {
			return expr_type.str()
		}
	}

	// If no type information available, this is an error - type checker should have caught this
	panic('No type found for with expression')
}

// get_block_type gets the type from the type context
fn (gen ErlangGenerator) get_block_type(block ast.BlockExpr) string {
	// Get type from the type context
	if type_ctx := gen.type_context {
		if block.body.len > 0 {
	last_stmt := block.body[block.body.len - 1]
	if last_stmt is ast.ExprStmt {
				if expr_type := type_ctx.get_expression_type(last_stmt.expr) {
					return expr_type.str()
				}
			}
		}
	}

	// If no type information available, this is an error - type checker should have caught this
	panic('No type found for block expression')
}

// get_map_literal_type gets the type from the type context
fn (gen ErlangGenerator) get_map_literal_type(expr ast.MapLiteralExpr) string {
	// Get type from the type context
	if type_ctx := gen.type_context {
		if expr_type := type_ctx.get_expression_type(expr) {
			return expr_type.str()
		}
	}

	// If no type information available, this is an error - type checker should have caught this
	panic('No type found for map literal expression')
}

// get_map_update_type gets the type from the type context
fn (gen ErlangGenerator) get_map_update_type(expr ast.MapUpdateExpr) string {
	// Get type from the type context
	if type_ctx := gen.type_context {
		if expr_type := type_ctx.get_expression_type(expr) {
			return expr_type.str()
		}
	}

	// If no type information available, this is an error - type checker should have caught this
	panic('No type found for map update expression')
}

// get_assignment_type gets the type from the type context
fn (gen ErlangGenerator) get_assignment_type(expr ast.AssignExpr) string {
	// Get type from the type context
	if type_ctx := gen.type_context {
		if expr_type := type_ctx.get_expression_type(expr) {
			return expr_type.str()
		}
	}

	// If no type information available, this is an error - type checker should have caught this
	panic('No type found for assignment expression')
}

// find_common_type_for_keys finds the most appropriate common type for map keys
fn (gen ErlangGenerator) find_common_type_for_keys(type1 string, type2 string) string {
	if type1 == type2 {
		return type1
	}

	// For keys, follow Erlang conventions:
	// atom() is preferred for static keys
	// string() for dynamic keys (JSON-like)
	// term() for truly mixed keys

	// If both are atom() or string(), prefer the more specific one
	if (type1 == 'atom()' && type2 == 'string()') || (type1 == 'string()' && type2 == 'atom()') {
		return 'atom() | string()'
	}

	// If one is atom(), prefer atom() for static keys
	if type1 == 'atom()' || type2 == 'atom()' {
		return 'atom()'
	}

	// If one is string(), prefer string() for dynamic keys
	if type1 == 'string()' || type2 == 'string()' {
		return 'string()'
	}

	// For other mixed types, use term()
	return 'term()'
}

// find_common_type_for_values finds the most appropriate common type for map values
fn (gen ErlangGenerator) find_common_type_for_values(type1 string, type2 string) string {
	if type1 == type2 {
		return type1
	}

	// For values, be more flexible and use union types or term()
	// Based on the test expectations, string() seems to be preferred

	// If one of the types is string(), prefer string()
	if type1 == 'string()' || type2 == 'string()' {
		return 'string()'
	}

	// If one of the types is atom(), prefer atom()
	if type1 == 'atom()' || type2 == 'atom()' {
		return 'atom()'
	}

	// Numeric types precedence: integer() > float()
	if (type1 == 'integer()' && type2 == 'float()') || (type1 == 'float()' && type2 == 'integer()') {
		return 'number()'
	}

	// Type precedence for finding common type: any() > integer() > float() > boolean()
	precedence_map := {
		'any()':     5
		'integer()': 4
		'float()':   3
		'boolean()': 2
	}

	precedence1 := precedence_map[type1] or { 0 }
	precedence2 := precedence_map[type2] or { 0 }

	// Return the type with higher precedence (more general)
	if precedence1 >= precedence2 {
		return type1
	} else {
		return type2
	}
}

// get_for_expression_type gets the type from the type context
fn (gen ErlangGenerator) get_for_expression_type(expr ast.ForExpr) string {
	// Get type from the type context
	if type_ctx := gen.type_context {
		if expr_type := type_ctx.get_expression_type(expr) {
			return expr_type.str()
		}
	}

	// If no type information available, this is an error - type checker should have caught this
	panic('No type found for for expression')
}

// create_typed_pattern creates a pattern with type information based on the element type
fn (gen ErlangGenerator) create_typed_pattern(pattern ast.Pattern, element_type string) ast.Pattern {
	match pattern {
		ast.VarPattern {
			// Create a VarPattern with the inferred type
			type_annotation := ast.SimpleTypeExpr{
				name:     element_type.replace('()', '')
				position: pattern.position
			}
			return ast.VarPattern{
				name:            pattern.name
				position:        pattern.position
				type_annotation: type_annotation
			}
		}
		ast.TuplePattern {
			// For tuple patterns, extract individual element types
			mut element_types := []string{}
			if element_type.starts_with('{') && element_type.ends_with('}') {
				// Parse tuple type like "{integer(), integer()}"
				inner_types := element_type[1..element_type.len - 1]
				// Better parsing - handle nested types and spaces
				element_types = gen.parse_tuple_element_types(inner_types)
			} else {
				// If we can't parse the tuple type, use any() for all elements
				for _ in pattern.elements {
					element_types << 'any()'
				}
			}

			// Create typed patterns for each tuple element
			mut typed_elements := []ast.Pattern{}
			for i, elem_pattern in pattern.elements {
				elem_type := if i < element_types.len { element_types[i] } else { 'any()' }
				typed_elements << gen.create_typed_pattern(elem_pattern, elem_type)
			}

			return ast.TuplePattern{
				elements: typed_elements
			}
		}
		else {
			// For other patterns, return as-is
			return pattern
		}
	}
}

// parse_tuple_element_types parses tuple element types from a string like "integer(), integer()"
fn (gen ErlangGenerator) parse_tuple_element_types(types_str string) []string {
	mut result := []string{}
	mut current := ''
	mut paren_count := 0

	for ch in types_str {
		match ch {
			`(` {
				paren_count++
				current += ch.ascii_str()
			}
			`)` {
				paren_count--
				current += ch.ascii_str()
			}
			`,` {
				if paren_count == 0 {
					result << current.trim_space()
					current = ''
				} else {
					current += ch.ascii_str()
				}
			}
			else {
				current += ch.ascii_str()
			}
		}
	}

	if current.trim_space() != '' {
		result << current.trim_space()
	}

	return result
}

// convert_type_expr_to_spec_string converts a TypeExpr to a spec string
fn (gen ErlangGenerator) convert_type_expr_to_spec_string(type_expr analysis1.TypeExpr) string {
	match type_expr {
		analysis1.TypeConstructor {
			if type_expr.parameters.len > 0 {
				params := type_expr.parameters.map(gen.convert_type_expr_to_spec_string(it)).join(', ')
				'${type_expr.name}(${params})'
			} else {
				type_expr.name
			}
		}
		analysis1.FunctionType {
			if type_expr.parameters.len == 0 {
				'fun(() -> ${gen.convert_type_expr_to_spec_string(type_expr.return_type)})'
			} else {
				params := type_expr.parameters.map(gen.convert_type_expr_to_spec_string(it)).join(', ')
				'fun((${params}) -> ${gen.convert_type_expr_to_spec_string(type_expr.return_type)})'
			}
		}
		analysis1.MapType {
			'#{${gen.convert_type_expr_to_spec_string(type_expr.key_type)} => ${gen.convert_type_expr_to_spec_string(type_expr.value_type)}}'
		}
		analysis1.TupleType {
			if type_expr.elements.len == 0 {
				'{}'
			} else {
				elements := type_expr.elements.map(gen.convert_type_expr_to_spec_string(it)).join(', ')
				'{${elements}}'
			}
		}
		analysis1.ListType {
			'[${gen.convert_type_expr_to_spec_string(type_expr.element_type)}]'
		}
		analysis1.UnionType {
			types := type_expr.types.map(gen.convert_type_expr_to_spec_string(it)).join(' | ')
			types
		}
		else {
			'any()'
		}
	}
	return 'any()'
}

// REMOVIDO: TypeInfo, parse_type_expression, parse_tuple_type, extract_tuple_elements, types_are_compatible, tuple_types_are_compatible e todos os usos relacionados.
// O backend não deve conter nenhuma lógica de análise ou comparação de tipos.

// typeinfo_to_erlang_type converts TypeInfo to Erlang type string
fn (gen ErlangGenerator) typeinfo_to_erlang_type(type_info analysis1.TypeInfo) string {
	match type_info.generic {
		'integer' {
			return 'integer()'
		}
		'float' {
			return 'float()'
		}
		'string' {
			return 'binary()'  // Lx strings são binaries UTF-8
		}
		'boolean' {
			return 'boolean()'
		}
		'atom' {
			if value := type_info.value {
				return value // Return the atom value directly (e.g., "ok", "error")
			}
			return 'atom()'
		}
		'nil' {
			return 'nil'
		}
		'any' {
			return 'any()'
		}
		'union' {
			if value := type_info.value {
				return value
			}
			return 'any()'
		}
		'tuple' {
			if value := type_info.value {
				return gen.convert_tuple_type_to_erlang(value)
			}
			return 'tuple()'
		}
		'record' {
			if value := type_info.value {
				return '#${value.to_lower()}{}'
			}
			return 'record()'
		}
		'list' {
			if value := type_info.value {
				return gen.convert_list_type_to_erlang(value)
			}
			return '[any()]'
		}
		'map' {
			if value := type_info.value {
				return gen.convert_map_type_to_erlang(value)
			}
			return '#{any() => any()}'
		}
		else {
			return 'any()'
		}
	}
}

// convert_union_type_to_erlang converts union type string to Erlang format
fn (gen ErlangGenerator) convert_union_type_to_erlang(union_type string) string {
	// Split union type by " | "
	parts := union_type.split(' | ')
	erlang_parts := parts.map(gen.convert_type_element_to_erlang(it))
	return erlang_parts.join(' | ')
}

// convert_tuple_type_to_erlang converts tuple type string to Erlang format
fn (gen ErlangGenerator) convert_tuple_type_to_erlang(tuple_type string) string {
	// Extract elements from tuple type string
	// Example: "{atom(ok), record(Usuario)}" -> ["atom(ok)", "record(Usuario)"]
	elements := analysis1.extract_tuple_elements(tuple_type)

	// Convert each element to Erlang format
	erlang_elements := elements.map(gen.convert_type_element_to_erlang(it))

	return '{${erlang_elements.join(', ')}}'
}

// convert_list_type_to_erlang converts list type string to Erlang format
fn (gen ErlangGenerator) convert_list_type_to_erlang(list_type string) string {
	// Extract element type from list type string
	// Example: "list(integer)" -> "integer"
	element_type := analysis1.extract_list_element_type(list_type)

	// Convert element type to Erlang format
	erlang_element := gen.convert_type_element_to_erlang(element_type)

	return '[${erlang_element}]'
}

// convert_map_type_to_erlang converts map type string to Erlang format
fn (gen ErlangGenerator) convert_map_type_to_erlang(map_type string) string {
	// Extract key and value types from map type string
	// Example: "map(integer=>string)" -> ["integer", "string"]
	key_value_types := analysis1.extract_map_key_value_types(map_type)

	if key_value_types.len == 2 {
		key_type := gen.convert_type_element_to_erlang(key_value_types[0])
		value_type := gen.convert_type_element_to_erlang(key_value_types[1])
		return '#{${key_type} => ${value_type}}'
	}

	return '#{any() => any()}'
}

// convert_type_element_to_erlang converts a type element to Erlang format
fn (gen ErlangGenerator) convert_type_element_to_erlang(type_element string) string {
	// Handle specific type patterns
	if type_element.starts_with('atom(') && type_element.ends_with(')') {
		// Extract atom value: "atom(ok)" -> "ok"
		atom_value := type_element[5..type_element.len-1]
		return atom_value
	}

	if type_element.starts_with('record(') && type_element.ends_with(')') {
		// Extract record name: "record(Usuario)" -> "#usuario{}"
		record_name := type_element[7..type_element.len-1]
		return '#${record_name.to_lower()}{}'
	}

	// Handle basic types
	match type_element {
		'integer' { return 'integer()' }
		'float' { return 'float()' }
		'string' { return 'binary()' }  // Lx strings são binaries UTF-8
		'boolean' { return 'boolean()' }
		'atom' { return 'atom()' }
		'nil' { return 'nil' }
		'any' { return 'any()' }
		else {
			// If it's a user-defined type (starts with capital letter), treat as record
			if type_element.len > 0 && type_element[0].is_capital() {
				return '#${type_element.to_lower()}{}'
			}
			return 'any()'
		}
	}
}
