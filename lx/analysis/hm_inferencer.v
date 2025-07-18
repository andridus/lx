module analysis

import ast
import errors

// HMInferencer implements basic HM type inference
pub struct HMInferencer {
pub mut:
	type_table           TypeTable
	type_env             TypeEnv
	next_type_var        int = 1
	constraints          []Constraint
	last_map_field_types map[string]TypeInfo
	record_field_types   map[string]TypeInfo
	error_reporter       ErrorReporter
}

// Constraint represents a type constraint in the HM system
pub struct Constraint {
pub:
	left  TypeInfo
	right TypeInfo
}

// new_hm_inferencer creates a new HM inferencer
pub fn new_hm_inferencer() HMInferencer {
	return HMInferencer{
		type_table:         new_type_table()
		type_env:           new_type_env()
		next_type_var:      1
		constraints:        []
		record_field_types: map[string]TypeInfo{}
		error_reporter:     new_error_reporter()
	}
}

// new_hm_inferencer_with_debug creates a new HM inferencer with debug mode
pub fn new_hm_inferencer_with_debug() HMInferencer {
	return HMInferencer{
		type_table:         new_type_table_with_debug()
		type_env:           new_type_env()
		next_type_var:      1
		constraints:        []
		record_field_types: map[string]TypeInfo{}
		error_reporter:     new_error_reporter()
	}
}

// enable_debug enables debug mode for the type table
pub fn (mut hmi HMInferencer) enable_debug() {
	hmi.type_table.enable_debug()
}

// disable_debug disables debug mode for the type table
pub fn (mut hmi HMInferencer) disable_debug() {
	hmi.type_table.disable_debug()
}

// print_debug prints debug information
pub fn (hmi HMInferencer) print_debug() {
	println('=== HM INFERENCER DEBUG ===')
	println('Next type variable: ${hmi.next_type_var}')
	println('Constraints: ${hmi.constraints.len}')

	if hmi.constraints.len > 0 {
		println('Constraints:')
		for i, constraint in hmi.constraints {
			println('  ${i + 1}: ${constraint.left} = ${constraint.right}')
		}
	}

	println('Type Environment:')
	hmi.type_env.print_debug()

	println('Type Table:')
	hmi.type_table.debug_print()
	println('==========================')
}

// get_type_table returns the type table
pub fn (hmi HMInferencer) get_type_table() &TypeTable {
	return &hmi.type_table
}

// fresh_type_var creates a fresh type variable
pub fn (mut hmi HMInferencer) fresh_type_var() TypeInfo {
	id := hmi.next_type_var
	hmi.next_type_var++
	return TypeInfo{
		generic: 'typevar'
		value:   'T${id}'
		values:  []
	}
}

// add_constraint adds a type constraint
pub fn (mut hmi HMInferencer) add_constraint(left TypeInfo, right TypeInfo) {
	hmi.constraints << Constraint{
		left:  left
		right: right
	}
}

// reset_for_function resets the HM inferencer state for a new function
pub fn (mut hmi HMInferencer) reset_for_function() {
	if hmi.type_table.debug_mode {
		println('[HM_INFERENCER] Resetting state for new function')
	}

	// Clear constraints from previous function
	hmi.constraints.clear()

	// Reset type variable counter to avoid conflicts
	hmi.next_type_var = 0
}

// infer_expression infers the type of an expression using HM
pub fn (mut hmi HMInferencer) infer_expression(expr ast.Expr) !TypeInfo {
	ast_id := ast.get_expr_ast_id(expr)

	expr_type := match expr {
		ast.VariableExpr { 'VariableExpr(${expr.name})' }
		ast.LiteralExpr { 'LiteralExpr' }
		ast.BinaryExpr { 'BinaryExpr' }
		ast.AssignExpr { 'AssignExpr(${expr.name})' }
		ast.BlockExpr { 'BlockExpr' }
		ast.CaseExpr { 'CaseExpr' }
		ast.RecordLiteralExpr { 'RecordLiteralExpr' }
		else { 'Other' }
	}
	if hmi.type_table.debug_mode {
		println('[HM_INFERENCER] >>> Inferring ${expr_type} with AST ID: ${ast_id}')
	}

	// Check if already inferred
	if ast_id > 0 {
		if hmi.type_table.debug_mode {
			println('[HM_INFERENCER] >>> Checking if AST ID ${ast_id} has cached type')
		}

		if existing_type := hmi.type_table.get_type(ast_id) {
			if hmi.type_table.debug_mode {
				println('[HM_INFERENCER] >>> Found cached type: ${existing_type} (generic: "${existing_type.generic}")')
			}
			return existing_type
		} else {
			if hmi.type_table.debug_mode {
				println('[HM_INFERENCER] >>> No cached type found, proceeding with inference')
			}
		}
	} else {
		if hmi.type_table.debug_mode {
			println('[HM_INFERENCER] >>> AST ID ${ast_id} is invalid, proceeding with inference')
		}
	}

	// Infer type based on expression kind
	if hmi.type_table.debug_mode {
		println('[HM_INFERENCER] >>> About to match expression type')
	}

	inferred_type := match expr {
		ast.VariableExpr {
			if hmi.type_table.debug_mode {
				println('[HM_INFERENCER] Matched VariableExpr')
			}
			hmi.infer_variable(expr)!
		}
		ast.LiteralExpr {
			if hmi.type_table.debug_mode {
				println('[HM_INFERENCER] Matched LiteralExpr')
			}
			hmi.infer_literal(expr)!
		}
		ast.BinaryExpr {
			if hmi.type_table.debug_mode {
				println('[HM_INFERENCER] Matched BinaryExpr')
			}
			hmi.infer_binary(expr)!
		}
		ast.AssignExpr {
			if hmi.type_table.debug_mode {
				println('[HM_INFERENCER] Matched AssignExpr')
			}
			hmi.infer_assign(expr)!
		}
		ast.BlockExpr {
			if hmi.type_table.debug_mode {
				println('[HM_INFERENCER] Matched BlockExpr')
			}
			hmi.infer_block(expr)!
		}
		ast.CallExpr {
			if hmi.type_table.debug_mode {
				println('[HM_INFERENCER] Matched CallExpr')
			}
			hmi.infer_call(expr)!
		}
		ast.TupleExpr {
			if hmi.type_table.debug_mode {
				println('[HM_INFERENCER] Matched TupleExpr')
			}
			hmi.infer_tuple(expr)!
		}
		ast.RecordLiteralExpr {
			if hmi.type_table.debug_mode {
				println('[HM_INFERENCER] Matched RecordLiteralExpr')
			}
			hmi.infer_record_literal(expr)!
		}
		ast.RecordAccessExpr {
			if hmi.type_table.debug_mode {
				println('[HM_INFERENCER] Matched RecordAccessExpr')
			}
			hmi.infer_record_access(expr)!
		}
		ast.CaseExpr {
			if hmi.type_table.debug_mode {
				println('[HM_INFERENCER] Matched CaseExpr')
			}
			hmi.infer_case(expr)!
		}
		ast.WithExpr {
			if hmi.type_table.debug_mode {
				println('[HM_INFERENCER] Matched WithExpr')
			}
			hmi.infer_with(expr)!
		}
		ast.SimpleMatchExpr {
			if hmi.type_table.debug_mode {
				println('[HM_INFERENCER] Matched SimpleMatchExpr')
			}
			hmi.infer_simple_match(expr)!
		}
		ast.MatchExpr {
			if hmi.type_table.debug_mode {
				println('[HM_INFERENCER] Matched MatchExpr')
			}
			hmi.infer_match(expr)!
		}
		ast.IfExpr {
			if hmi.type_table.debug_mode {
				println('[HM_INFERENCER] Matched IfExpr')
			}
			hmi.infer_if(expr)!
		}
		ast.MatchRescueExpr {
			if hmi.type_table.debug_mode {
				println('[HM_INFERENCER] Matched MatchRescueExpr')
			}
			hmi.infer_match_rescue(expr)!
		}
		ast.ForExpr {
			if hmi.type_table.debug_mode {
				println('[HM_INFERENCER] Matched ForExpr')
			}
			hmi.infer_for(expr)!
		}
		ast.ListLiteralExpr {
			if hmi.type_table.debug_mode {
				println('[HM_INFERENCER] Matched ListLiteralExpr')
			}
			hmi.infer_list_literal(expr)!
		}
		ast.MapLiteralExpr {
			if hmi.type_table.debug_mode {
				println('[HM_INFERENCER] Matched MapLiteralExpr')
			}
			hmi.infer_map_literal(expr)!
		}
		ast.UnaryExpr {
			if hmi.type_table.debug_mode {
				println('[HM_INFERENCER] Matched UnaryExpr')
			}
			hmi.infer_unary(expr)!
		}
		ast.BinaryPatternExpr {
			if hmi.type_table.debug_mode {
				println('[HM_INFERENCER] Matched BinaryPatternExpr')
			}
			// Para padrões binários, fazer bind das variáveis dos segmentos
			for segment in expr.segments {
				if segment.value is ast.VariableExpr {
					var_expr := segment.value as ast.VariableExpr
					if hmi.type_table.debug_mode {
						println('[HM_INFERENCER] Bind variável binária: ' + var_expr.name)
					}
					hmi.type_env.bind(var_expr.name, monotype(typeinfo_integer()))
				}
			}
			return typeinfo_binary()
		}
		else {
			if hmi.type_table.debug_mode {
				println('[HM_INFERENCER] Unsupported expression type: ${expr}')
			}
			return error('Unsupported expression type for HM inference: ${expr}')
		}
	}

	// Store inferred type in type table
	if ast_id > 0 {
		pos := hmi.get_position_string(expr)
		expr_text := hmi.get_expression_text(expr)
		hmi.type_table.assign_type_with_pos(ast_id, inferred_type, pos, expr_text)

		if hmi.type_table.debug_mode {
			println('[HM_INFERENCER] Inferred type: ${inferred_type} for AST ID: ${ast_id}')
		}
	} else {
		if hmi.type_table.debug_mode {
			println('[HM_INFERENCER] WARNING: Expression has invalid AST ID: ${ast_id}')
		}
	}

	return inferred_type
}

// infer_variable infers the type of a variable
pub fn (mut hmi HMInferencer) infer_variable(expr ast.VariableExpr) !TypeInfo {
	if hmi.type_table.debug_mode {
		println('[HM_INFERENCER] infer_variable: ${expr.name}')
	}

	// Look up variable in type environment
	if scheme := hmi.type_env.lookup(expr.name) {
		if hmi.type_table.debug_mode {
			println('[HM_INFERENCER] Found variable ${expr.name} with type: ${scheme.type_info}')
		}
		return scheme.type_info
	}

	if hmi.type_table.debug_mode {
		println('[HM_INFERENCER] Variable ${expr.name} not found in environment')
	}

	// Report error using error_reporter instead of throwing exception
	hmi.error_reporter.report_error(.type_error, 'Variable "${expr.name}" is not defined',
		expr.position, 'Make sure the variable is declared before use')

	// Return any() type to allow analysis to continue
	return typeinfo_any()
}

// infer_literal infers the type of a literal
pub fn (mut hmi HMInferencer) infer_literal(expr ast.LiteralExpr) !TypeInfo {
	inferred_type := match expr.value {
		ast.IntegerLiteral {
			if hmi.type_table.debug_mode {
				println('[HM_INFERENCER] infer_literal: integer ${expr.value.value}')
			}
			typeinfo_integer()
		}
		ast.FloatLiteral {
			if hmi.type_table.debug_mode {
				println('[HM_INFERENCER] infer_literal: float ${expr.value.value}')
			}
			typeinfo_float()
		}
		ast.StringLiteral {
			if hmi.type_table.debug_mode {
				println('[HM_INFERENCER] infer_literal: string "${expr.value.value}"')
			}
			typeinfo_string()
		}
		ast.BooleanLiteral {
			if hmi.type_table.debug_mode {
				println('[HM_INFERENCER] infer_literal: boolean ${expr.value.value}')
			}
			typeinfo_boolean()
		}
		ast.AtomLiteral {
			if hmi.type_table.debug_mode {
				println('[HM_INFERENCER] infer_literal: atom ${expr.value.value}')
			}
			typeinfo_atom()
		}
		ast.NilLiteral {
			if hmi.type_table.debug_mode {
				println('[HM_INFERENCER] infer_literal: nil')
			}
			typeinfo_nil()
		}
	}

	if hmi.type_table.debug_mode {
		println('[HM_INFERENCER] Literal inferred as: ${inferred_type}')
	}

	return inferred_type
}

// infer_binary infers the type of a binary expression
pub fn (mut hmi HMInferencer) infer_binary(expr ast.BinaryExpr) !TypeInfo {
	if hmi.type_table.debug_mode {
		println('[HM_INFERENCER] infer_binary: ${expr.op}')
	}

	left_type := hmi.infer_expression(expr.left)!
	right_type := hmi.infer_expression(expr.right)!

	if hmi.type_table.debug_mode {
		println('[HM_INFERENCER] Binary left: ${left_type}, right: ${right_type}')
	}

	// Add constraint that left and right types should be compatible
	hmi.add_constraint(left_type, right_type)

	// Determine result type based on operator
	result_type := match expr.op {
		.add, .subtract, .multiply, .divide, .modulo {
			// Arithmetic operators
			if left_type.generic == 'integer' && right_type.generic == 'integer' {
				typeinfo_integer()
			} else if left_type.generic == 'float' || right_type.generic == 'float' {
				typeinfo_float()
			} else {
				typeinfo_integer() // Default to integer
			}
		}
		.equal, .not_equal, .less_than, .greater_than, .less_equal, .greater_equal {
			// Comparison operators
			typeinfo_boolean()
		}
		.and, .or {
			// Logical operators
			typeinfo_boolean()
		}
		.append {
			// String concatenation or list concatenation
			if left_type.generic == 'string' {
				typeinfo_string()
			} else {
				left_type // For lists, return the list type
			}
		}
		else {
			return error('Unsupported binary operator: ${expr.op}')
		}
	}

	if hmi.type_table.debug_mode {
		println('[HM_INFERENCER] Binary result type: ${result_type}')
	}

	return result_type
}

// infer_assign infers the type of an assignment
pub fn (mut hmi HMInferencer) infer_assign(expr ast.AssignExpr) !TypeInfo {
	if hmi.type_table.debug_mode {
		println('[HM_INFERENCER] infer_assign: ${expr.name}')
	}

	// Infer type of the value being assigned
	value_type := hmi.infer_expression(expr.value)!

	// Bind the variable to the inferred type
	hmi.type_env.bind(expr.name, monotype(value_type))

	if hmi.type_table.debug_mode {
		println('[HM_INFERENCER] Assigned ${expr.name} to type: ${value_type}')
	}

	return value_type
}

// resolve_constraints resolves all collected constraints and applies substitutions
pub fn (mut hmi HMInferencer) resolve_constraints() !Substitution {
	if hmi.type_table.debug_mode {
		println('[HM_INFERENCER] Resolving ${hmi.constraints.len} constraints')
	}

	if hmi.constraints.len == 0 {
		return Substitution{
			subst: map[int]TypeInfo{}
		}
	}

	// Group constraints by type variable
	mut var_constraints := map[int][]TypeInfo{}

	for constraint in hmi.constraints {
		if hmi.type_table.debug_mode {
			println('[HM_INFERENCER] Processing constraint: ${constraint.left} = ${constraint.right}')
		}

		// If left is a type variable, collect all constraints for it
		if constraint.left.generic == 'typevar' {
			if value := constraint.left.value {
				var_id := value.int()
				if var_id !in var_constraints {
					var_constraints[var_id] = []
				}
				var_constraints[var_id] << constraint.right
			}
		}
	}

	// Create substitution based on collected constraints
	mut substitution := Substitution{
		subst: map[int]TypeInfo{}
	}

	for var_id, types in var_constraints {
		if hmi.type_table.debug_mode {
			println('[HM_INFERENCER] Variable ${var_id} has ${types.len} constraints: ${types}')
		}

		if types.len == 1 {
			// Single constraint - direct substitution
			substitution.subst[var_id] = types[0]
			if hmi.type_table.debug_mode {
				println('[HM_INFERENCER] Single constraint: typevar(T${var_id}) -> ${types[0]}')
			}
		} else {
			// Multiple constraints - create union type
			union_type := typeinfo_union(types)
			substitution.subst[var_id] = union_type
			if hmi.type_table.debug_mode {
				println('[HM_INFERENCER] Multiple constraints: typevar(T${var_id}) -> ${union_type}')
			}
		}
	}

	if hmi.type_table.debug_mode {
		println('[HM_INFERENCER] Generated substitution: ${substitution.str()}')
	}

	// Apply substitution to all types in the type table
	hmi.apply_substitution_to_type_table(substitution)

	// Apply substitution to all bindings in the type environment
	hmi.apply_substitution_to_type_env(substitution)

	return substitution
}

// apply_substitution_to_type_table applies a substitution to all types in the type table
fn (mut hmi HMInferencer) apply_substitution_to_type_table(substitution Substitution) {
	if hmi.type_table.debug_mode {
		println('[HM_INFERENCER] Applying substitution to type table')
	}

	// Get all types from the type table
	all_types := hmi.type_table.get_all_types()

	for ast_id, type_info in all_types {
		substituted_type := substitution.apply_to_type(type_info)

		if hmi.type_table.debug_mode {
			println('[HM_INFERENCER] AST ID ${ast_id}: ${type_info} -> ${substituted_type}')
		}

		// Update the type in the table
		hmi.type_table.assign_type(ast_id, substituted_type)
	}

	// Log final type table after substitution
	if hmi.type_table.debug_mode {
		println('[HM_INFERENCER] FINAL TYPE TABLE:')
		for ast_id, type_info in hmi.type_table.get_all_types() {
			println('  AST ID ${ast_id}: ${type_info}')
		}
	}
}

// apply_substitution_to_type_env applies a substitution to all bindings in the type environment
fn (mut hmi HMInferencer) apply_substitution_to_type_env(substitution Substitution) {
	if hmi.type_table.debug_mode {
		println('[HM_INFERENCER] Applying substitution to type environment')
	}

	// Apply substitution to all bindings in the current environment
	for name, scheme in hmi.type_env.bindings {
		substituted_type := substitution.apply_to_type(scheme.type_info)

		if hmi.type_table.debug_mode {
			println('[HM_INFERENCER] Environment binding "${name}": ${scheme.type_info} -> ${substituted_type}')
		}

		// Update the binding with the substituted type
		hmi.type_env.bindings[name] = TypeScheme{
			vars:      scheme.vars
			type_info: substituted_type
		}
	}

	// Log final environment after substitution
	if hmi.type_table.debug_mode {
		println('[HM_INFERENCER] FINAL TYPE ENVIRONMENT:')
		for name, scheme in hmi.type_env.bindings {
			println('  ${name}: ${scheme.type_info}')
		}
	}
}

// infer_block infers the type of a block expression
pub fn (mut hmi HMInferencer) infer_block(expr ast.BlockExpr) !TypeInfo {
	if hmi.type_table.debug_mode {
		println('[HM_INFERENCER] >>> Entrando em infer_block')
	}

	mut last_type := typeinfo_nil()
	mut return_types := []TypeInfo{}
	mut last_stmt_expr := ast.Expr{}

	for i, stmt in expr.body {
		if hmi.type_table.debug_mode {
			println('[HM_INFERENCER] Processing statement ${i + 1}/${expr.body.len}')
		}

		match stmt {
			ast.ExprStmt {
				if hmi.type_table.debug_mode {
					println('[HM_INFERENCER] Found ExprStmt, inferring expression')
				}
				stmt_type := hmi.infer_expression(stmt.expr)!
				if hmi.type_table.debug_mode {
					println('[HM_INFERENCER] ExprStmt inferred as: ${stmt_type}')
				}

				// Check if this expression can return a value (like MatchRescueExpr or SimpleMatchExpr)
				if stmt.expr is ast.MatchRescueExpr {
					if hmi.type_table.debug_mode {
						println('[HM_INFERENCER] Found MatchRescueExpr, adding to return types')
					}
					return_types << stmt_type
				}
				if stmt.expr is ast.SimpleMatchExpr {
					if hmi.type_table.debug_mode {
						println('[HM_INFERENCER] Found SimpleMatchExpr, adding to return types')
					}
					return_types << stmt_type
				}

				last_type = stmt_type
				last_stmt_expr = stmt.expr
			}
			else {
				if hmi.type_table.debug_mode {
					println('[HM_INFERENCER] Skipping non-expression statement')
				}
			}
		}
	}

	// Apply current substitution to resolve any remaining typevars
	mut final_last_type := last_type
	if hmi.constraints.len > 0 {
		if hmi.type_table.debug_mode {
			println('[HM_INFERENCER] Resolving constraints before final type calculation')
		}
		substitution := hmi.resolve_constraints() or {
			Substitution{
				subst: map[int]TypeInfo{}
			}
		}
		final_last_type = substitution.apply_to_type(last_type)
		if hmi.type_table.debug_mode {
			println('[HM_INFERENCER] Applied substitution to last_type: ${last_type} -> ${final_last_type}')
		}
	}

	// Check if the last statement is a variable expression (extracted from pattern)
	mut additional_types := []TypeInfo{}
	if last_stmt_expr is ast.VariableExpr {
		ve := last_stmt_expr as ast.VariableExpr
		var_name := ve.name
		if hmi.type_table.debug_mode {
			println('[HM_INFERENCER] Last statement is VariableExpr: ${var_name}')
		}
		// Look up the variable in the type environment
		if scheme := hmi.type_env.lookup(var_name) {
			if hmi.type_table.debug_mode {
				println('[HM_INFERENCER] Found variable ${var_name} in environment with type: ${scheme.type_info}')
			}
			additional_types << scheme.type_info
		} else {
			if hmi.type_table.debug_mode {
				println('[HM_INFERENCER] Variable ${var_name} not found in environment')
			}
		}
	}

	// Sempre inclua o tipo do último statement na união, mesmo que já esteja em return_types
	if return_types.len > 0 {
		mut all_types := return_types.clone()
		all_types << final_last_type // força inclusão, mesmo que já exista
		all_types << additional_types // adiciona tipos de variáveis extraídas
		mut result_type := typeinfo_union(all_types)

		if hmi.type_table.debug_mode {
			println('[HM_INFERENCER] Final union types: ${all_types}')
			println('[HM_INFERENCER] Final result type: ${result_type}')
		}

		if hmi.type_table.debug_mode {
			println('[HM_INFERENCER] <<< Saindo de infer_block com tipo união: ${result_type}')
		}
		return result_type
	}

	// If no return_types, but we have additional_types, create union
	if additional_types.len > 0 {
		mut all_types := [final_last_type]
		all_types << additional_types
		mut result_type := typeinfo_union(all_types)

		if hmi.type_table.debug_mode {
			println('[HM_INFERENCER] Final union types (no return_types): ${all_types}')
			println('[HM_INFERENCER] Final result type: ${result_type}')
		}

		if hmi.type_table.debug_mode {
			println('[HM_INFERENCER] <<< Saindo de infer_block com tipo união: ${result_type}')
		}
		return result_type
	}

	if hmi.type_table.debug_mode {
		println('[HM_INFERENCER] <<< Saindo de infer_block com tipo: ${final_last_type}')
	}

	return final_last_type
}

// infer_call infers the type of a function call
pub fn (mut hmi HMInferencer) infer_call(expr ast.CallExpr) !TypeInfo {
	if hmi.type_table.debug_mode {
		println('[HM_INFERENCER] infer_call: processing function call')
	}

	// Infer types of arguments
	mut arg_types := []TypeInfo{}
	for i, arg in expr.arguments {
		arg_type := hmi.infer_expression(arg)!
		arg_types << arg_type
		if hmi.type_table.debug_mode {
			println('[HM_INFERENCER] Argument ${i + 1}: ${arg_type}')
		}
	}

	// Try to resolve function name
	if expr.function is ast.LiteralExpr {
		if expr.function.value is ast.AtomLiteral {
			func_name := expr.function.value.value.replace(':', '')
			if hmi.type_table.debug_mode {
				println('[HM_INFERENCER] Looking up function "${func_name}" in environment')
			}

			if scheme := hmi.type_env.lookup(func_name) {
				if hmi.type_table.debug_mode {
					println('[HM_INFERENCER] Found function "${func_name}" with type: ${scheme.type_info}')
				}

				// Extract return type from function type
				if scheme.type_info.generic == 'function' && scheme.type_info.values.len > 0 {
					return_type := scheme.type_info.values.last()
					if hmi.type_table.debug_mode {
						println('[HM_INFERENCER] Function "${func_name}" returns: ${return_type}')
					}
					return return_type
				}
			}

			// If we can't resolve from environment, try to get from type table
			// Look for function return type in the type table
			if func_return_type := hmi.get_function_return_type(func_name, arg_types.len) {
				if hmi.type_table.debug_mode {
					println('[HM_INFERENCER] Found function "${func_name}" return type in type table: ${func_return_type}')
				}
				return func_return_type
			}
		}
	}

	// If we can't resolve the function, return a fresh type variable
	fresh_type := hmi.fresh_type_var()
	if hmi.type_table.debug_mode {
		println('[HM_INFERENCER] Could not resolve function, returning fresh type: ${fresh_type}')
	}

	return fresh_type
}

// infer_tuple infers the type of a tuple expression
pub fn (mut hmi HMInferencer) infer_tuple(expr ast.TupleExpr) !TypeInfo {
	if hmi.type_table.debug_mode {
		println('[HM_INFERENCER] infer_tuple: ${expr.elements.len} elements')
	}

	mut element_types := []TypeInfo{}
	for element in expr.elements {
		element_type := hmi.infer_expression(element)!
		element_types << element_type
	}

	tuple_type := typeinfo_tuple(element_types)
	if hmi.type_table.debug_mode {
		println('[HM_INFERENCER] Tuple type: ${tuple_type}')
	}

	return tuple_type
}

// infer_record_literal infers the type of a record literal
pub fn (mut hmi HMInferencer) infer_record_literal(expr ast.RecordLiteralExpr) !TypeInfo {
	if hmi.type_table.debug_mode {
		println('[HM_INFERENCER] infer_record_literal: ${expr.name}')
	}

	record_type := typeinfo_record(expr.name)
	if hmi.type_table.debug_mode {
		println('[HM_INFERENCER] Record type: ${record_type}')
	}

	return record_type
}

// infer_record_access infers the type of a record field access
pub fn (mut hmi HMInferencer) infer_record_access(expr ast.RecordAccessExpr) !TypeInfo {
	if hmi.type_table.debug_mode {
		println('[HM_INFERENCER] infer_record_access: accessing field ${expr.field}')
	}

	// Infer the type of the record being accessed
	record_type := hmi.infer_expression(expr.record)!
	if hmi.type_table.debug_mode {
		println('[HM_INFERENCER] Record type: ${record_type}')
	}

	// For now, return string type for record field access
	// TODO: Implement proper record field type lookup
	field_type := typeinfo_string()
	if hmi.type_table.debug_mode {
		println('[HM_INFERENCER] Field ${expr.field} type: ${field_type}')
	}

	return field_type
}

// infer_case infers the type of a case expression
pub fn (mut hmi HMInferencer) infer_case(expr ast.CaseExpr) !TypeInfo {
	if hmi.type_table.debug_mode {
		println('[HM_INFERENCER] infer_case: processing case expression')
	}

	// Infer type of the value being matched
	value_type := hmi.infer_expression(expr.value)!
	if hmi.type_table.debug_mode {
		println('[HM_INFERENCER] Case value type: ${value_type}')
	}

	// Collect all case body types
	mut case_body_types := []TypeInfo{}

	// Process each case branch
	for i, case_branch in expr.cases {
		if hmi.type_table.debug_mode {
			println('[HM_INFERENCER] Processing case branch ${i + 1}')
		}

		// Extract variables from the pattern and bind them to the type environment
		hmi.extract_pattern_variables(case_branch.pattern, value_type)

		if hmi.type_table.debug_mode {
			println('[HM_INFERENCER] Extracted pattern variables for case branch ${i + 1}')
		}

		// Infer the type of the case body
		case_body_type := hmi.infer_expression(case_branch.body)!
		case_body_types << case_body_type
		if hmi.type_table.debug_mode {
			println('[HM_INFERENCER] Case branch ${i + 1} body type: ${case_body_type}')
		}
	}

	// Create union type from all case body types
	mut result_type := if case_body_types.len > 0 {
		typeinfo_union(case_body_types)
	} else {
		hmi.fresh_type_var()
	}

	if hmi.type_table.debug_mode {
		println('[HM_INFERENCER] Case expression result type: ${result_type}')
	}

	return result_type
}

// infer_if infers the type of an if expression
pub fn (mut hmi HMInferencer) infer_if(expr ast.IfExpr) !TypeInfo {
	if hmi.type_table.debug_mode {
		println('[HM_INFERENCER] infer_if: processing if expression')
	}

	// Infer type of the condition (should be boolean)
	condition_type := hmi.infer_expression(expr.condition)!
	if hmi.type_table.debug_mode {
		println('[HM_INFERENCER] If condition type: ${condition_type}')
	}

	// Create a fresh type variable for the result type
	result_type := hmi.fresh_type_var()
	if hmi.type_table.debug_mode {
		println('[HM_INFERENCER] If result type variable: ${result_type}')
	}

	// Infer type of the then body
	then_type := hmi.infer_expression(expr.then_body)!
	if hmi.type_table.debug_mode {
		println('[HM_INFERENCER] If then body type: ${then_type}')
	}

	// Add constraint that result type must match then body type
	hmi.add_constraint(result_type, then_type)
	if hmi.type_table.debug_mode {
		println('[HM_INFERENCER] Added constraint: ${result_type} = ${then_type}')
	}

	// Infer type of the else body if present
	if expr.else_body.body.len > 0 {
		else_type := hmi.infer_expression(expr.else_body)!
		if hmi.type_table.debug_mode {
			println('[HM_INFERENCER] If else body type: ${else_type}')
		}

		// Add constraint that both bodies must have the same type
		hmi.add_constraint(result_type, else_type)
		if hmi.type_table.debug_mode {
			println('[HM_INFERENCER] Added constraint: ${result_type} = ${else_type}')
		}
	} else {
		// No else body - if without else returns nil
		nil_result_type := typeinfo_nil()
		if hmi.type_table.debug_mode {
			println('[HM_INFERENCER] If without else returns: ${nil_result_type}')
		}

		// Add constraint that result type must match nil
		hmi.add_constraint(result_type, nil_result_type)
		if hmi.type_table.debug_mode {
			println('[HM_INFERENCER] Added constraint: ${result_type} = ${nil_result_type}')
		}
	}

	if hmi.type_table.debug_mode {
		println('[HM_INFERENCER] If expression result type: ${result_type}')
	}

	return result_type
}

// infer_with infers the type of a with expression
pub fn (mut hmi HMInferencer) infer_with(expr ast.WithExpr) !TypeInfo {
	if hmi.type_table.debug_mode {
		println('[HM_INFERENCER] infer_with: processing with expression')
	}

	// Process bindings first
	for i, binding in expr.bindings {
		if hmi.type_table.debug_mode {
			println('[HM_INFERENCER] Processing with binding ${i + 1}/${expr.bindings.len}')
		}

		// Infer type of the value being matched
		value_type := hmi.infer_expression(binding.value)!
		if hmi.type_table.debug_mode {
			println('[HM_INFERENCER] With binding value type: ${value_type}')
		}

		// Extract variables from the pattern and bind them to the type environment
		hmi.extract_pattern_variables(binding.pattern, value_type)
	}

	// Infer type of the main body
	body_type := hmi.infer_expression(expr.body)!
	if hmi.type_table.debug_mode {
		println('[HM_INFERENCER] With body type: ${body_type}')
	}

	// Infer type of the else body if present
	if expr.else_body.body.len > 0 {
		else_type := hmi.infer_expression(expr.else_body)!
		if hmi.type_table.debug_mode {
			println('[HM_INFERENCER] With else body type: ${else_type}')
		}

		// Add constraint that both bodies must have the same type
		hmi.add_constraint(body_type, else_type)
		if hmi.type_table.debug_mode {
			println('[HM_INFERENCER] Added constraint: ${body_type} = ${else_type}')
		}
	}

	// Check each statement in the body for match expressions
	mut match_return_types := []TypeInfo{}

	// Check each statement in the body for match expressions
	for stmt in expr.body.body {
		if stmt is ast.ExprStmt {
			expr_stmt := stmt as ast.ExprStmt
			if expr_stmt.expr is ast.SimpleMatchExpr {
				// Simple match - include the value type in union (circuit breaker)
				simple_match := expr_stmt.expr as ast.SimpleMatchExpr
				value_type := hmi.infer_expression(simple_match.value)!
				match_return_types << value_type
				if hmi.type_table.debug_mode {
					println('[HM_INFERENCER] Added simple match return type: ${value_type}')
				}
			}
			if expr_stmt.expr is ast.MatchRescueExpr {
				// Match rescue - include the rescue body type in union (circuit breaker)
				match_rescue := expr_stmt.expr as ast.MatchRescueExpr
				rescue_body_type := hmi.infer_expression(match_rescue.rescue_body)!
				match_return_types << rescue_body_type
				if hmi.type_table.debug_mode {
					println('[HM_INFERENCER] Added match rescue return type: ${rescue_body_type}')
				}
			}
		}
	}

	// If there are match expressions, create a union type
	mut result_type := body_type
	if match_return_types.len > 0 {
		// Create union of body type and all match return types
		mut union_values := [body_type]
		union_values << match_return_types

		result_type = TypeInfo{
			generic: 'union'
			values:  union_values
		}

		if hmi.type_table.debug_mode {
			println('[HM_INFERENCER] Created union type for with: ${result_type}')
		}
	}

	if hmi.type_table.debug_mode {
		println('[HM_INFERENCER] With expression result type: ${result_type}')
	}

	return result_type
}

// infer_simple_match infers the type of a simple match expression
pub fn (mut hmi HMInferencer) infer_simple_match(expr ast.SimpleMatchExpr) !TypeInfo {
	if hmi.type_table.debug_mode {
		println('[HM_INFERENCER] infer_simple_match')
	}

	// Infer the type of the value being matched
	value_type := hmi.infer_expression(expr.value)!

	// Extract variables from the pattern and bind them to the type environment
	hmi.extract_pattern_variables(expr.pattern, value_type)

	// For simple match, return the value type
	return value_type
}

// Função auxiliar recursiva para coletar tipos de rescue e o tipo de sucesso final
fn (mut hmi HMInferencer) collect_match_types(expr ast.MatchExpr, mut rescue_types []TypeInfo) !TypeInfo {
	// Inferir o tipo do valor sendo casado
	value_type := hmi.infer_expression(expr.value)!
	// Extrair variáveis do padrão
	if expr.cases.len > 0 {
		hmi.extract_pattern_variables(expr.cases[0].pattern, value_type)
	}

	// Só acumular o tipo de erro propagado se NÃO houver rescue
	if expr.rescue == none {
		rescue_types << value_type
	}

	// Se o próximo expr for outro MatchExpr, descer recursivamente
	if expr.expr is ast.MatchExpr {
		final_success_type := hmi.collect_match_types(expr.expr as ast.MatchExpr, mut
			rescue_types)!
		// Se houver rescue neste nível, acumular
		if expr.rescue != none {
			if expr.rescue is ast.MatchRescueExpr {
				match_rescue := expr.rescue as ast.MatchRescueExpr
				// Vincular a variável de rescue ao ambiente com o tipo do valor
				hmi.type_env.bind(match_rescue.rescue_var, monotype(value_type))
				rescue_type := hmi.infer_block(match_rescue.rescue_body)!
				rescue_types << rescue_type
			}
		}
		return final_success_type
	} else {
		// Último nível: inferir o tipo de sucesso final
		success_type := hmi.infer_expression(expr.expr)!
		if expr.rescue != none {
			if expr.rescue is ast.MatchRescueExpr {
				match_rescue := expr.rescue as ast.MatchRescueExpr
				// Vincular a variável de rescue ao ambiente com o tipo do valor
				hmi.type_env.bind(match_rescue.rescue_var, monotype(value_type))
				rescue_type := hmi.infer_block(match_rescue.rescue_body)!
				rescue_types << rescue_type
			}
		}
		return success_type
	}
}

// infer_match infers the type of a match expression com rescue profundo
pub fn (mut hmi HMInferencer) infer_match(expr ast.MatchExpr) !TypeInfo {
	if hmi.type_table.debug_mode {
		println('[HM_INFERENCER] infer_match (recursivo)')
	}

	mut rescue_types := []TypeInfo{}
	final_success_type := hmi.collect_match_types(expr, mut rescue_types)!
	// Union de todos os tipos de rescue + sucesso final
	mut all_types := rescue_types.clone()
	all_types << final_success_type
	// Remover duplicatas
	all_types = remove_duplicate_types(all_types)
	union_type := typeinfo_union(all_types)

	if hmi.type_table.debug_mode {
		println('[HM_INFERENCER] MatchExpr tipos finais: rescues=${rescue_types}, sucesso=${final_success_type}, union=${union_type}')
	}

	return union_type
}

// infer_match_rescue infers the type of a match_rescue expression
pub fn (mut hmi HMInferencer) infer_match_rescue(expr ast.MatchRescueExpr) !TypeInfo {
	if hmi.type_table.debug_mode {
		println('[HM_INFERENCER] infer_match_rescue: processing match_rescue expression')
	}

	// Infer type of the value being matched
	value_type := hmi.infer_expression(expr.value)!
	if hmi.type_table.debug_mode {
		println('[HM_INFERENCER] MatchRescue value type: ${value_type}')
	}

	// Add the rescue variable to the type environment
	// The rescue variable has the same type as the value being matched
	hmi.type_env.bind(expr.rescue_var, monotype(value_type))
	if hmi.type_table.debug_mode {
		println('[HM_INFERENCER] Bound rescue variable "${expr.rescue_var}" to type: ${value_type}')
	}

	// For match rescue, we need to infer the type of the rescue body
	// The rescue body is a BlockExpr that handles the error case
	rescue_body_type := hmi.infer_expression(expr.rescue_body)!
	if hmi.type_table.debug_mode {
		println('[HM_INFERENCER] MatchRescue rescue body type: ${rescue_body_type}')
	}

	// For match rescue, we return the type of the rescue body
	// The continuation type will be handled by the block expression that contains this match rescue
	result_type := rescue_body_type

	if hmi.type_table.debug_mode {
		println('[HM_INFERENCER] MatchRescue expression result type: ${result_type}')
	}

	return result_type
}

// infer_for infers the type of a for expression (list comprehension)
pub fn (mut hmi HMInferencer) infer_for(expr ast.ForExpr) !TypeInfo {
	if hmi.type_table.debug_mode {
		println('[HM_INFERENCER] infer_for: processing for expression')
	}
	// Inferir o tipo da coleção
	collection_type := hmi.infer_expression(expr.collection)!
	if hmi.type_table.debug_mode {
		println('[HM_INFERENCER] Collection type: ${collection_type}')
	}

	// O tipo da coleção deve ser uma lista
	mut element_type := hmi.fresh_type_var()
	if collection_type.generic == 'list' && collection_type.values.len > 0 {
		element_type = collection_type.values[0]
		if hmi.type_table.debug_mode {
			println('[HM_INFERENCER] Extracted element type from list: ${element_type}')
		}
	} else if collection_type.generic == 'typevar' {
		// Adiciona constraint que collection_type deve ser list(element_type)
		hmi.add_constraint(collection_type, typeinfo_list(element_type))
		if hmi.type_table.debug_mode {
			println('[HM_INFERENCER] Added constraint: ${collection_type} = list(${element_type})')
		}
	}

	// Bind o padrão ao tipo do elemento
	hmi.extract_pattern_variables(expr.pattern, element_type)
	if hmi.type_table.debug_mode {
		println('[HM_INFERENCER] Extracted pattern variables')
	}

	// Processar o guard se presente
	if expr.guard !is ast.LiteralExpr {
		guard_type := hmi.infer_expression(expr.guard)!
		if hmi.type_table.debug_mode {
			println('[HM_INFERENCER] Guard type: ${guard_type}')
		}
		// O guard deve ser boolean
		hmi.add_constraint(guard_type, typeinfo_boolean())
	}

	// Inferir o tipo do corpo do for
	body_type := hmi.infer_expression(expr.body)!
	if hmi.type_table.debug_mode {
		println('[HM_INFERENCER] Body type: ${body_type}')
	}

	// O tipo do for é list(body_type)
	result_type := typeinfo_list(body_type)
	if hmi.type_table.debug_mode {
		println('[HM_INFERENCER] For result type: ${result_type}')
	}
	return result_type
}

// infer_list_literal infers the type of a list literal expression
pub fn (mut hmi HMInferencer) infer_list_literal(expr ast.ListLiteralExpr) !TypeInfo {
	if hmi.type_table.debug_mode {
		println('[HM_INFERENCER] infer_list_literal: processing list literal')
	}
	if expr.elements.len == 0 {
		// Lista vazia: list(any())
		return typeinfo_list(typeinfo_any())
	}
	// Inferir o tipo de todos os elementos
	mut element_types := []TypeInfo{}
	// Se todos os elementos são mapas literais, coletar tipos dos campos
	mut all_maps := true
	mut field_types := map[string]TypeInfo{}
	for el in expr.elements {
		element_types << hmi.infer_expression(el)!
		if el is ast.MapLiteralExpr {
			for entry in el.entries {
				if entry.key is ast.LiteralExpr && entry.key.value is ast.AtomLiteral {
					atom_key := entry.key.value as ast.AtomLiteral
					field_value_type := hmi.infer_expression(entry.value) or { typeinfo_any() }
					// Verificar se o campo já existe com tipo diferente
					if existing_type := field_types[atom_key.value] {
						if !types_are_compatible(existing_type, field_value_type) {
							hmi.error_reporter.report_error(.type_error, 'Type mismatch for map field "${atom_key.value}": ${existing_type} vs ${field_value_type}',
								el.position, 'Ensure all map fields have consistent types')
							return error('Type mismatch detected')
						}
					}
					field_types[atom_key.value] = field_value_type
				}
			}
		} else {
			all_maps = false
		}
	}
	if all_maps && field_types.len > 0 {
		hmi.last_map_field_types = field_types.clone()
	} else {
		hmi.last_map_field_types = map[string]TypeInfo{}
	}
	// Unificar todos os tipos dos elementos
	mut unified_type := element_types[0]
	for t in element_types[1..] {
		if !types_are_compatible(unified_type, t) {
			unified_type = typeinfo_any()
			break
		}
	}
	return typeinfo_list(unified_type)
}

// infer_map_literal infers the type of a map literal expression
pub fn (mut hmi HMInferencer) infer_map_literal(expr ast.MapLiteralExpr) !TypeInfo {
	if hmi.type_table.debug_mode {
		println('[HM_INFERENCER] infer_map_literal: processing map literal')
	}
	if expr.entries.len == 0 {
		// Mapa vazio: map(any(), any())
		return typeinfo_map(typeinfo_any(), typeinfo_any())
	}
	// Inferir o tipo de todas as chaves e valores
	mut key_types := []TypeInfo{}
	mut value_types := []TypeInfo{}
	for entry in expr.entries {
		key_type := hmi.infer_expression(entry.key)!
		value_type := hmi.infer_expression(entry.value)!
		key_types << key_type
		value_types << value_type
		if hmi.type_table.debug_mode {
			println('[HM_INFERENCER] Map entry: key=${key_type}, value=${value_type}')
		}
	}
	// Unificar todos os tipos das chaves
	mut unified_key_type := key_types[0]
	for t in key_types[1..] {
		if !types_are_compatible(unified_key_type, t) {
			unified_key_type = typeinfo_any()
			break
		}
	}
	// Unificar todos os tipos dos valores
	mut unified_value_type := value_types[0]
	for t in value_types[1..] {
		if !types_are_compatible(unified_value_type, t) {
			unified_value_type = typeinfo_any()
			break
		}
	}
	if hmi.type_table.debug_mode {
		println('[HM_INFERENCER] Unified map type: map(${unified_key_type}, ${unified_value_type})')
	}
	return typeinfo_map(unified_key_type, unified_value_type)
}

pub fn (mut hmi HMInferencer) infer_unary(expr ast.UnaryExpr) !TypeInfo {
	operand_type := hmi.infer_expression(expr.operand)!
	result_type := match expr.op {
		.minus {
			if operand_type.generic == 'integer' {
				typeinfo_integer()
			} else if operand_type.generic == 'float' {
				typeinfo_float()
			} else {
				typeinfo_integer()
			}
		}
		.not {
			typeinfo_boolean()
		}
		.bitwise_not {
			typeinfo_integer()
		}
	}
	return result_type
}

// extract_pattern_variables extracts variables from a pattern and binds them to the type environment
fn (mut hmi HMInferencer) extract_pattern_variables(pattern ast.Pattern, value_type TypeInfo) {
	if hmi.type_table.debug_mode {
		println('[HM_INFERENCER] extract_pattern_variables: pattern=${typeof(pattern).name}, value_type=${value_type}')
	}

	// Descompacta union de um único valor
	mut substituted_type := value_type
	if substituted_type.generic == 'union' && substituted_type.values.len == 1 {
		if hmi.type_table.debug_mode {
			println('[HM_INFERENCER] [UNION] Descompactando union de um único valor: ${substituted_type.values[0]}')
		}
		substituted_type = substituted_type.values[0]
	}

	if hmi.constraints.len > 0 {
		if hmi.constraints.len > 10 {
			return
		}

		substitution := hmi.resolve_constraints() or {
			Substitution{
				subst: map[int]TypeInfo{}
			}
		}
		substituted_type = substitution.apply_to_type(substituted_type)
		if hmi.type_table.debug_mode {
			println('[HM_INFERENCER] Applied substitution: ${value_type} -> ${substituted_type}')
		}
	}

	match pattern {
		ast.VarPattern {
			mut bind_type := substituted_type
			if bind_type.generic == 'any' && pattern.name.contains('.') {
				if pattern.name in hmi.record_field_types {
					bind_type = hmi.record_field_types[pattern.name]
				}
			}
			hmi.type_env.bind(pattern.name, monotype(bind_type))
			if hmi.type_table.debug_mode {
				println('[HM_INFERENCER] Bound pattern variable "${pattern.name}" to type: ${bind_type}')
			}
		}
		ast.TuplePattern {
			// For tuple patterns, we need to extract the types of each element
			if hmi.type_table.debug_mode {
				println('[HM_INFERENCER] [TUPLE] Pattern: ${pattern}, Type: ${substituted_type}')
				println('[HM_INFERENCER] [TUPLE] substituted_type.generic: ${substituted_type.generic}')
				println('[HM_INFERENCER] [TUPLE] substituted_type.values.len: ${substituted_type.values.len}')
				println('[HM_INFERENCER] [TUPLE] pattern.elements.len: ${pattern.elements.len}')
			}
			if substituted_type.generic == 'tuple'
				&& substituted_type.values.len == pattern.elements.len {
				for i, element_pattern in pattern.elements {
					if i < substituted_type.values.len {
						element_type := substituted_type.values[i]
						if hmi.type_table.debug_mode {
							println('[HM_INFERENCER] [TUPLE] Element ${i}: pattern=${typeof(element_pattern).name}, type=${element_type}')
						}
						match element_pattern {
							ast.VarPattern {
								if hmi.type_table.debug_mode {
									println('[HM_INFERENCER] [TUPLE] Binding variable "${element_pattern.name}" to type: ${element_type}')
								}
								hmi.type_env.bind(element_pattern.name, monotype(element_type))
							}
							else {
								hmi.extract_pattern_variables(element_pattern, element_type)
							}
						}
					}
				}
			} else {
				if hmi.type_table.debug_mode {
					println('[HM_INFERENCER] [TUPLE] Cannot determine exact tuple types, using fallback')
				}
				for element_pattern in pattern.elements {
					hmi.extract_pattern_variables(element_pattern, substituted_type)
				}
			}
		}
		ast.ListConsPattern {
			// For list cons patterns, head gets the element type, tail gets the list type
			if substituted_type.generic == 'list' && substituted_type.values.len > 0 {
				element_type := substituted_type.values[0]
				hmi.extract_pattern_variables(pattern.head, element_type)
				hmi.extract_pattern_variables(pattern.tail, substituted_type)
			} else {
				// Create a fresh type variable for the element type
				element_type := hmi.fresh_type_var()
				// Create a list type with the element type
				list_type := typeinfo_list(element_type)
				if hmi.type_table.debug_mode {
					println('[HM_INFERENCER] Created list type: ${list_type} for ListConsPattern')
				}
				// Extract variables with the created types
				hmi.extract_pattern_variables(pattern.head, element_type)
				hmi.extract_pattern_variables(pattern.tail, list_type)
			}
		}
		ast.ListLiteralPattern {
			// For list literal patterns, each element gets the corresponding type
			if substituted_type.generic == 'list' && substituted_type.values.len > 0 {
				element_type := substituted_type.values[0]
				for element_pattern in pattern.elements {
					hmi.extract_pattern_variables(element_pattern, element_type)
				}
			} else {
				// Fallback: bind all to the value type
				for element_pattern in pattern.elements {
					hmi.extract_pattern_variables(element_pattern, substituted_type)
				}
			}
		}
		ast.MapPattern {
			// For map patterns, we need to handle key-value pairs
			if substituted_type.generic == 'map' && substituted_type.values.len >= 2 {
				map_key_type := substituted_type.values[0]
				map_value_type := substituted_type.values[1]
				if hmi.type_table.debug_mode {
					println('[HM_INFERENCER] [MAP] Key type: ${map_key_type}, Value type: ${map_value_type}')
				}
				for entry in pattern.entries {
					match entry.key {
						ast.AtomPattern {
							key_name := entry.key.value
							if hmi.type_table.debug_mode {
								println('[HM_INFERENCER] [MAP] Processing key: ${key_name}')
							}
							// Se temos tipos deduzidos dos campos, usar
							field_type := if key_name in hmi.last_map_field_types {
								hmi.last_map_field_types[key_name]
							} else {
								map_value_type
							}
							hmi.extract_pattern_variables(entry.value, field_type)
							// Limpar após uso para não vazar contexto
							hmi.last_map_field_types.delete(key_name)
						}
						else {
							hmi.extract_pattern_variables(entry.key, map_key_type)
							hmi.extract_pattern_variables(entry.value, map_value_type)
						}
					}
				}
			} else {
				if hmi.type_table.debug_mode {
					println('[HM_INFERENCER] [MAP] Cannot determine map types, using fallback')
				}
				for entry in pattern.entries {
					hmi.extract_pattern_variables(entry.key, substituted_type)
					hmi.extract_pattern_variables(entry.value, substituted_type)
				}
			}
		}
		ast.RecordPattern {
			mut record_type := substituted_type
			if record_type.generic != 'record' && pattern.name != '' {
				record_type = typeinfo_record(pattern.name)
			}
			hmi.add_constraint(substituted_type, record_type)
			if record_type.generic == 'record' {
				record_name := record_type.value or { 'unknown' }
				for field in pattern.fields {
					key := '${record_name}.${field.name}'
					if key in hmi.record_field_types {
						field_type := hmi.record_field_types[key]
						if hmi.type_table.debug_mode {
							println('[HM_INFERENCER] [RECORD] Field ${key}: ${field_type}')
						}
						hmi.extract_pattern_variables(field.pattern, field_type)
					} else {
						if hmi.type_table.debug_mode {
							println('[HM_INFERENCER] [RECORD] Unknown field ${key}, using any type')
						}
						hmi.extract_pattern_variables(field.pattern, typeinfo_any())
					}
				}
			} else {
				// Fallback: use any type for all fields
				if hmi.type_table.debug_mode {
					println('[HM_INFERENCER] [RECORD] Cannot determine record type, using any for all fields')
				}
				for field in pattern.fields {
					hmi.extract_pattern_variables(field.pattern, typeinfo_any())
				}
			}
		}
		else {
			// For other patterns (wildcard, literal, atom), no variables to bind
			if hmi.type_table.debug_mode {
				println('[HM_INFERENCER] No variables to extract from pattern type: ${pattern}')
			}
		}
	}
}

// get_function_return_type gets the return type of a function from the type table
fn (hmi HMInferencer) get_function_return_type(func_name string, arity int) ?TypeInfo {
	mut best_candidate := TypeInfo{}

	for _, type_info in hmi.type_table.get_all_types() {
		if type_info.generic == 'typevar' || type_info.generic == 'function' {
			continue
		}
		if type_info.generic == 'union' || type_info.generic == 'tuple'
			|| type_info.generic == 'string' || type_info.generic == 'atom'
			|| type_info.generic == 'boolean' || type_info.generic == 'integer' {
			best_candidate = type_info
		}
	}
	if best_candidate.generic != '' {
		return best_candidate
	}
	return none
}

// Helper methods
fn (hmi HMInferencer) get_position_string(expr ast.Expr) string {
	pos := hmi.get_expression_position(expr)
	return '${pos.line}:${pos.column}'
}

fn (hmi HMInferencer) get_expression_text(expr ast.Expr) string {
	return match expr {
		ast.VariableExpr {
			expr.name
		}
		ast.LiteralExpr {
			match expr.value {
				ast.IntegerLiteral { expr.value.value.str() }
				ast.FloatLiteral { expr.value.value.str() }
				ast.StringLiteral { '"${expr.value.value}"' }
				ast.BooleanLiteral { expr.value.value.str() }
				ast.AtomLiteral { expr.value.value }
				ast.NilLiteral { 'nil' }
			}
		}
		ast.BinaryExpr {
			'${expr.op}'
		}
		ast.AssignExpr {
			'${expr.name} = ...'
		}
		ast.BlockExpr {
			'block'
		}
		ast.CallExpr {
			'call'
		}
		ast.TupleExpr {
			'{...}'
		}
		ast.RecordLiteralExpr {
			'${expr.name}{...}'
		}
		else {
			'expr'
		}
	}
}

fn (hmi HMInferencer) get_expression_position(expr ast.Expr) ast.Position {
	return match expr {
		ast.VariableExpr { expr.position }
		ast.LiteralExpr { expr.position }
		ast.BinaryExpr { expr.position }
		ast.AssignExpr { expr.position }
		ast.BlockExpr { expr.position }
		ast.CallExpr { expr.position }
		ast.TupleExpr { expr.position }
		ast.RecordLiteralExpr { expr.position }
		else { ast.Position{} }
	}
}

// get_errors returns the compilation errors from the error reporter
pub fn (hmi HMInferencer) get_errors() []errors.CompilationError {
	return hmi.error_reporter.get_compilation_errors()
}

// has_errors returns true if there are any errors
pub fn (hmi HMInferencer) has_errors() bool {
	return hmi.error_reporter.errors.len > 0
}

// clear_errors limpa os erros do error_reporter
pub fn (mut hmi HMInferencer) clear_errors() {
	hmi.error_reporter.errors.clear()
}
