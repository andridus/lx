module analysis

import ast

// HMInferencer implements basic HM type inference
pub struct HMInferencer {
pub mut:
	type_table    TypeTable
	type_env      TypeEnv
	next_type_var int = 1
	constraints   []Constraint
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
		type_table:    new_type_table()
		type_env:      new_type_env()
		next_type_var: 1
		constraints:   []
	}
}

// new_hm_inferencer_with_debug creates a new HM inferencer with debug mode
pub fn new_hm_inferencer_with_debug() HMInferencer {
	return HMInferencer{
		type_table:    new_type_table_with_debug()
		type_env:      new_type_env()
		next_type_var: 1
		constraints:   []
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

// infer_expression infers the type of an expression using HM
pub fn (mut hmi HMInferencer) infer_expression(expr ast.Expr) !TypeInfo {
	ast_id := ast.get_expr_ast_id(expr)

	if hmi.type_table.debug_mode {
		expr_type := match expr {
			ast.VariableExpr { 'VariableExpr(${expr.name})' }
			ast.LiteralExpr { 'LiteralExpr' }
			ast.BinaryExpr { 'BinaryExpr' }
			ast.AssignExpr { 'AssignExpr(${expr.name})' }
			ast.BlockExpr { 'BlockExpr' }
			else { 'Other' }
		}
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

	return error('Variable ${expr.name} not found in environment')
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

// infer_block infers the type of a block expression
pub fn (mut hmi HMInferencer) infer_block(expr ast.BlockExpr) !TypeInfo {
	if hmi.type_table.debug_mode {
		println('[HM_INFERENCER] infer_block: ${expr.body.len} statements')
	}

	mut last_type := typeinfo_nil()

	for i, stmt in expr.body {
		if hmi.type_table.debug_mode {
			println('[HM_INFERENCER] Processing statement ${i + 1}/${expr.body.len}')
		}

		match stmt {
			ast.ExprStmt {
				if hmi.type_table.debug_mode {
					println('[HM_INFERENCER] Found ExprStmt, inferring expression')
				}
				last_type = hmi.infer_expression(stmt.expr)!
				if hmi.type_table.debug_mode {
					println('[HM_INFERENCER] ExprStmt inferred as: ${last_type}')
				}
			}
			else {
				if hmi.type_table.debug_mode {
					println('[HM_INFERENCER] Skipping non-expression statement')
				}
			}
		}
	}

	if hmi.type_table.debug_mode {
		println('[HM_INFERENCER] Block final type: ${last_type}')
	}

	return last_type
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
