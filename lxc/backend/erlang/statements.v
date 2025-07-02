module erlang

import ast

// generate_statement generates code for a single statement
pub fn (gen ErlangGenerator) generate_statement(stmt ast.Stmt) string {
	match stmt {
		ast.ExprStmt {
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

// infer_function_return_type infers the return type of a function based on its body
fn (gen ErlangGenerator) infer_function_return_type(clause ast.FunctionClause) string {
	if clause.body.len == 0 {
		return 'ok'
	}

	// Get the last statement in the body (the return value)
	last_stmt := clause.body[clause.body.len - 1]

	return match last_stmt {
		ast.ExprStmt {
			gen.infer_expression_return_type_with_context(last_stmt.expr, clause.parameters)
		}
		else {
			'ok'
		}
	}
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
		else {
			'any()'
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
		.and, .or, .andalso, .orelse {
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

// generate_function generates code for function definitions
pub fn (gen ErlangGenerator) generate_function(func ast.FunctionStmt) string {
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
			parameters := clause.parameters.map(gen.generate_pattern(it))
			mut guard := ''
			if clause.guard is ast.LiteralExpr {
				literal := clause.guard as ast.LiteralExpr
				if literal.value is ast.BooleanLiteral {
					boolean := literal.value as ast.BooleanLiteral
					if !boolean.value {
						guard = ' when ' + gen.generate_expression(clause.guard)
					}
				} else {
					guard = ' when ' + gen.generate_expression(clause.guard)
				}
			} else {
				guard = ' when ' + gen.generate_expression(clause.guard)
			}
			body := clause.body.map(gen.generate_statement(it))
			// Emit all statements separated by ',' except the last one (Erlang style)
			body_code := if body.len > 1 {
				body[..body.len - 1].join(',\n') + ',\n' + body[body.len - 1]
			} else if body.len == 1 {
				body[0]
			} else {
				'ok'
			}
			clause_strings << '${func.name}(${parameters.join(', ')})${guard} ->\n${body_code}'
		}

		// Add spec line before function definition
		function_definitions << spec_line
		function_definitions << clause_strings.join(';\n') + '.'
		function_definitions << '\n'
	}
	println(function_definitions.join(''))
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
