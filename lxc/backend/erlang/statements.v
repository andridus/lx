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

		function_definitions << clause_strings.join(';\n') + '.'
	}

	return function_definitions.join('\n\n')
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
