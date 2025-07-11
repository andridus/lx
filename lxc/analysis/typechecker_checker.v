module analysis

import ast
import errors

pub struct TypeChecker {
pub mut:
	context            &TypeContext
	expression_checker ExpressionChecker
	errors             []errors.CompilationError
	warnings           []errors.CompilationError
}

pub fn new_type_checker() TypeChecker {
	context := new_type_context()
	return TypeChecker{
		context:            &context
		expression_checker: new_expression_checker(&context)
		errors:             []
		warnings:           []
	}
}

// Nova função: faz uma primeira passagem para coletar tipos de retorno de todas as funções
pub fn (mut tc TypeChecker) precollect_function_return_types(functions []ast.FunctionStmt) {
	for stmt in functions {
		for clause in stmt.clauses {
			arity := clause.parameters.len
			ret_type := tc.expression_checker.infer_expression_type_info(clause.body)
			tc.context.store_function_return_type(stmt.name, arity, ret_type)
		}
	}
}

// Modificar check_module para chamar a precollect antes de checar funções
pub fn (mut tc TypeChecker) check_module(mod_stmt ast.ModuleStmt) TypeCheckResult {
	// Filtrar apenas FunctionStmt
	mut functions := []ast.FunctionStmt{}
	for stmt in mod_stmt.statements {
		if stmt is ast.FunctionStmt {
			functions << (stmt as ast.FunctionStmt)
		}
	}
	tc.precollect_function_return_types(functions)
	for stmt in mod_stmt.statements {
		tc.check_statement(stmt)
	}
	return TypeCheckResult{
		context:  tc.context
		errors:   tc.errors
		warnings: tc.warnings
	}
}

pub struct TypeCheckResult {
pub:
	context  &TypeContext
	errors   []errors.CompilationError
	warnings []errors.CompilationError
}

fn (mut tc TypeChecker) check_statement(stmt ast.Stmt) {
	match stmt {
		ast.ExprStmt { tc.check_expression_statement(stmt) }
		ast.FunctionStmt { tc.check_function_statement(stmt) }
		ast.RecordDefStmt { tc.check_record_statement(stmt) }
		ast.TypeDefStmt { tc.check_type_statement(stmt) }
		ast.TypeAliasStmt { tc.check_type_alias_statement(stmt) }
		ast.ModuleStmt {}
	}
}

fn (mut tc TypeChecker) check_expression_statement(stmt ast.ExprStmt) {
	tc.expression_checker.check_expression(stmt.expr)
	tc.errors << tc.expression_checker.errors
	tc.expression_checker.errors = []
}

fn (mut tc TypeChecker) check_function_statement(stmt ast.FunctionStmt) {
	// First pass: infer and store return types for all functions
	// Removed first pass to avoid interference with variable bindings

	// Second pass: check function bodies with proper type context
	// Group clauses by arity to share context
	mut clauses_by_arity := map[int][]ast.FunctionClause{}
	for clause in stmt.clauses {
		arity := clause.parameters.len
		if arity !in clauses_by_arity {
			clauses_by_arity[arity] = []ast.FunctionClause{}
		}
		clauses_by_arity[arity] << clause
	}

	// Process each arity group with shared context
	for arity, clauses in clauses_by_arity {
		// Create shared context for this arity
		function_context := tc.context.enter_function('${stmt.name}_${arity}')
		c := &function_context
		tc.context = c

		// First pass: bind all parameters for this arity
		for clause in clauses {
			tc.bind_parameters_only(clause, stmt.name, arity)
		}

		// Second pass: check function bodies and infer return types
		for clause in clauses {
			tc.check_function_body_only(clause, stmt.name, arity)
		}

		// Exit function context and propagate to parent
		if mut parent := tc.context.exit_function() {
			// Propagar function_param_types do filho para o pai
			for k, v in tc.context.function_param_types {
				parent.function_param_types[k] = v
			}
			// Propagar function_return_types do filho para o pai
			for k, v in tc.context.function_return_types {
				parent.function_return_types[k] = v
			}
			// Propagar bindings do filho para o pai
			for k, v in tc.context.bindings {
				parent.bindings[k] = v
			}
			tc.context = parent
		}
	}
}

fn (mut tc TypeChecker) check_function_clause_with_name(clause ast.FunctionClause, fn_name string, arity int) {
	// Bind parameters to context and collect their types FIRST
	mut param_types := []TypeInfo{}
	for param in clause.parameters {
		param_type := tc.check_pattern_and_get_type(param)
		param_types << param_type
	}

	// Store parameter types for union generation
	tc.context.merge_function_param_types(fn_name, arity, param_types)

	// Check function body
	println('DEBUG: Checking function body for ${fn_name}/${arity}')
	tc.expression_checker.check_expression(clause.body)
	tc.errors << tc.expression_checker.errors
	tc.expression_checker.errors = []

	// Atualizar o tipo de retorno da função após checagem real do corpo
	println('DEBUG: Inferring return type for ${fn_name}/${arity}')
	return_type := tc.expression_checker.infer_expression_type_info(clause.body)
	println('DEBUG: Inferred return type: ${return_type.str()}')
	tc.context.merge_function_return_type(fn_name, arity, return_type)

	// Validate return type if specified
	if return_type_expr := clause.return_type {
		expected_type := tc.type_expr_to_type_info(return_type_expr)
		actual_type := tc.expression_checker.infer_expression_type_info(clause.body)

		if !types_are_compatible(expected_type, actual_type) {
			tc.report_error('Function return type mismatch: expected ${expected_type.str()}, got ${actual_type.str()}',
				'Make sure the function body returns the declared type', clause.body.position)
		}
	}
}

fn (mut tc TypeChecker) bind_parameters_only(clause ast.FunctionClause, fn_name string, arity int) {
	// Bind parameters to context and collect their types
	mut param_types := []TypeInfo{}
	for param in clause.parameters {
		param_type := tc.check_pattern_and_get_type(param)
		param_types << param_type
	}

	// Store parameter types for union generation
	tc.context.merge_function_param_types(fn_name, arity, param_types)
}

fn (mut tc TypeChecker) check_function_body_only(clause ast.FunctionClause, fn_name string, arity int) {
	// Check function body

	// Create a new ExpressionChecker with the correct context
	mut ec := analysis.new_expression_checker(tc.context)
	ec.check_expression(clause.body)
	tc.errors << ec.errors

	// Atualizar o tipo de retorno da função após checagem real do corpo
	return_type := ec.infer_expression_type_info(clause.body)
	tc.context.merge_function_return_type(fn_name, arity, return_type)

	// Validate return type if specified
	if return_type_expr := clause.return_type {
		expected_type := tc.type_expr_to_type_info(return_type_expr)
		actual_type := ec.infer_expression_type_info(clause.body)

		if !types_are_compatible(expected_type, actual_type) {
			tc.report_error('Function return type mismatch: expected ${expected_type.str()}, got ${actual_type.str()}',
				'Make sure the function body returns the declared type', clause.body.position)
		}
	}
}

// Check pattern and return its type
fn (mut tc TypeChecker) check_pattern_and_get_type(pattern ast.Pattern) TypeInfo {
	match pattern {
		ast.VarPattern {
			// For variable patterns, infer type from annotation or use any
			if type_ann := pattern.type_annotation {
				type_info := tc.type_expr_to_type_info(type_ann)
				tc.context.bind(pattern.name, type_info, pattern.position)
				return type_info
			} else {
				type_info := typeinfo_any()
				tc.context.bind(pattern.name, type_info, pattern.position)
				return type_info
			}
		}
		ast.AtomPattern {
			return typeinfo_atom_value(pattern.value)
		}
		ast.LiteralPattern {
			return typeinfo_from_literal(pattern.value)
		}
		ast.TuplePattern {
			mut element_types := []TypeInfo{}
			for element in pattern.elements {
				element_type := tc.check_pattern_and_get_type(element)
				element_types << element_type
			}
			return typeinfo_tuple(element_types)
		}
		ast.ListLiteralPattern {
			if pattern.elements.len == 0 {
				return typeinfo_list(typeinfo_any())
			}
			first_type := tc.check_pattern_and_get_type(pattern.elements[0])
			return typeinfo_list(first_type)
		}
		ast.ListConsPattern {
			head_type := tc.check_pattern_and_get_type(pattern.head)
			return typeinfo_list(head_type)
		}
		ast.ListEmptyPattern {
			return typeinfo_list(typeinfo_any())
		}
		ast.MapPattern {
			return typeinfo_map(typeinfo_any(), typeinfo_any())
		}
		ast.RecordPattern {
			return typeinfo_record('record')
		}
		ast.BinaryPattern {
			return TypeInfo{
				generic: 'bitstring'
				value:   none
				values:  []
			}
		}
		ast.WildcardPattern {
			return typeinfo_any()
		}
	}
	// Default return in case no match (should not happen)
	return typeinfo_any()
}

// Mantém a função original para compatibilidade
fn (mut tc TypeChecker) check_function_clause(clause ast.FunctionClause) {
	tc.check_function_clause_with_name(clause, '<unknown>', 0) // Assuming arity 0 for compatibility
}

fn (mut tc TypeChecker) check_pattern(pattern ast.Pattern) {
	match pattern {
		ast.VarPattern { tc.check_variable_pattern(pattern) }
		ast.AtomPattern { tc.check_atom_pattern(pattern) }
		ast.ListConsPattern { tc.check_list_cons_pattern(pattern) }
		ast.ListEmptyPattern { tc.check_list_empty_pattern(pattern) }
		ast.ListLiteralPattern { tc.check_list_literal_pattern(pattern) }
		ast.TuplePattern { tc.check_tuple_pattern(pattern) }
		ast.MapPattern { tc.check_map_pattern(pattern) }
		ast.RecordPattern { tc.check_record_pattern(pattern) }
		ast.BinaryPattern { tc.check_binary_pattern(pattern) }
		ast.WildcardPattern { tc.check_wildcard_pattern(pattern) }
		ast.LiteralPattern { tc.check_literal_pattern(pattern) }
	}
}

fn (mut tc TypeChecker) check_variable_pattern(pattern ast.VarPattern) {
	// Bind variable to context with a fresh type variable
	type_info := TypeInfo{
		generic: 'var'
		value:   pattern.name
	}
	tc.context.bind(pattern.name, type_info, pattern.position)
}

fn (mut tc TypeChecker) check_atom_pattern(pattern ast.AtomPattern) {
	// Atom patterns don't introduce new bindings
}

fn (mut tc TypeChecker) check_list_cons_pattern(pattern ast.ListConsPattern) {
	tc.check_pattern(pattern.head)
	tc.check_pattern(pattern.tail)
}

fn (mut tc TypeChecker) check_list_empty_pattern(pattern ast.ListEmptyPattern) {
	// Empty list patterns don't introduce new bindings
}

fn (mut tc TypeChecker) check_list_literal_pattern(pattern ast.ListLiteralPattern) {
	for element in pattern.elements {
		tc.check_pattern(element)
	}
}

fn (mut tc TypeChecker) check_tuple_pattern(pattern ast.TuplePattern) {
	for element in pattern.elements {
		tc.check_pattern(element)
	}
}

fn (mut tc TypeChecker) check_map_pattern(pattern ast.MapPattern) {
	for entry in pattern.entries {
		tc.check_pattern(entry.key)
		tc.check_pattern(entry.value)
	}
}

fn (mut tc TypeChecker) check_record_pattern(pattern ast.RecordPattern) {
	for field in pattern.fields {
		tc.check_pattern(field.pattern)
	}
}

fn (mut tc TypeChecker) check_binary_pattern(pattern ast.BinaryPattern) {
	// Binary patterns don't introduce new bindings
}

fn (mut tc TypeChecker) check_wildcard_pattern(pattern ast.WildcardPattern) {
	// Wildcard patterns don't introduce new bindings
}

fn (mut tc TypeChecker) check_literal_pattern(pattern ast.LiteralPattern) {
	// Literal patterns don't introduce new bindings
}

fn (mut tc TypeChecker) check_record_statement(stmt ast.RecordDefStmt) {
	// Record statements define new types
	// Store record type information in context
	tc.context.record_types[stmt.name] = stmt.name
}

fn (mut tc TypeChecker) check_type_statement(stmt ast.TypeDefStmt) {
	// Type statements define type aliases
	// Store type alias information in context
	tc.context.type_aliases[stmt.name] = any_type // Placeholder
}

fn (mut tc TypeChecker) check_type_alias_statement(stmt ast.TypeAliasStmt) {
	// Type alias statements define type aliases
	// Store type alias information in context
	tc.context.type_aliases[stmt.name] = any_type // Placeholder
}

// New TypeInfo-based methods
pub fn (mut tc TypeChecker) infer_expression_type_info(expr ast.Expr) TypeInfo {
	return tc.expression_checker.infer_expression_type_info(expr)
}

pub fn (mut tc TypeChecker) is_numeric_type_info(type_info TypeInfo) bool {
	return tc.expression_checker.is_numeric_type_info(type_info)
}

pub fn (mut tc TypeChecker) types_are_compatible_info(expected TypeInfo, actual TypeInfo) bool {
	return types_are_compatible(expected, actual)
}

// Legacy methods for backwards compatibility - simplified
pub fn (mut tc TypeChecker) infer_expression_type(expr ast.Expr) TypeExpr {
	return tc.expression_checker.infer_expression_type(expr)
}

pub fn (mut tc TypeChecker) is_numeric_type(type_expr TypeExpr) bool {
	return tc.expression_checker.is_numeric_type(type_expr)
}

pub fn (mut tc TypeChecker) types_are_compatible(type1 TypeExpr, type2 TypeExpr) bool {
	// Simple compatibility check without external dependencies
	return type1.str() == type2.str() || type1.str() == 'any' || type2.str() == 'any'
}

fn (mut tc TypeChecker) type_expr_to_type_info(type_expr ast.TypeExpression) TypeInfo {
	return match type_expr {
		ast.SimpleTypeExpr {
			match type_expr.name {
				'integer' { typeinfo_integer() }
				'float' { typeinfo_float() }
				'string' { typeinfo_string() }
				'boolean' { typeinfo_boolean() }
				'atom' { typeinfo_atom() }
				'nil' { typeinfo_nil() }
				'any' { typeinfo_any() }
				else {
					if type_expr.name.len > 0 && type_expr.name[0].is_capital() {
						typeinfo_record(type_expr.name)
					} else {
						typeinfo_atom_value(type_expr.name)
					}
				}
			}
		}
		ast.UnionTypeExpr {
			types := type_expr.types.map(tc.type_expr_to_type_info(it))
			typeinfo_union(types)
		}
		ast.ListTypeExpr {
			element_type := tc.type_expr_to_type_info(type_expr.element_type)
			typeinfo_list(element_type)
		}
		ast.TupleTypeExpr {
			element_types := type_expr.element_types.map(tc.type_expr_to_type_info(it))
			typeinfo_tuple(element_types)
		}
		ast.MapTypeExpr {
			key_type := tc.type_expr_to_type_info(type_expr.key_type)
			value_type := tc.type_expr_to_type_info(type_expr.value_type)
			typeinfo_map(key_type, value_type)
		}
		ast.FunctionTypeExpr {
			return_type := tc.type_expr_to_type_info(type_expr.return_type)
			TypeInfo{
				generic: 'union'
				value:   none
				values:  [return_type]
			}
		}
		ast.VariableTypeExpr {
			typeinfo_atom_value(type_expr.name)
		}
		ast.RecordTypeExpr {
			typeinfo_record(type_expr.name)
		}
	}
}

fn (mut tc TypeChecker) report_error(message string, suggestion string, position ast.Position) {
	error := errors.new_compilation_error(errors.TypeError{
		message:    message
		suggestion: suggestion
	}, position, message)
	tc.errors << error
}
