module analysis1

import ast
import errors

pub struct TypeChecker {
pub mut:
	context &TypeContext
	expression_checker ExpressionChecker
	errors []errors.CompilationError
	warnings []errors.CompilationError
}

pub fn new_type_checker() TypeChecker {
	context := new_type_context()
	return TypeChecker{
		context: &context
		expression_checker: new_expression_checker(&context)
		errors: []
		warnings: []
	}
}

pub fn (mut tc TypeChecker) check_module(module_stmt ast.ModuleStmt) TypeCheckResult {
	mut ctx := new_type_context()
	tc.context = &ctx
	tc.errors = []
	tc.warnings = []

	// Check all statements in the module
	for stmt in module_stmt.statements {
		tc.check_statement(stmt)
	}

	return TypeCheckResult{
		context: tc.context
		errors: tc.errors
		warnings: tc.warnings
	}
}

pub struct TypeCheckResult {
pub:
	context &TypeContext
	errors []errors.CompilationError
	warnings []errors.CompilationError
}

fn (mut tc TypeChecker) check_statement(stmt ast.Stmt) {
	match stmt {
		ast.ExprStmt { tc.check_expression_statement(stmt) }
		ast.FunctionStmt { tc.check_function_statement(stmt) }
		ast.RecordDefStmt { tc.check_record_statement(stmt) }
		ast.TypeDefStmt { tc.check_type_statement(stmt) }
		ast.TypeAliasStmt { tc.check_type_alias_statement(stmt) }
		ast.ModuleStmt { /* Module statements don't need type checking */ }
	}
}

fn (mut tc TypeChecker) check_expression_statement(stmt ast.ExprStmt) {
	tc.expression_checker.check_expression(stmt.expr)
	tc.errors << tc.expression_checker.errors
	tc.expression_checker.errors = []
}

fn (mut tc TypeChecker) check_function_statement(stmt ast.FunctionStmt) {
	// Group clauses by arity for proper type handling
	mut clauses_by_arity := map[int][]ast.FunctionClause{}
	for clause in stmt.clauses {
		arity := clause.parameters.len
		if arity !in clauses_by_arity {
			clauses_by_arity[arity] = []ast.FunctionClause{}
		}
		clauses_by_arity[arity] << clause
	}

	// Process each arity group
	for arity, clauses in clauses_by_arity {
		// Coletar todos os tipos dos parâmetros de todos os heads
		mut all_param_types := [][]TypeInfo{}
		mut return_types := []TypeInfo{}
		for clause in clauses {
			// Enter function context
			function_context := tc.context.enter_function(stmt.name)
			tc.context = &function_context

			// Bind parameters to context e coletar tipos
			mut param_types := []TypeInfo{}
			for param in clause.parameters {
				param_type := tc.check_pattern_and_get_type(param)
				param_types << param_type
			}
			all_param_types << param_types

			// Check function body
			tc.expression_checker.check_expression(clause.body)
			tc.errors << tc.expression_checker.errors
			tc.expression_checker.errors = []

			// Exit function context
			if mut parent := tc.context.exit_function() {
				tc.context = parent
			}

			return_types << tc.expression_checker.infer_expression_type_info(clause.body)
		}

		// Unificar tipos dos parâmetros por posição
		mut param_unions := []TypeInfo{}
		if all_param_types.len > 0 {
			for i in 0 .. all_param_types[0].len {
				mut types_at_pos := []TypeInfo{}
				for pt in all_param_types {
					if i < pt.len {
						types_at_pos << pt[i]
					}
				}
				// Unir tipos distintos
				mut unique_types := []string{}
				mut unique_typeinfos := []TypeInfo{}
				for t in types_at_pos {
					if t.str() !in unique_types {
						unique_types << t.str()
						unique_typeinfos << t
					}
				}
				if unique_typeinfos.len == 1 {
					param_unions << unique_typeinfos[0]
				} else {
					// Criar union
					union_str := unique_typeinfos.map(fn (t TypeInfo) string {
						if t.generic == 'string' { return 'binary()' } else { return t.generic + '()' }
					}).join(' | ')
					param_unions << TypeInfo{ generic: 'union', value: union_str }
				}
			}
		}
		tc.context.function_param_types['${stmt.name}/${arity}/params'] = param_unions

		// Unificar tipos de retorno (como antes)
		mut base_types := []string{}
		for t in return_types {
			if t.generic !in base_types {
				base_types << t.generic
			}
		}
		if base_types.len == 1 {
			tc.context.store_function_return_type(stmt.name, arity, return_types[0])
		} else {
			mut union_parts := []string{}
			for bt in base_types {
				if bt == 'string' {
					union_parts << 'binary()'
				} else {
					union_parts << bt + '()'
				}
			}
			union_str := union_parts.join(' | ')
			tc.context.store_function_return_type(stmt.name, arity, TypeInfo{ generic: 'union', value: union_str })
		}
	}
}

fn (mut tc TypeChecker) check_function_clause_with_name(clause ast.FunctionClause, fn_name string, arity int) {
	// Enter function context
	function_context := tc.context.enter_function(fn_name)
	tc.context = &function_context

	// Bind parameters to context and collect their types
	mut param_types := []TypeInfo{}
	for param in clause.parameters {
		param_type := tc.check_pattern_and_get_type(param)
		param_types << param_type
	}

	// Check function body
	tc.expression_checker.check_expression(clause.body)
	tc.errors << tc.expression_checker.errors
	tc.expression_checker.errors = []

	// Store parameter types for union generation
	tc.context.merge_function_param_types(fn_name, arity, param_types)

	// Exit function context
	if mut parent := tc.context.exit_function() {
		// Propagar function_param_types do filho para o pai
		for k, v in tc.context.function_param_types {
			parent.function_param_types[k] = v
		}
		tc.context = parent
	}

	// Validate return type if specified
	if return_type_expr := clause.return_type {
		expected_type := tc.type_expr_to_type_info(return_type_expr)
		actual_type := tc.expression_checker.infer_expression_type_info(clause.body)

		if !types_are_compatible(expected_type, actual_type) {
			tc.report_error(
				'Function return type mismatch: expected ${expected_type.str()}, got ${actual_type.str()}',
				'Make sure the function body returns the declared type',
				clause.body.position
			)
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
				type_info := TypeInfo{ generic: 'any', value: none }
				tc.context.bind(pattern.name, type_info, pattern.position)
				return type_info
			}
		}
		ast.AtomPattern {
			return TypeInfo{ generic: 'atom', value: pattern.value }
		}
		ast.LiteralPattern {
			return typeinfo_from_literal(pattern.value)
		}
		ast.TuplePattern {
			mut element_types := []string{}
			for element in pattern.elements {
				element_type := tc.check_pattern_and_get_type(element)
				element_types << element_type.str()
			}
			return TypeInfo{ generic: 'tuple', value: '{${element_types.join(', ')}}' }
		}
		ast.ListLiteralPattern {
			if pattern.elements.len == 0 {
				return TypeInfo{ generic: 'list', value: 'list(any)' }
			}
			first_type := tc.check_pattern_and_get_type(pattern.elements[0])
			return TypeInfo{ generic: 'list', value: 'list(${first_type.str()})' }
		}
		ast.ListConsPattern {
			head_type := tc.check_pattern_and_get_type(pattern.head)
			return TypeInfo{ generic: 'list', value: 'list(${head_type.str()})' }
		}
		ast.ListEmptyPattern {
			return TypeInfo{ generic: 'list', value: 'list(any)' }
		}
		ast.MapPattern {
			return TypeInfo{ generic: 'map', value: 'map(any=>any)' }
		}
		ast.RecordPattern {
			return TypeInfo{ generic: 'record', value: 'record' }
		}
		ast.BinaryPattern {
			return TypeInfo{ generic: 'bitstring', value: none }
		}
		ast.WildcardPattern {
			return TypeInfo{ generic: 'any', value: none }
		}
	}
	// Default return in case no match (should not happen)
	return TypeInfo{ generic: 'any', value: none }
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
	type_info := TypeInfo{ generic: 'var', value: pattern.name }
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
			// Use the existing typeinfo_from_str function for dynamic detection
			typeinfo_from_str(type_expr.name)
		}
		ast.TupleTypeExpr {
			mut element_types := []string{}
			for elem in type_expr.element_types {
				elem_info := tc.type_expr_to_type_info(elem)
				element_types << elem_info.str()
			}
			TypeInfo{ generic: 'tuple', value: '{${element_types.join(', ')}}' }
		}
		ast.ListTypeExpr {
			elem_info := tc.type_expr_to_type_info(type_expr.element_type)
			TypeInfo{ generic: 'list', value: 'list(${elem_info.str()})' }
		}
		ast.MapTypeExpr {
			key_info := tc.type_expr_to_type_info(type_expr.key_type)
			value_info := tc.type_expr_to_type_info(type_expr.value_type)
			TypeInfo{ generic: 'map', value: 'map(${key_info.str()}=>${value_info.str()})' }
		}
		else {
			TypeInfo{ generic: 'any', value: none }
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