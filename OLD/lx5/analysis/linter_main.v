module analysis

import ast
import errors

pub struct Linter {
pub mut:
	rules    []LintRule
	reporter LintReporter
}

pub fn new_linter() Linter {
	return Linter{
		rules:    create_default_rules()
		reporter: new_lint_reporter()
	}
}

pub fn (mut l Linter) lint_module(module_stmt ast.ModuleStmt) LintResult {
	mut result := LintResult{
		errors:   []
		warnings: []
	}

	for rule in l.rules {
		rule_errors := rule.check(module_stmt)
		result.errors << rule_errors.errors
		result.warnings << rule_errors.warnings
	}

	return result
}

pub struct LintResult {
pub mut:
	errors   []errors.CompilationError
	warnings []errors.CompilationError
}

pub interface LintRule {
	check(module_stmt ast.ModuleStmt) LintResult
}

pub struct LintReporter {
pub mut:
	errors   []errors.CompilationError
	warnings []errors.CompilationError
}

pub fn new_lint_reporter() LintReporter {
	return LintReporter{
		errors:   []
		warnings: []
	}
}

pub fn (mut lr LintReporter) report_error(message string, position ast.Position, suggestion string) {
	error := errors.new_compilation_error(errors.SyntaxError{
		message:  message
		expected: suggestion
		found:    ''
	}, position, message)
	lr.errors << error
}

pub fn (mut lr LintReporter) report_warning(message string, position ast.Position, suggestion string) {
	warning := errors.new_compilation_error(errors.SyntaxError{
		message:  message
		expected: suggestion
		found:    ''
	}, position, message)
	lr.warnings << warning
}

// Default linting rules
fn create_default_rules() []LintRule {
	return [
		ArityOrderingRule{},
		UnusedVariableRule{},
		FunctionNamingRule{},
		RecordFieldNamingRule{},
	]
}

// Arity ordering rule - functions with same name should be ordered by arity
pub struct ArityOrderingRule {
}

pub fn (ar ArityOrderingRule) check(module_stmt ast.ModuleStmt) LintResult {
	mut result := LintResult{
		errors:   []
		warnings: []
	}

	// Group functions by name
	mut function_groups := map[string][]ast.FunctionStmt{}
	for stmt in module_stmt.statements {
		if stmt is ast.FunctionStmt {
			func_stmt := stmt as ast.FunctionStmt
			if func_stmt.name in function_groups {
				function_groups[func_stmt.name] << func_stmt
			} else {
				function_groups[func_stmt.name] = [func_stmt]
			}
		}
	}

	// Check arity ordering for each function group
	for name, functions in function_groups {
		if functions.len > 1 {
			// Sort by arity
			mut sorted_functions := functions.clone()
			sorted_functions.sort_with_compare(fn (a &ast.FunctionStmt, b &ast.FunctionStmt) int {
				arity_a := if a.clauses.len > 0 { a.clauses[0].parameters.len } else { 0 }
				arity_b := if b.clauses.len > 0 { b.clauses[0].parameters.len } else { 0 }
				return arity_a - arity_b
			})

			// Check if original order matches sorted order
			for i, func_stmt in functions {
				if i < sorted_functions.len {
					original_arity := if func_stmt.clauses.len > 0 {
						func_stmt.clauses[0].parameters.len
					} else {
						0
					}
					sorted_arity := if sorted_functions[i].clauses.len > 0 {
						sorted_functions[i].clauses[0].parameters.len
					} else {
						0
					}
					if original_arity != sorted_arity {
						result.warnings << errors.new_compilation_error(errors.SyntaxError{
							message:  'Function ${name} clauses should be ordered by arity'
							expected: 'Order function clauses by increasing arity'
							found:    ''
						}, func_stmt.position, 'Function ${name} clauses should be ordered by arity')
					}
				}
			}
		}
	}

	return result
}

// Unused variable rule
pub struct UnusedVariableRule {
}

pub fn (uvr UnusedVariableRule) check(module_stmt ast.ModuleStmt) LintResult {
	mut result := LintResult{
		errors:   []
		warnings: []
	}

	// This rule would need access to the variable scope information
	// For now, we'll implement a basic version that checks for obvious unused variables
	for stmt in module_stmt.statements {
		if stmt is ast.FunctionStmt {
			func_stmt := stmt as ast.FunctionStmt
			for clause in func_stmt.clauses {
				for param in clause.parameters {
					if param is ast.VarPattern {
						var_pattern := param as ast.VarPattern
						if var_pattern.name.starts_with('_') {
							// Underscore-prefixed variables are intentionally unused
							continue
						}
						// Check if the variable is used in the function body
						if !uvr.is_variable_used(var_pattern.name, clause.body) {
							result.warnings << errors.new_compilation_error(errors.SyntaxError{
								message:  'Unused variable: ${var_pattern.name}'
								expected: 'Use the variable or prefix with underscore to indicate intentional non-use'
								found:    ''
							}, var_pattern.position, 'Unused variable: ${var_pattern.name}')
						}
					}
				}
			}
		}
	}

	return result
}

fn (uvr UnusedVariableRule) is_variable_used(name string, expr ast.Expr) bool {
	match expr {
		ast.VariableExpr {
			return expr.name == name
		}
		ast.BinaryExpr {
			return uvr.is_variable_used(name, expr.left) || uvr.is_variable_used(name, expr.right)
		}
		ast.CallExpr {
			mut used := uvr.is_variable_used(name, expr.function)
			for arg in expr.arguments {
				used = used || uvr.is_variable_used(name, arg)
			}
			return used
		}
		ast.IfExpr {
			if uvr.is_variable_used(name, expr.condition) {
				return true
			}
			if uvr.is_variable_used(name, expr.then_body) {
				return true
			}
			if expr.else_body.body.len > 0 {
				return uvr.is_variable_used(name, expr.else_body)
			}
			return false
		}
		ast.CaseExpr {
			if uvr.is_variable_used(name, expr.value) {
				return true
			}
			for case_expr in expr.cases {
				if uvr.is_variable_used(name, case_expr.body) {
					return true
				}
			}
			return false
		}
		ast.MatchExpr {
			if uvr.is_variable_used(name, expr.value) {
				return true
			}
			for case_expr in expr.cases {
				if uvr.is_variable_used(name, case_expr.body) {
					return true
				}
			}
			return false
		}
		ast.ReceiveExpr {
			for case_expr in expr.cases {
				if uvr.is_variable_used(name, case_expr.body) {
					return true
				}
			}
			if timeout := expr.timeout {
				return uvr.is_variable_used(name, timeout.body)
			}
			return false
		}
		ast.FunExpr {
			for param in expr.parameters {
				if param is ast.VarPattern {
					var_pattern := param as ast.VarPattern
					if var_pattern.name == name {
						return false // Variable is shadowed
					}
				}
			}
			return uvr.is_variable_used(name, expr.body)
		}
		ast.ListConsExpr {
			return uvr.is_variable_used(name, expr.head) || uvr.is_variable_used(name, expr.tail)
		}
		ast.ListLiteralExpr {
			for element in expr.elements {
				if uvr.is_variable_used(name, element) {
					return true
				}
			}
			return false
		}
		ast.TupleExpr {
			for element in expr.elements {
				if uvr.is_variable_used(name, element) {
					return true
				}
			}
			return false
		}
		ast.MapLiteralExpr {
			for entry in expr.entries {
				if uvr.is_variable_used(name, entry.key) || uvr.is_variable_used(name, entry.value) {
					return true
				}
			}
			return false
		}
		ast.MapAccessExpr {
			return uvr.is_variable_used(name, expr.map_expr) || uvr.is_variable_used(name, expr.key)
		}
		ast.MapUpdateExpr {
			if uvr.is_variable_used(name, expr.base_map) {
				return true
			}
			for entry in expr.entries {
				if uvr.is_variable_used(name, entry.key) || uvr.is_variable_used(name, entry.value) {
					return true
				}
			}
			return false
		}
		ast.RecordLiteralExpr {
			for field in expr.fields {
				if uvr.is_variable_used(name, field.value) {
					return true
				}
			}
			return false
		}
		ast.RecordAccessExpr {
			return uvr.is_variable_used(name, expr.record)
		}
		ast.RecordUpdateExpr {
			if uvr.is_variable_used(name, expr.base_record) {
				return true
			}
			for field in expr.fields {
				if uvr.is_variable_used(name, field.value) {
					return true
				}
			}
			return false
		}
		ast.LiteralExpr {
			return false
		}
		ast.BlockExpr {
			for stmt in expr.body {
				if stmt is ast.ExprStmt {
					expr_stmt := stmt as ast.ExprStmt
					if uvr.is_variable_used(name, expr_stmt.expr) {
						return true
					}
				}
			}
			return false
		}
		ast.WithExpr {
			for binding in expr.bindings {
				if uvr.is_variable_used(name, binding.value) {
					return true
				}
			}
			if uvr.is_variable_used(name, expr.body) {
				return true
			}
			if expr.else_body.body.len > 0 {
				return uvr.is_variable_used(name, expr.else_body)
			}
			return false
		}
		ast.ForExpr {
			if uvr.is_variable_used(name, expr.collection) {
				return true
			}
			if expr.guard != ast.Expr{} {
				if uvr.is_variable_used(name, expr.guard) {
					return true
				}
			}
			return uvr.is_variable_used(name, expr.body)
		}
		ast.SendExpr {
			return uvr.is_variable_used(name, expr.pid) || uvr.is_variable_used(name, expr.message)
		}
		ast.GuardExpr {
			return uvr.is_variable_used(name, expr.condition)
		}
		ast.UnaryExpr {
			return uvr.is_variable_used(name, expr.operand)
		}
		ast.AssignExpr {
			return uvr.is_variable_used(name, expr.value)
		}
		ast.ListEmptyExpr {
			return false
		}
		ast.SimpleMatchExpr {
			return false
		}
		ast.MatchRescueExpr {
			return false
		}
		ast.BinaryPatternExpr {
			return false
		}
	}
}

// Function naming rule
pub struct FunctionNamingRule {
}

pub fn (fnr FunctionNamingRule) check(module_stmt ast.ModuleStmt) LintResult {
	mut result := LintResult{
		errors:   []
		warnings: []
	}

	for stmt in module_stmt.statements {
		if stmt is ast.FunctionStmt {
			func_stmt := stmt as ast.FunctionStmt
			if !fnr.is_valid_function_name(func_stmt.name) {
				result.warnings << errors.new_compilation_error(errors.SyntaxError{
					message:  'Function name should follow snake_case convention: ${func_stmt.name}'
					expected: 'Use snake_case for function names'
					found:    func_stmt.name
				}, func_stmt.position, 'Function name should follow snake_case convention: ${func_stmt.name}')
			}
		}
	}

	return result
}

fn (fnr FunctionNamingRule) is_valid_function_name(name string) bool {
	// Check if name follows snake_case convention
	if name.len == 0 {
		return false
	}

	// Should start with lowercase letter
	if name[0].is_capital() {
		return false
	}

	// Should not contain consecutive underscores
	if name.contains('__') {
		return false
	}

	// Should not end with underscore
	if name.ends_with('_') {
		return false
	}

	return true
}

// Record field naming rule
pub struct RecordFieldNamingRule {
}

pub fn (rfnr RecordFieldNamingRule) check(module_stmt ast.ModuleStmt) LintResult {
	mut result := LintResult{
		errors:   []
		warnings: []
	}

	for stmt in module_stmt.statements {
		if stmt is ast.RecordDefStmt {
			record_stmt := stmt as ast.RecordDefStmt
			for field in record_stmt.fields {
				if !rfnr.is_valid_field_name(field.name) {
					result.warnings << errors.new_compilation_error(errors.SyntaxError{
						message:  'Record field name should follow snake_case convention: ${field.name}'
						expected: 'Use snake_case for record field names'
						found:    field.name
					}, field.position, 'Record field name should follow snake_case convention: ${field.name}')
				}
			}
		}
	}

	return result
}

fn (rfnr RecordFieldNamingRule) is_valid_field_name(name string) bool {
	// Same validation as function names
	if name.len == 0 {
		return false
	}

	if name[0].is_capital() {
		return false
	}

	if name.contains('__') {
		return false
	}

	if name.ends_with('_') {
		return false
	}

	return true
}
