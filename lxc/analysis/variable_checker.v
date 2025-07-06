module analysis

import ast
import errors

// VariableBinding represents a variable binding in a scope
pub struct VariableBinding {
pub:
	name     string
	position ast.Position
	defined  bool
pub mut:
	used bool
}

// FunctionBinding represents a function binding in a scope
pub struct FunctionBinding {
pub:
	name     string
	position ast.Position
	arity    int
}

// VariableScope represents a lexical scope for variable checking
pub struct VariableScope {
pub mut:
	variables map[string]VariableBinding
	functions map[string]FunctionBinding
	level     int
}

// new_variable_scope creates a new variable scope
pub fn new_variable_scope() VariableScope {
	return VariableScope{
		variables: map[string]VariableBinding{}
		functions: map[string]FunctionBinding{}
		level:     0
	}
}

// VariableChecker provides variable scope checking functionality using a stack-based approach
pub struct VariableChecker {
pub mut:
	scope_stack []VariableScope
	errors      []errors.CompilationError
}

// new_variable_checker creates a new variable checker
pub fn new_variable_checker() VariableChecker {
	return VariableChecker{
		scope_stack: [new_variable_scope()]
		errors:      []
	}
}

// enter_scope enters a new nested scope
pub fn (mut vc VariableChecker) enter_scope() {
	vc.scope_stack << VariableScope{
		variables: map[string]VariableBinding{}
		functions: map[string]FunctionBinding{}
		level:     vc.scope_stack.len
	}
}

// exit_scope exits the current scope and returns to parent
pub fn (mut vc VariableChecker) exit_scope() {
	if vc.scope_stack.len > 1 {
		vc.scope_stack.delete_last()
	}
}

// bind_variable adds a variable to the current scope
pub fn (mut vc VariableChecker) bind_variable(name string, position ast.Position) {
	if vc.scope_stack.len > 0 {
		vc.scope_stack[vc.scope_stack.len - 1].variables[name] = VariableBinding{
			name:     name
			position: position
			defined:  true
			used:     false
		}
	}
}

// bind_function adds a function to the current scope
pub fn (mut vc VariableChecker) bind_function(name string, position ast.Position, arity int) {
	if vc.scope_stack.len > 0 {
		vc.scope_stack[vc.scope_stack.len - 1].functions[name] = FunctionBinding{
			name:     name
			position: position
			arity:    arity
		}
	}
}

// has_binding_local checks if variable exists in current scope only
pub fn (vc &VariableChecker) has_binding_local(name string) bool {
	if vc.scope_stack.len == 0 {
		return false
	}
	return name in vc.scope_stack[vc.scope_stack.len - 1].variables
}

// has_binding_recursive checks if variable exists in current or parent scopes
pub fn (vc &VariableChecker) has_binding_recursive(name string) bool {
	for i := vc.scope_stack.len - 1; i >= 0; i-- {
		if name in vc.scope_stack[i].variables {
			return true
		}
	}
	return false
}

// has_function_recursive checks if function exists in current or parent scopes
pub fn (vc &VariableChecker) has_function_recursive(name string) bool {
	for i := vc.scope_stack.len - 1; i >= 0; i-- {
		if name in vc.scope_stack[i].functions {
			return true
		}
	}
	return false
}

// has_binding_in_parent checks if variable exists in parent scopes only
pub fn (vc &VariableChecker) has_binding_in_parent(name string) bool {
	for i := vc.scope_stack.len - 2; i >= 0; i-- {
		if name in vc.scope_stack[i].variables {
			return true
		}
	}
	return false
}

// report_error adds a variable-related error
pub fn (mut vc VariableChecker) report_error(message string, suggestion string, position ast.Position) {
	error := errors.new_compilation_error(errors.UnboundVariableError{
		variable:   ''
		similar:    []
		suggestion: suggestion
	}, position, message)
	vc.errors << error
}

// check_assignment_expression validates assignment expressions
pub fn (mut vc VariableChecker) check_assignment_expression(expr ast.AssignExpr) {
	// Check for rebind in current scope
	if vc.has_binding_local(expr.name) {
		vc.report_error("Variable '${expr.name}' cannot be reassigned", 'Variables in LX are immutable and cannot be reassigned. Use a different variable name or restructure your code.',
			expr.position)
		return
	}

	// Check for shadowing from parent scopes
	if vc.has_binding_in_parent(expr.name) {
		vc.report_error("Variable '${expr.name}' shadows variable from outer scope", 'Shadowing is not allowed in LX. Use a different variable name: ${expr.name}_inner',
			expr.position)
		return
	}

	// Check the value expression
	vc.check_expression(expr.value)

	// Bind the variable to current scope
	vc.bind_variable(expr.name, expr.position)
}

// check_variable_expression validates variable usage
pub fn (mut vc VariableChecker) check_variable_expression(expr ast.VariableExpr) {
	if !vc.has_binding_recursive(expr.name) && !vc.has_function_recursive(expr.name) {
		vc.report_error("Variable '${expr.name}' is not defined", 'Variables must be defined before use. Check spelling or add an assignment: ${expr.name} = some_value',
			expr.position)
	} else {
		// Mark variable as used (only if it's actually a variable, not a function)
		if vc.has_binding_recursive(expr.name) {
			vc.mark_variable_used(expr.name)
		}
	}
}

// check_expression validates expressions for variable usage
pub fn (mut vc VariableChecker) check_expression(expr ast.Expr) {
	match expr {
		ast.VariableExpr {
			vc.check_variable_expression(expr)
		}
		ast.AssignExpr {
			vc.check_assignment_expression(expr)
		}
		ast.BinaryExpr {
			vc.check_expression(expr.left)
			vc.check_expression(expr.right)
		}
		ast.CallExpr {
			if expr.external {
				for arg in expr.arguments {
					vc.check_expression(arg)
				}
			} else if expr.function is ast.VariableExpr {
				func_var := expr.function as ast.VariableExpr
				func_name := func_var.name
				arity := expr.arguments.len
				if !vc.has_function_recursive(func_name) {
					mut params := []string{}
					for i, arg in expr.arguments {
						param_type := vc.infer_argument_type(arg)
						params << 'arg${i + 1} :: ${param_type}'
					}
					params_str := params.join(', ')
					func_suggestion := 'def ${func_name}(${params_str}) do\n  ...\nend'
					vc.errors << errors.new_compilation_error(errors.UnboundFunctionError{
						function:   func_name
						arity:      arity
						suggestion: func_suggestion
					}, func_var.position, "Function '${func_name}/${arity}' is not defined")
				}
				for arg in expr.arguments {
					vc.check_expression(arg)
				}
			} else {
				vc.check_expression(expr.function)
				for arg in expr.arguments {
					vc.check_expression(arg)
				}
			}
		}
		ast.MatchExpr {
			vc.check_expression(expr.value)
			for case_ in expr.cases {
				vc.check_pattern(case_.pattern)
				vc.check_expression(case_.guard)
				vc.check_block_expression(case_.body)
			}
		}
		ast.ListConsExpr {
			vc.check_expression(expr.head)
			vc.check_expression(expr.tail)
		}
		ast.ListLiteralExpr {
			for element in expr.elements {
				vc.check_expression(element)
			}
		}
		ast.TupleExpr {
			for element in expr.elements {
				vc.check_expression(element)
			}
		}
		ast.MapLiteralExpr {
			vc.check_map_literal_expression(expr)
		}
		ast.MapAccessExpr {
			vc.check_map_access_expression(expr)
		}
		ast.MapUpdateExpr {
			vc.check_map_update_expression(expr)
		}
		ast.RecordLiteralExpr {
			for field in expr.fields {
				vc.check_expression(field.value)
			}
		}
		ast.RecordAccessExpr {
			vc.check_expression(expr.record)
		}
		ast.RecordUpdateExpr {
			vc.check_expression(expr.base_record)
			for field in expr.fields {
				vc.check_expression(field.value)
			}
		}
		ast.FunExpr {
			vc.enter_scope()
			for param in expr.parameters {
				vc.check_pattern(param)
			}
			vc.check_block_expression(expr.body)
			vc.exit_scope()
		}
		ast.SendExpr {
			vc.check_expression(expr.pid)
			vc.check_expression(expr.message)
		}
		ast.ReceiveExpr {
			for case_ in expr.cases {
				vc.check_pattern(case_.pattern)
				vc.check_expression(case_.guard)
				vc.check_block_expression(case_.body)
			}
			// Check timeout clause if present
			if timeout_clause := expr.timeout {
				vc.check_expression(timeout_clause.timeout)
				vc.check_block_expression(timeout_clause.body)
			}
		}
		ast.GuardExpr {
			vc.check_expression(expr.condition)
		}
		ast.UnaryExpr {
			vc.check_expression(expr.operand)
		}
		ast.IfExpr {
			vc.check_expression(expr.condition)
			vc.check_block_expression(expr.then_body)
			vc.check_block_expression(expr.else_body)
		}
		ast.CaseExpr {
			vc.check_expression(expr.value)
			for case_ in expr.cases {
				vc.check_pattern(case_.pattern)
				vc.check_expression(case_.guard)
				vc.check_block_expression(case_.body)
			}
		}
		ast.WithExpr {
			for binding in expr.bindings {
				vc.check_pattern(binding.pattern)
				vc.check_expression(binding.value)
				vc.check_expression(binding.guard)
			}
			vc.check_block_expression(expr.body)
			vc.check_block_expression(expr.else_body)
		}
		ast.ForExpr {
			vc.check_pattern(expr.pattern)
			vc.check_expression(expr.collection)
			vc.check_expression(expr.guard)
			vc.check_block_expression(expr.body)
		}
		ast.LiteralExpr, ast.ListEmptyExpr {
			// No variables to check in literals
		}
		ast.SimpleMatchExpr {
			vc.check_expression(expr.value)
			vc.check_pattern(expr.pattern)
			vc.check_expression(expr.guard)
		}
		ast.MatchRescueExpr {
			vc.check_match_rescue_expression(expr)
		}
		ast.BlockExpr {
			vc.check_block_expression(expr)
		}
	}
}

// check_pattern validates patterns for variable binding
pub fn (mut vc VariableChecker) check_pattern(pattern ast.Pattern) {
	match pattern {
		ast.VarPattern {
			// In patterns, variables are bound (not used)
			// Note: VarPattern doesn't have position, so we'll use a default position
			position := ast.new_position(0, 0, '')
			vc.bind_variable(pattern.name, position)
		}
		ast.WildcardPattern, ast.LiteralPattern, ast.AtomPattern, ast.ListEmptyPattern {
			// These patterns don't bind variables
		}
		ast.ListConsPattern {
			vc.check_pattern(pattern.head)
			vc.check_pattern(pattern.tail)
		}
		ast.ListLiteralPattern {
			for element in pattern.elements {
				vc.check_pattern(element)
			}
		}
		ast.TuplePattern {
			for element in pattern.elements {
				vc.check_pattern(element)
			}
		}
		ast.MapPattern {
			for entry in pattern.entries {
				vc.check_pattern(entry.key)
				vc.check_pattern(entry.value)
			}
		}
		ast.RecordPattern {
			for field in pattern.fields {
				vc.check_pattern(field.pattern)
			}
		}
		ast.BinaryPattern {
			// Binary patterns don't bind variables
		}
	}
}

// check_statement validates statements for variable usage
pub fn (mut vc VariableChecker) check_statement(stmt ast.Stmt) {
	match stmt {
		ast.ExprStmt {
			vc.check_expression(stmt.expr)
		}
		ast.ModuleStmt {
			for stmt_ in stmt.statements {
				vc.check_statement(stmt_)
			}
		}
		ast.FunctionStmt {
			// Register the function in the current scope
			vc.bind_function(stmt.name, stmt.position, stmt.clauses[0].parameters.len)

			for clause in stmt.clauses {
				vc.enter_scope()
				for param in clause.parameters {
					vc.check_pattern(param)
				}
				vc.check_expression(clause.guard)
				vc.check_block_expression(clause.body)
				vc.check_unused_variables()
				vc.exit_scope()
			}
		}
		ast.RecordDefStmt, ast.TypeDefStmt, ast.TypeAliasStmt {
			// These don't involve variable scoping
		}
	}
}

// Resultado da checagem de variáveis
pub struct VariableCheckResult {
pub:
	success bool
	errors  []errors.CompilationError
}

// Check a whole module (top-level)
pub fn (mut vc VariableChecker) check_module(mod ast.ModuleStmt) VariableCheckResult {
	for stmt in mod.statements {
		vc.check_statement(stmt)
	}

	// Check for unused variables after all statements have been processed
	vc.check_unused_variables()

	return VariableCheckResult{
		success: vc.errors.len == 0
		errors:  vc.errors.clone()
	}
}

// has_errors checks if the variable checker has encountered any errors
pub fn (vc VariableChecker) has_errors() bool {
	return vc.errors.len > 0
}

// get_errors returns all errors encountered by the variable checker
pub fn (vc VariableChecker) get_errors() []errors.CompilationError {
	return vc.errors.clone()
}

// mark_variable_used marks a variable as used in the current scope or parent scopes
pub fn (mut vc VariableChecker) mark_variable_used(name string) {
	// Find the variable in the scope stack and mark it as used
	for i := vc.scope_stack.len - 1; i >= 0; i-- {
		if name in vc.scope_stack[i].variables {
			mut binding := vc.scope_stack[i].variables[name]
			binding.used = true
			vc.scope_stack[i].variables[name] = binding
			break
		}
	}
}

// check_unused_variables checks for unused variables in all scopes
pub fn (mut vc VariableChecker) check_unused_variables() {
	for scope in vc.scope_stack {
		for name, binding in scope.variables {
			// Skip variables that start with underscore (wildcard pattern)
			if name.starts_with('_') {
				continue
			}

			if binding.defined && !binding.used {
				error_msg := "Variable '${name}' is defined but never used"
				suggestion := 'Remove the variable assignment or use the variable in an expression'

				unused_error := errors.UnusedVariableError{
					variable:   name
					suggestion: suggestion
				}

				compilation_error := errors.new_compilation_error_with_severity(unused_error,
					binding.position, error_msg, .warning)

				vc.errors << compilation_error
			}
		}
	}
}

// infer_argument_type infers the type of an argument expression
fn (vc &VariableChecker) infer_argument_type(arg ast.Expr) string {
	match arg {
		ast.LiteralExpr {
			match arg.value {
				ast.IntegerLiteral { return 'integer' }
				ast.FloatLiteral { return 'float' }
				ast.StringLiteral { return 'string' }
				ast.BooleanLiteral { return 'boolean' }
				ast.AtomLiteral { return 'atom' }
				ast.NilLiteral { return 'nil' }
			}
		}
		ast.VariableExpr {
			return 'any'
		}
		ast.BinaryExpr {
			match arg.op {
				.add, .subtract, .multiply, .divide, .modulo, .power {
					return 'integer'
				}
				.equal, .not_equal, .less_than, .less_equal, .greater_than, .greater_equal {
					return 'boolean'
				}
				.and, .or {
					return 'boolean'
				}
				else {
					return 'any'
				}
			}
		}
		ast.CallExpr {
			return 'any'
		}
		else {
			return 'any'
		}
	}
}

// check_block_expression validates block expressions for variable usage
pub fn (mut vc VariableChecker) check_block_expression(expr ast.BlockExpr) {
	vc.enter_scope()
	for stmt in expr.body {
		vc.check_statement(stmt)
	}
	vc.exit_scope()
}

// check_match_rescue_expression validates match rescue expressions for variable usage
pub fn (mut vc VariableChecker) check_match_rescue_expression(expr ast.MatchRescueExpr) {
	vc.check_expression(expr.value)
	vc.check_pattern(expr.pattern)
	vc.enter_scope()
	// Bind the rescue variable
	vc.bind_variable(expr.rescue_var, expr.position)
	vc.check_block_expression(expr.rescue_body)
	vc.exit_scope()
}

// check_map_literal_expression checks map literal expressions
fn (mut vc VariableChecker) check_map_literal_expression(expr ast.MapLiteralExpr) {
	for entry in expr.entries {
		vc.check_expression(entry.key)
		vc.check_expression(entry.value)
	}
}

// check_map_access_expression checks map access expressions
fn (mut vc VariableChecker) check_map_access_expression(expr ast.MapAccessExpr) {
	vc.check_expression(expr.map_expr)
	vc.check_expression(expr.key)
}

// check_map_update_expression checks map update expressions
fn (mut vc VariableChecker) check_map_update_expression(expr ast.MapUpdateExpr) {
	vc.check_expression(expr.base_map)
	for entry in expr.entries {
		vc.check_expression(entry.key)
		vc.check_expression(entry.value)
	}
}
