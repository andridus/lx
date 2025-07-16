module analysis

import ast
import errors
import time

pub struct Analyzer {
pub mut:
	hm_inferencer       HMInferencer
	variable_checker    VariableChecker
	directive_manager   Manager
	linter              Linter
	stop_on_first_error bool
	enable_warnings     bool
}

pub struct AnalysisResult {
pub mut:
	success       bool
	module_stmt   ast.ModuleStmt
	type_context  &TypeContext
	errors        []errors.CompilationError
	warnings      []errors.CompilationError
	phase_results map[AnalysisPhase]PhaseResult
}

pub enum AnalysisPhase {
	variable_check
	directive_processing
	linting
	type_checking
}

pub struct PhaseResult {
pub:
	success       bool
	duration      f64 // in milliseconds
	error_count   int
	warning_count int
}

pub fn new_analyzer() Analyzer {
	return Analyzer{
		hm_inferencer:       new_hm_inferencer()
		variable_checker:    new_variable_checker()
		directive_manager:   new_manager()
		linter:              new_linter()
		stop_on_first_error: false
		enable_warnings:     true
	}
}

// new_analyzer_with_debug creates an analyzer with debug mode enabled
pub fn new_analyzer_with_debug() Analyzer {
	return Analyzer{
		hm_inferencer:       new_hm_inferencer_with_debug()
		variable_checker:    new_variable_checker()
		directive_manager:   new_manager()
		linter:              new_linter()
		stop_on_first_error: false
		enable_warnings:     true
	}
}

// enable_debug enables debug mode for the HM inferencer
pub fn (mut a Analyzer) enable_debug() {
	a.hm_inferencer.enable_debug()
}

// disable_debug disables debug mode for the HM inferencer
pub fn (mut a Analyzer) disable_debug() {
	a.hm_inferencer.disable_debug()
}

// print_debug prints debug information from the HM inferencer
pub fn (a Analyzer) print_debug() {
	println('=== ANALYZER DEBUG ===')
	println('Stop on first error: ${a.stop_on_first_error}')
	println('Enable warnings: ${a.enable_warnings}')

	println('\nHM Inferencer:')
	a.hm_inferencer.print_debug()
	println('======================')
}

// get_type_table returns the type table from the HM system
pub fn (a Analyzer) get_type_table() &TypeTable {
	return a.hm_inferencer.get_type_table()
}

pub fn (mut a Analyzer) analyze_module(module_stmt ast.ModuleStmt) AnalysisResult {
	mut ctx := new_type_context()
	mut result := AnalysisResult{
		success:       true
		module_stmt:   module_stmt
		type_context:  &ctx
		errors:        []
		warnings:      []
		phase_results: map[AnalysisPhase]PhaseResult{}
	}

	// Phase 1: Variable checking
	mut phase_start := time.now()
	variable_errors := a.variable_checker.check_module(module_stmt)
	result.errors << variable_errors
	result.phase_results[.variable_check] = PhaseResult{
		success:       variable_errors.len == 0
		duration:      f64(time.now().unix_milli() - phase_start.unix_milli())
		error_count:   variable_errors.len
		warning_count: 0
	}

	if a.stop_on_first_error && variable_errors.len > 0 {
		result.success = false
		return result
	}

	// Phase 2: Directive processing
	phase_start = time.now()
	directive_errors := a.directive_manager.process_directives(module_stmt)
	result.errors << directive_errors
	result.phase_results[.directive_processing] = PhaseResult{
		success:       directive_errors.len == 0
		duration:      f64(time.now().unix_milli() - phase_start.unix_milli())
		error_count:   directive_errors.len
		warning_count: 0
	}

	if a.stop_on_first_error && directive_errors.len > 0 {
		result.success = false
		return result
	}

	// Phase 3: Linting
	phase_start = time.now()
	lint_result := a.linter.lint_module(module_stmt)
	result.errors << lint_result.errors
	result.warnings << lint_result.warnings
	result.phase_results[.linting] = PhaseResult{
		success:       lint_result.errors.len == 0
		duration:      f64(time.now().unix_milli() - phase_start.unix_milli())
		error_count:   lint_result.errors.len
		warning_count: lint_result.warnings.len
	}

	if a.stop_on_first_error && lint_result.errors.len > 0 {
		result.success = false
		return result
	}

	// Phase 4: Type checking with HM
	phase_start = time.now()

	// Use HM system for type checking
	type_result := a.check_module_with_hm(module_stmt)

	result.errors << type_result.errors
	result.warnings << type_result.warnings
	result.type_context = type_result.context
	result.phase_results[.type_checking] = PhaseResult{
		success:       type_result.errors.len == 0
		duration:      f64(time.now().unix_milli() - phase_start.unix_milli())
		error_count:   type_result.errors.len
		warning_count: type_result.warnings.len
	}

	result.success = result.errors.len == 0
	return result
}

// check_module_with_hm performs type checking using the HM system
fn (mut a Analyzer) check_module_with_hm(module_stmt ast.ModuleStmt) TypeCheckResult {
	mut error_list := []errors.CompilationError{}
	mut warning_list := []errors.CompilationError{}

	// Process each statement in the module
	for stmt in module_stmt.statements {
		match stmt {
			ast.FunctionStmt {
				// Process the entire function (all clauses)
				a.process_function_hm(stmt) or {
					// Collect errors from HM inferencer
					hm_errors := a.hm_inferencer.get_errors()
					error_list << hm_errors
					a.hm_inferencer.clear_errors()
					// Don't continue if there are type errors
					if hm_errors.len > 0 {
						break
					}
				}
			}
			ast.RecordDefStmt {
				// Process record definitions to register them in the type environment
				a.process_record_definition_hm(stmt)
			}
			ast.WorkerStmt {
				// Process worker statements recursively
				a.process_worker_hm(stmt) or {
					hm_errors := a.hm_inferencer.get_errors()
					error_list << hm_errors
					a.hm_inferencer.clear_errors()
					if hm_errors.len > 0 {
						break
					}
				}
			}
			ast.SupervisorStmt {
				// Process supervisor statements recursively
				a.process_supervisor_hm(stmt) or {
					hm_errors := a.hm_inferencer.get_errors()
					error_list << hm_errors
					a.hm_inferencer.clear_errors()
					if hm_errors.len > 0 {
						break
					}
				}
			}
			else {
				// For other statements, we can extend HM support later
			}
		}
	}

	// Also collect any remaining errors from HM inferencer
	remaining_errors := a.hm_inferencer.get_errors()
	error_list << remaining_errors
	a.hm_inferencer.clear_errors()

	// Create a minimal type context for compatibility
	type_context := new_type_context()

	return TypeCheckResult{
		context:  &type_context
		errors:   error_list
		warnings: warning_list
	}
}

// process_function_hm processes an entire function with all its clauses
fn (mut a Analyzer) process_function_hm(func_stmt ast.FunctionStmt) ! {
	mut hm_inferencer := &a.hm_inferencer

	// Reset HM inferencer state for this function
	hm_inferencer.reset_for_function()

	if hm_inferencer.type_table.debug_mode {
		println('[HM_INFERENCER] Processing function "${func_stmt.name}" with ${func_stmt.clauses.len} clauses')
	}

	// Group clauses by arity for separate type processing
	mut clauses_by_arity := map[int][]ast.FunctionClause{}
	for clause in func_stmt.clauses {
		arity := clause.parameters.len
		if arity !in clauses_by_arity {
			clauses_by_arity[arity] = []ast.FunctionClause{}
		}
		clauses_by_arity[arity] << clause
	}

	// Process each arity group separately
	for arity, clauses in clauses_by_arity {
		mut clause_return_types := []TypeInfo{}

		if hm_inferencer.type_table.debug_mode {
			println('[HM_INFERENCER] Processing arity ${arity} with ${clauses.len} clauses')
		}

		// Process each clause in this arity group
		for i, clause in clauses {
			if hm_inferencer.type_table.debug_mode {
				println('[HM_INFERENCER] Processing clause ${i + 1}/${clauses.len} for arity ${arity}')
			}

			// Create a new environment scope for this clause
			clause_env := hm_inferencer.type_env.extend()
			original_env := hm_inferencer.type_env
			hm_inferencer.type_env = clause_env

			// Process parameters first to bind their types
			a.process_function_parameters_hm(clause)!

			// Infer types for expressions in the function body
			body_type := hm_inferencer.infer_expression(clause.body)!

			if hm_inferencer.type_table.debug_mode {
				println('[HM_INFERENCER] Clause ${i + 1} body type: ${body_type}')
			}

			// Check if clause has return type annotation
			if return_type_expr := clause.return_type {
				expected_return_type := a.convert_type_expr_to_type_info(return_type_expr)

				if hm_inferencer.type_table.debug_mode {
					println('[HM_INFERENCER] Clause ${i + 1} expected return type: ${expected_return_type}')
				}

				// Add constraint that body type must match expected return type
				hm_inferencer.add_constraint(body_type, expected_return_type)
				clause_return_types << expected_return_type
			} else {
				// No annotation - use inferred body type
				clause_return_types << body_type
			}

			// Restore original environment
			hm_inferencer.type_env = original_env
		}

		// Determine the return type for this arity
		arity_return_type := if clause_return_types.len == 1 {
			clause_return_types[0]
		} else {
			// Multiple clauses for this arity - create union type
			typeinfo_union(clause_return_types)
		}

		if hm_inferencer.type_table.debug_mode {
			println('[HM_INFERENCER] Function "${func_stmt.name}/${arity}" return type: ${arity_return_type}')
		}

		// Store the return type for this specific arity using a unique AST ID
		// We'll create a synthetic AST ID for each arity by combining function AST ID with arity
		arity_ast_id := func_stmt.ast_id * 1000 + arity
		pos := '${func_stmt.position.line}:${func_stmt.position.column}'
		func_text := 'def ${func_stmt.name}/${arity}(...)'
		hm_inferencer.type_table.assign_type_with_pos(arity_ast_id, arity_return_type,
			pos, func_text)

		if hm_inferencer.type_table.debug_mode {
			println('[HM_INFERENCER] Stored function "${func_stmt.name}/${arity}" return type in TypeTable with AST ID: ${arity_ast_id}')
		}

		// Create function type for this arity
		first_clause := clauses[0]
		mut param_types := []TypeInfo{}

		for param in first_clause.parameters {
			match param {
				ast.VarPattern {
					if type_ann := param.type_annotation {
						param_type := a.convert_type_expr_to_type_info(type_ann)
						param_types << param_type
					} else {
						param_types << typeinfo_any()
					}
				}
				else {
					param_types << typeinfo_any()
				}
			}
		}

		// Create function type
		mut all_types := param_types.clone()
		all_types << arity_return_type

		function_type := TypeInfo{
			generic: 'function'
			value:   '${param_types.len}' // arity
			values:  all_types
		}

		// Bind function to environment so it can be called
		hm_inferencer.type_env.bind(func_stmt.name, monotype(function_type))

		if hm_inferencer.type_table.debug_mode {
			println('[HM_INFERENCER] Bound function "${func_stmt.name}/${arity}" to type: ${function_type}')
		}
	}

	// Also store the overall function return type (union of all arities) for compatibility
	// Process each clause and collect return types
	mut all_clause_return_types := []TypeInfo{}
	for clause in func_stmt.clauses {
		// Create a new environment scope for this clause
		clause_env := hm_inferencer.type_env.extend()
		original_env := hm_inferencer.type_env
		hm_inferencer.type_env = clause_env

		// Process parameters first to bind their types
		a.process_function_parameters_hm(clause)!

		// Infer types for expressions in the function body
		body_type := hm_inferencer.infer_expression(clause.body)!

		// Check if clause has return type annotation
		if return_type_expr := clause.return_type {
			expected_return_type := a.convert_type_expr_to_type_info(return_type_expr)
			hm_inferencer.add_constraint(body_type, expected_return_type)
			all_clause_return_types << expected_return_type
		} else {
			// No annotation - use inferred body type
			all_clause_return_types << body_type
		}

		// Restore original environment
		hm_inferencer.type_env = original_env
	}

	// Determine the function's overall return type
	function_return_type := if all_clause_return_types.len == 1 {
		all_clause_return_types[0]
	} else {
		// Multiple clauses - create union type
		typeinfo_union(all_clause_return_types)
	}

	// CRITICAL: Also store function RETURN TYPE in TypeTable for logging
	if func_stmt.ast_id > 0 {
		pos := '${func_stmt.position.line}:${func_stmt.position.column}'
		func_text := 'def ${func_stmt.name}(...)'
		hm_inferencer.type_table.assign_type_with_pos(func_stmt.ast_id, function_return_type,
			pos, func_text)

		if hm_inferencer.type_table.debug_mode {
			println('[HM_INFERENCER] Stored function "${func_stmt.name}" return type in TypeTable with AST ID: ${func_stmt.ast_id}')
		}
	} else {
		if hm_inferencer.type_table.debug_mode {
			println('[HM_INFERENCER] WARNING: Function "${func_stmt.name}" has invalid AST ID: ${func_stmt.ast_id}')
		}
	}

	// Resolve constraints and apply substitutions
	if hm_inferencer.constraints.len > 0 {
		if hm_inferencer.type_table.debug_mode {
			println('[HM_INFERENCER] DEBUG: Function "${func_stmt.name}" constraints after processing:')
			for i, constraint in hm_inferencer.constraints {
				println('[HM_INFERENCER] DEBUG: Constraint ${i + 1}: ${constraint.left} = ${constraint.right}')
			}
		}

		// Resolve all constraints
		substitution := hm_inferencer.resolve_constraints()!

		if hm_inferencer.type_table.debug_mode {
			println('[HM_INFERENCER] DEBUG: Applied substitution: ${substitution.str()}')
		}
	}

	// Check if there are any type errors and prevent compilation
	if hm_inferencer.has_errors() {
		return error('Type errors detected in function "${func_stmt.name}"')
	}
}

// process_function_parameters_hm processes function parameters and binds their types to HM environment
fn (mut a Analyzer) process_function_parameters_hm(clause ast.FunctionClause) ! {
	// Get access to the HM inferencer
	mut hm_inferencer := &a.hm_inferencer

	if hm_inferencer.type_table.debug_mode {
		println('[HM_INFERENCER] Processing ${clause.parameters.len} function parameters')
	}

	// Process each parameter
	for param in clause.parameters {
		match param {
			ast.VarPattern {
				// Check if parameter has type annotation
				if type_ann := param.type_annotation {
					// Convert type annotation to TypeInfo
					param_type := a.convert_type_expr_to_type_info(type_ann)

					// Bind the parameter name to its annotated type in HM environment
					hm_inferencer.type_env.bind(param.name, monotype(param_type))

					if hm_inferencer.type_table.debug_mode {
						println('[HM_INFERENCER] Bound parameter "${param.name}" to type: ${param_type}')
					}
				} else {
					// No type annotation - create fresh type variable
					fresh_type := hm_inferencer.fresh_type_var()
					hm_inferencer.type_env.bind(param.name, monotype(fresh_type))

					if hm_inferencer.type_table.debug_mode {
						println('[HM_INFERENCER] Bound parameter "${param.name}" to fresh type: ${fresh_type}')
					}
				}
			}
			else {
				// For other pattern types, we'll implement later
				if hm_inferencer.type_table.debug_mode {
					println('[HM_INFERENCER] Skipping non-variable parameter pattern')
				}
			}
		}
	}
}

// convert_type_expr_to_type_info converts a TypeExpression to TypeInfo
fn (a Analyzer) convert_type_expr_to_type_info(type_expr ast.TypeExpression) TypeInfo {
	return match type_expr {
		ast.SimpleTypeExpr {
			match type_expr.name {
				'integer' {
					typeinfo_integer()
				}
				'float' {
					typeinfo_float()
				}
				'string' {
					typeinfo_string()
				}
				'atom' {
					typeinfo_atom()
				}
				'boolean' {
					typeinfo_boolean()
				}
				'any' {
					typeinfo_any()
				}
				else {
					// Check if it's an atom literal (starts with :)
					if type_expr.name.starts_with(':') {
						// It's an atom literal like :ok, :error, etc.
						typeinfo_atom_value(type_expr.name)
					} else {
						// For unknown types, create a named type
						TypeInfo{
							generic: 'named'
							value:   type_expr.name
							values:  []
						}
					}
				}
			}
		}
		ast.ListTypeExpr {
			element_type := a.convert_type_expr_to_type_info(type_expr.element_type)
			typeinfo_list(element_type)
		}
		ast.TupleTypeExpr {
			mut element_types := []TypeInfo{}
			for elem_type in type_expr.element_types {
				element_types << a.convert_type_expr_to_type_info(elem_type)
			}
			typeinfo_tuple(element_types)
		}
		ast.FunctionTypeExpr {
			mut param_types := []TypeInfo{}
			for param_type in type_expr.param_types {
				param_types << a.convert_type_expr_to_type_info(param_type)
			}
			return_type := a.convert_type_expr_to_type_info(type_expr.return_type)

			// Create a function type representation
			mut all_types := param_types.clone()
			all_types << return_type

			TypeInfo{
				generic: 'function'
				value:   '${param_types.len}' // arity
				values:  all_types
			}
		}
		ast.UnionTypeExpr {
			// For union types, we'll use the first type for now
			// In a full implementation, we'd create a proper union type
			if type_expr.types.len > 0 {
				a.convert_type_expr_to_type_info(type_expr.types[0])
			} else {
				typeinfo_any()
			}
		}
		ast.MapTypeExpr {
			key_type := a.convert_type_expr_to_type_info(type_expr.key_type)
			value_type := a.convert_type_expr_to_type_info(type_expr.value_type)
			typeinfo_map(key_type, value_type)
		}
		ast.RecordTypeExpr {
			typeinfo_record(type_expr.name)
		}
		ast.VariableTypeExpr {
			// For type variables, create a fresh type variable
			TypeInfo{
				generic: 'typevar'
				value:   type_expr.name
				values:  []
			}
		}
	}
}

// process_record_definition_hm processes record definitions and registers them in the HM environment
fn (mut a Analyzer) process_record_definition_hm(record_stmt ast.RecordDefStmt) {
	mut hm_inferencer := &a.hm_inferencer

	if hm_inferencer.type_table.debug_mode {
		println('[HM_INFERENCER] Processing record definition: ${record_stmt.name}')
	}

	// Create a record type with field information
	for field in record_stmt.fields {
		field_type := a.convert_type_expr_to_type_info(field.field_type)
		key := '${record_stmt.name}.${field.name}'
		a.hm_inferencer.record_field_types[key] = field_type
		if a.hm_inferencer.type_table.debug_mode {
			println('[HM_INFERENCER] Record field ${key}: ${field_type}')
		}
	}

	// Create the record type
	record_type := TypeInfo{
		generic: 'record'
		value:   record_stmt.name
		values:  []TypeInfo{} // We'll store field types in a different way
	}

	// Bind the record name to its type in the environment
	a.hm_inferencer.type_env.bind(record_stmt.name, monotype(record_type))

	if a.hm_inferencer.type_table.debug_mode {
		println('[HM_INFERENCER] Registered record "${record_stmt.name}" with type: ${record_type}')
	}
}

// process_worker_hm processes worker statements with HM type checking
fn (mut a Analyzer) process_worker_hm(worker_stmt ast.WorkerStmt) ! {
	// Process all statements within the worker
	for stmt in worker_stmt.statements {
		match stmt {
			ast.FunctionStmt {
				a.process_function_hm(stmt)!
			}
			ast.RecordDefStmt {
				a.process_record_definition_hm(stmt)
			}
			else {
				// Handle other statement types as needed
			}
		}
	}
}

// process_supervisor_hm processes supervisor statements with HM type checking
fn (mut a Analyzer) process_supervisor_hm(supervisor_stmt ast.SupervisorStmt) ! {
	// Process all statements within the supervisor
	for stmt in supervisor_stmt.statements {
		match stmt {
			ast.FunctionStmt {
				a.process_function_hm(stmt)!
			}
			ast.RecordDefStmt {
				a.process_record_definition_hm(stmt)
			}
			else {
				// Handle other statement types as needed
			}
		}
	}
}
