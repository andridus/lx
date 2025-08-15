module analysis

import ast
import errors
import kernel

pub struct Analyzer {
mut:
	error_reporter   errors.ErrorReporter
	type_table       TypeTable
	type_envs        []TypeEnv
	current_env      int
	analysis_context AnalysisContext
	otp_contexts     []OtpContextInfo
}

enum AnalysisContext {
	expression         // Regular expression context
	pattern            // Pattern matching context
	function_parameter // Function parameter context
	record_field       // Record field definition context
}

struct OtpContextInfo {
	name            string
	local_functions map[string]bool
}

pub fn (a Analyzer) lookup(name string) ?TypeScheme {
	// Search from current scope back to root, so inner scopes see outer bindings (e.g., function params inside case clauses)
	for i := a.current_env; i >= 0; i-- {
		if name in a.type_envs[i].bindings {
			return a.type_envs[i].bindings[name]
		}
	}
	return none
}

pub fn (mut a Analyzer) bind(name string, scheme TypeScheme) {
	a.type_envs[a.current_env].bindings[name] = scheme
}

pub fn (mut a Analyzer) bind_with_position(name string, scheme TypeScheme, position ast.Position) {
	a.type_envs[a.current_env].bind_with_position(name, scheme, position)
}

pub fn (mut a Analyzer) mark_variable_used(name string) {
	// Mark variable as used in the scope where it was found
	for i := a.current_env; i >= 0; i-- {
		if name in a.type_envs[i].bindings {
			a.type_envs[i].mark_used(name)
			break
		}
	}
}

pub fn (mut a Analyzer) check_unused_variables() ! {
	// Check for unused variables in current function scope
	if a.current_env > 0 {
		unused_vars := a.type_envs[a.current_env].get_unused_variables_with_positions()
		for var_name, position in unused_vars {
			// Skip underscore variables as they are intended to be unused
			if !var_name.starts_with('_') && var_name != '_pattern' {
				a.error('Unused variable: `${var_name}`. If this variable is intentionally unused, prefix it with an underscore: `_${var_name}`',
					position)
				return error('Unused variable: ${var_name}')
			}
		}
	}
}

pub fn (mut a Analyzer) enter_scope(scope_name string) {
	new_env := new_type_env(scope_name)
	a.type_envs << new_env
	a.current_env = a.type_envs.len - 1
}

pub fn (mut a Analyzer) exit_scope() {
	if a.current_env > 0 {
		a.current_env--
	}
}

fn (mut a Analyzer) with_context(ctx AnalysisContext, f fn (mut Analyzer) !ast.Node) !ast.Node {
	old_context := a.analysis_context
	a.analysis_context = ctx
	defer { a.analysis_context = old_context }
	return f(mut a)
}

fn (mut a Analyzer) extract_type_from_annotation(node ast.Node) !ast.Type {
	// Se o nó é um identifier simples (tipo básico), usa type_node_to_type para marcar como usado
	if node.kind == .identifier {
		return a.type_node_to_type(node)
	}

	// Se o nó não é uma anotação de tipo válida, retorna 'any'
	if node.kind != .type_annotation {
		return ast.Type{
			name:   'any'
			params: []
		}
	}

	// Caso contrário, analisa como type_annotation
	analyzed := a.analyze_type_annotation(node)!
	return a.type_table.get_type(analyzed.id) or {
		ast.Type{
			name:   node.value
			params: []
		}
	}
}

// Convert an AST node representing a type expression into an ast.Type recursively
fn (mut a Analyzer) type_node_to_type(node ast.Node) ast.Type {
	// Handle atoms as specialized atom types
	if node.kind == .atom {
		return ast.Type{
			name:              'atom'
			params:            []
			specialized_value: node.value
		}
	}

	// Handle union types
	if node.kind == .union_type {
		// For now, represent union types as 'union' type with variant types as params
		mut variant_types := []ast.Type{}
		for child in node.children {
			variant_types << a.type_node_to_type(child)
		}
		return ast.Type{
			name:   'union'
			params: variant_types
		}
	}

	// Handle tuple types
	if node.kind == .tuple_literal {
		mut element_types := []ast.Type{}
		for child in node.children {
			element_types << a.type_node_to_type(child)
		}
		return ast.Type{
			name:   'tuple'
			params: element_types
		}
	}

	// Handle list types (including empty lists)
	if node.kind == .list_literal {
		if node.children.len == 0 {
			// Empty list type []
			return ast.Type{
				name:   'list'
				params: []
			}
		} else {
			// List with element types
			mut element_types := []ast.Type{}
			for child in node.children {
				element_types << a.type_node_to_type(child)
			}
			return ast.Type{
				name:   'list'
				params: element_types
			}
		}
	}

	// Identifier represents either a basic/custom type, a generic, a record, or an atom literal
	if node.kind == .identifier {
		// Handle parameterized types: name(T1, T2, ...)
		if node.children.len > 0 {
			mut param_types := []ast.Type{}
			for child in node.children {
				param_types << a.type_node_to_type(child)
			}
			return ast.Type{
				name:   node.value
				params: param_types
			}
		}

		// No params: check for built-ins first
		match node.value {
			'integer', 'float', 'string', 'boolean', 'atom', 'any', 'term', 'module', 'nil',
			'list', 'tuple', 'map' {
				return ast.Type{
					name:   node.value
					params: []
				}
			}
			else {}
		}

		// Check if this is a record type (either starts with capital or is a defined record)
		if node.value.len > 0 && node.value[0].is_capital() {
			return ast.Type{
				name:   node.value
				params: []
			}
		}

		// Check if this identifier is a defined record type
		if _ := a.type_table.get_record_type(node.value) {
			// Mark record type as used
			a.type_table.mark_type_used(node.value)
			return ast.Type{
				name:              node.value
				params:            []
				specialized_value: 'record_type'
			}
		}

		// Check if this identifier is a defined custom type
		if _ := a.type_table.get_custom_type(node.value) {
			// Mark custom type as used
			a.type_table.mark_type_used(node.value)
			return ast.Type{
				name:              node.value
				params:            []
				specialized_value: 'custom_type'
			}
		}

		// Check if this is a valid built-in type or generic type variable
		// Single uppercase letters are generic type variables (T, U, V, etc.)
		if node.value.len == 1 && node.value[0].is_capital() {
			return ast.Type{
				name:   node.value
				params: []
			}
		}

		// If we reach here, it's potentially an undefined type
		// During pre-processing, not all types may be available yet
		// So we return a placeholder and let the actual analysis phase handle validation
		return ast.Type{
			name:   node.value
			params: []
		}
	}

	// Fallback for unexpected nodes: treat as 'any'
	return ast.Type{
		name:   'any'
		params: []
	}
}

pub fn new_analyzer() Analyzer {
	return Analyzer{
		type_table:       new_type_table()
		type_envs:        [new_type_env('root')]
		error_reporter:   errors.new_error_reporter()
		analysis_context: .expression
	}
}

pub fn (mut a Analyzer) analyze(node ast.Node) !ast.Node {
	return a.analyze_node(node)
}

pub fn (a Analyzer) get_errors() []errors.Err {
	return a.error_reporter.all()
}

pub fn (a Analyzer) get_function_type(name string) ?FunctionType {
	return a.type_table.get_function_type(name)
}

pub fn (a Analyzer) get_all_function_types() map[string]FunctionType {
	return a.type_table.function_types
}

fn (mut a Analyzer) error(msg string, pos ast.Position) {
	a.error_reporter.report(.analysis, msg, pos)
}

fn (mut a Analyzer) error_with_suggestion(msg string, pos ast.Position, suggestion string) {
	a.error_reporter.report_with_suggestion(.analysis, msg, pos, suggestion)
}

// Check for unused types and report warnings
fn (mut a Analyzer) check_unused_types() {
	unused_types := a.type_table.get_unused_types()
	for type_name in unused_types {
		// Get the position where the type was defined
		pos := a.type_table.get_type_position(type_name) or {
			ast.Position{
				line:   0
				column: 0
				file:   ''
			}
		}
		// For now, just report as an error
		// In the future, this could be a warning instead
		a.error('Unused type: ${type_name}', pos)
	}
}

// Check if a type name is valid (built-in, record, or custom type)
fn (mut a Analyzer) is_valid_type(type_name string) bool {
	// Check if it's a built-in type
	builtin_types := ['integer', 'float', 'atom', 'string', 'binary', 'boolean', 'any', 'list',
		'tuple', 'map']
	if type_name in builtin_types {
		return true
	}

	// Check if it's a registered record type
	if _ := a.type_table.get_record_type(type_name) {
		return true
	}

	// Check if it's a registered custom type
	if _ := a.type_table.get_custom_type(type_name) {
		return true
	}

	// Check if it's a generic type variable (single uppercase letter)
	if type_name.len == 1 && type_name[0].is_capital() {
		return true
	}

	return false
}

// Check if two types are compatible for return type validation
fn (mut a Analyzer) are_types_compatible(inferred ast.Type, annotated ast.Type) bool {
	// Rule 3: If inferred type is 'any', any annotated type is compatible
	if inferred.name == 'any' {
		return true
	}

	// Rule 1: If types are exactly the same, check specialized values
	if inferred.name == annotated.name {
		// If both have specialized values, they must match exactly
		if inferred_spec := inferred.specialized_value {
			if annotated_spec := annotated.specialized_value {
				return inferred_spec == annotated_spec
			}
		}
		// If no specialized values to check, types are compatible
		return true
	}

	// Rule 2: If annotated type is a custom type alias, check what it resolves to
	if specialized := annotated.specialized_value {
		if specialized == 'custom_type' {
			// Get the actual type definition for the custom type
			if actual_type := a.type_table.get_custom_type(annotated.name) {
				// Check if the inferred type matches the actual underlying type
				return a.are_types_compatible(inferred, actual_type)
			}
		}
	}

	// Handle specialized atom types (like :ok, :error)
	if inferred.name == 'atom' && annotated.name == 'atom' {
		// Check if annotated type has a specialized value (specific atom)
		if annotated_spec := annotated.specialized_value {
			// If annotated type is specialized (e.g., :ok), check if inferred matches
			if inferred_spec := inferred.specialized_value {
				// Both are specialized atoms, they must match exactly
				return inferred_spec == annotated_spec
			} else {
				// Inferred is generic atom, annotated is specific - incompatible
				return false
			}
		} else {
			// Annotated is generic atom, any atom is compatible
			return true
		}
	}

	// Add more compatibility rules as needed
	return false
}

fn (mut a Analyzer) analyze_node(node ast.Node) !ast.Node {
	return match node.kind {
		.module {
			a.analyze_module(node)
		}
		.function, .private_function {
			// All functions (public or private) are named functions
			a.analyze_function(node)
		}
		.variable_binding {
			a.analyze_binding(node)
		}
		.variable_ref {
			a.analyze_variable_ref(node)
		}
		.identifier {
			a.analyze_identifier(node)
		}
		.block {
			a.analyze_block(node)
		}
		.integer, .float, .string, .string_charlist, .boolean, .atom, .nil {
			a.analyze_literal(node)
		}
		.function_caller {
			a.analyze_function_caller(node)
		}
		.external_function_call {
			return a.analyze_external_function_call(node)
		}
		.parentheses {
			a.analyze_parentheses(node)
		}
		.directive_call {
			a.analyze_directive_call(node)
		}
		.list_literal {
			a.analyze_list_literal(node)
		}
		.list_cons {
			a.analyze_list_cons(node)
		}
		.tuple_literal {
			a.analyze_tuple_literal(node)
		}
		.map_literal {
			a.analyze_map_literal(node)
		}
		.map_access {
			a.analyze_map_access(node)
		}
		.record_definition {
			a.analyze_record_definition(node)
		}
		.record_literal {
			a.analyze_record_literal(node)
		}
		.record_access {
			a.analyze_record_access(node)
		}
		.record_update {
			a.analyze_record_update(node)
		}
		.function_parameter {
			a.analyze_function_parameter(node)
		}
		.lambda_expression {
			a.analyze_lambda_expression(node)
		}
		.case_expression {
			a.analyze_case_expression(node)
		}
		.case_clause {
			a.analyze_case_clause(node)
		}
		.pattern_match {
			a.analyze_pattern_match(node)
		}
		.pattern_binding {
			a.analyze_pattern_binding(node)
		}
		.type_alias {
			a.analyze_type_alias(node)
		}
		.type_annotation {
			a.analyze_type_annotation(node)
		}
		// Task 11: Control Flow
		.if_expr {
			a.analyze_if_expr(node)
		}
		.with_expr {
			a.analyze_with_expr(node)
		}
		.match_expr {
			a.analyze_match_expr(node)
		}
		// Task 11: Concurrency
		.spawn_expr {
			a.analyze_spawn_expr(node)
		}
		.send_expr {
			a.analyze_send_expr(node)
		}
		.receive_expr {
			a.analyze_receive_expr(node)
		}
		.supervisor_def {
			a.analyze_supervisor_def(node)
		}
		.worker_def {
			a.analyze_worker_def(node)
		}
		// Task 11: Binaries
		.binary_literal {
			a.analyze_binary_literal(node)
		}
		.binary_pattern {
			a.analyze_binary_pattern(node)
		}
		.binary_segment {
			a.analyze_binary_segment(node)
		}
		// Task 11: Custom Types
		.type_def {
			a.analyze_type_def(node)
		}
		.union_type {
			a.analyze_union_type(node)
		}
		.generic_type {
			a.analyze_generic_type(node)
		}
		.opaque_type {
			a.analyze_opaque_type(node)
		}
		.nominal_type {
			a.analyze_nominal_type(node)
		}
		// Task 11: Module System
		.deps_declaration {
			a.analyze_deps_declaration(node)
		}
		.application_config {
			a.analyze_application_config(node)
		}
		.import_statement {
			a.analyze_import_statement(node)
		}
		// Task 11: Advanced Features
		.string_interpolation {
			a.analyze_string_interpolation(node)
		}
		.anonymous_function {
			a.analyze_anonymous_function(node)
		}
		.lambda_call {
			a.analyze_lambda_call(node)
		}
		.list_comprehension {
			a.analyze_list_comprehension(node)
		}
		.directive {
			a.analyze_directive(node)
		}
		.test_block {
			a.analyze_test_block(node)
		}
		else {
			a.error('Unsupported node type: ${node.kind}', node.position)
			return error('Unsupported node type: ${node.kind}')
		}
	}
}

fn (mut a Analyzer) analyze_module(node ast.Node) !ast.Node {
	mut analyzed_children := []ast.Node{}

	mut function_names := map[string]bool{}

	// First pass: pre-register all records and custom types for forward references
	for child in node.children {
		if child.kind == .record_definition {
			record_name := child.value
			// Pre-register record type (empty for now, will be filled later)
			mut field_types := map[string]ast.Type{}
			mut field_defaults := map[string]ast.Node{}
			a.type_table.register_record_type(record_name, field_types, field_defaults)
			// Register position for error reporting
			a.type_table.register_type_position(record_name, child.position)
		} else if child.kind == .type_def || child.kind == .opaque_type
			|| child.kind == .nominal_type {
			type_name := child.value
			// Pre-register custom type (will be properly filled during analysis)
			// Just register the name for now, the actual type will be set during analyze_type_def
			if child.children.len > 0 {
				base_type := a.type_node_to_type(child.children[0])
				a.type_table.register_custom_type(type_name, base_type)
			}
			// Register position for error reporting
			a.type_table.register_type_position(type_name, child.position)
		}
	}

	// Validate that all custom type base types exist
	for child in node.children {
		if child.kind == .type_def || child.kind == .opaque_type || child.kind == .nominal_type {
			if child.children.len > 0 {
				base_type_node := child.children[0]
				if base_type_node.kind == .identifier {
					base_type_name := base_type_node.value
					// Check if the base type exists (built-in, record, or custom type)
					if !a.is_valid_type(base_type_name) {
						a.error('Undefined type: ${base_type_name}', base_type_node.position)
						return error('Undefined type: ${base_type_name}')
					}
				}
			}
		}
	}

	// Second pass: pre-register all function signatures for forward references
	for child in node.children {
		if child.kind == .function || child.kind == .private_function {
			func_name := child.value
			if func_name in function_names {
				a.error('Duplicate function name: ${func_name}', child.position)
			}
			function_names[func_name] = true

			// Pre-register function with basic signature
			a.preregister_function(child)!
		}
	}

	// Third pass: analyze all nodes with function signatures and records already available
	for child in node.children {
		analyzed_child := a.analyze_node(child)!
		analyzed_children << analyzed_child
	}

	a.type_table.assign_type(node.id, ast.Type{
		name:   'module'
		params: []
	})

	// Check for unused types at the end of module analysis
	a.check_unused_types()

	return ast.Node{
		...node
		children: analyzed_children
	}
}

// Pre-register function signature for forward reference resolution
fn (mut a Analyzer) preregister_function(node ast.Node) ! {
	if node.kind != .function && node.kind != .private_function {
		return
	}

	function_name := node.value
	if function_name == '' {
		return
	}

	if node.children.len < 2 {
		return
	}

	parameters_node := node.children[0]
	mut parameters := []ast.Type{}

	// Extract parameter types (use 'any' as default)
	for param in parameters_node.children {
		param_type := if param.children.len > 0 {
			a.extract_type_from_annotation(param.children[0]) or {
				ast.Type{
					name:   'any'
					params: []
				}
			}
		} else {
			ast.Type{
				name:   'any'
				params: []
			}
		}
		parameters << param_type
	}

	// Extract return type (use 'any' as default if not specified)
	return_type := if node.children.len > 2 {
		return_type_node := node.children[2]
		if return_type_node.children.len > 0 {
			a.extract_type_from_annotation(return_type_node) or {
				ast.Type{
					name:   'any'
					params: []
				}
			}
		} else {
			ast.Type{
				name:   'any'
				params: []
			}
		}
	} else {
		ast.Type{
			name:   'any'
			params: []
		}
	}

	// Register function type in global type table for forward references
	a.type_table.register_function_type(function_name, parameters, return_type)
}

fn (mut a Analyzer) analyze_function(node ast.Node) !ast.Node {
	// Only check scope for named functions, not anonymous functions (heads)
	if node.value != '' && a.current_env > 0 {
		// Supervisor and worker contexts are treated as global scope since they generate separate files
		current_scope_name := a.type_envs[a.current_env].scope_name
		is_otp_context := current_scope_name.starts_with('supervisor_')
			|| current_scope_name.starts_with('worker_')

		if !is_otp_context {
			a.error('Function definitions are only allowed in global scope, not inside functions',
				node.position)
			return error('Function definitions are only allowed in global scope, not inside functions')
		}
	}

	function_name := node.value

	// For function heads (function_name == ''), register pattern variables immediately
	if function_name == '' && node.children.len >= 2 {
		args_block := node.children[0]

		// Register variables from patterns in arguments
		for arg in args_block.children {
			a.register_pattern_variables(arg)
		}
	}

	// Parse args, body, and optional return type from children
	if node.children.len < 2 {
		a.error('Function must have args and body', node.position)
		return error('Function must have args and body')
	}

	args_block := node.children[0]
	body := node.children[1]

	// Check for return type annotation (third child)
	mut return_type_annotation := ast.Type{
		name:   'any'
		params: []
	}
	if node.children.len > 2 {
		return_type_annotation = a.extract_type_from_annotation(node.children[2])!
	}

	// Create a new environment for this function (only for named functions)
	if function_name != '' {
		a.type_envs << new_type_env(function_name)
		a.current_env = a.type_envs.len - 1
	}

	// Analyze args and add them to the function's environment
	mut parameters := []ast.Type{}
	mut parameter_names := []string{}

	for arg in args_block.children {
		// Set context to function_parameter when analyzing function arguments
		old_context := a.analysis_context
		a.analysis_context = .function_parameter

		// Check if this is a pattern argument (has children with patterns)
		if arg.value == '' && arg.children.len > 0 {
			// This is a pattern argument, register all variables within the pattern
			pattern := arg.children[0] // The pattern is the first child
			a.register_pattern_variables(pattern)

			// For pattern arguments, we use 'any' type for now
			arg_type := ast.Type{
				name:   'any'
				params: []
			}
			parameters << arg_type
			parameter_names << '_pattern' // Placeholder name for pattern
		} else {
			// Regular named argument
			arg_name := arg.value
			arg_type := if arg.children.len > 0 {
				a.extract_type_from_annotation(arg.children[0])!
			} else {
				ast.Type{
					name:   'any'
					params: []
				}
			}

			parameters << arg_type
			parameter_names << arg_name

			// Add argument to the function's environment
			a.bind(arg_name, TypeScheme{
				quantified_vars: []
				body:            arg_type
			})
		}

		// Restore context
		a.analysis_context = old_context
	}

	// Register function type in global type table FIRST (only for named functions)
	if function_name != '' {
		// Check if this is a multi-head function (no args in definition but heads have args)
		has_function_heads := args_block.children.len == 0 && body.kind == .block
			&& body.children.len > 0 && body.children[0].kind == .function

		// Validate: cannot have both arguments in definition AND multiple heads in body
		if args_block.children.len > 0 && body.kind == .block && body.children.len > 0
			&& body.children[0].kind == .function {
			a.error('Function ${function_name} cannot have both arguments in definition and multiple heads in body. Use either "def ${function_name}(args) do body end" or "def ${function_name} do (args) -> body end"',
				node.position)
			return error('Invalid function definition: cannot mix argument definition with multiple heads')
		}

		if has_function_heads {
			// Collect all parameter types from all heads to create union types
			parameters = [] // Reset parameters array
			parameter_names = [] // Reset parameter names

			// For single-argument functions, collect all argument types
			if body.children.len > 0 {
				first_head := body.children[0]
				if first_head.children.len > 0 {
					first_head_args := first_head.children[0]

					// Determine if this is a single-argument or multi-argument function
					is_single_arg := first_head_args.kind != .block
						|| first_head_args.children.len == 1

					if is_single_arg {
						// Single argument function - collect types from all heads
						mut all_arg_types := []ast.Type{}

						for head in body.children {
							if head.kind == .function && head.children.len > 0 {
								head_args := head.children[0]

								// Get the actual argument (handle both direct and block cases)
								actual_arg := if head_args.kind == .block
									&& head_args.children.len == 1 {
									head_args.children[0]
								} else {
									head_args
								}

								// Extract type based on argument kind
								arg_type := if actual_arg.kind == .identifier
									&& actual_arg.children.len > 0
									&& actual_arg.children[0].kind == .identifier {
									// Has type annotation (like x :: integer)
									a.extract_type_from_annotation(actual_arg.children[0])!
								} else {
									// No type annotation, infer from kind
									match actual_arg.kind {
										.integer {
											ast.Type{
												name:   'integer'
												params: []
											}
										}
										.float {
											ast.Type{
												name:   'float'
												params: []
											}
										}
										.string {
											ast.Type{
												name:   'string'
												params: []
											}
										}
										.boolean {
											ast.Type{
												name:   'boolean'
												params: []
											}
										}
										.atom {
											ast.Type{
												name:   'atom'
												params: []
											}
										}
										.nil {
											ast.Type{
												name:   'nil'
												params: []
											}
										}
										.identifier {
											ast.Type{
												name:   'any'
												params: []
											}
										} // Variable binding
										.list_literal {
											// Analyze elements to infer list element type
											mut element_types := []ast.Type{}
											for elem in actual_arg.children {
												if elem.kind == .identifier && elem.children.len > 0
													&& elem.children[0].kind == .identifier {
													// Element has type annotation
													elem_type := a.extract_type_from_annotation(elem.children[0]) or {
														ast.Type{
															name:   'any'
															params: []
														}
													}
													element_types << elem_type
												} else {
													// No annotation, infer from element kind
													elem_type := match elem.kind {
														.integer {
															ast.Type{
																name:   'integer'
																params: []
															}
														}
														.float {
															ast.Type{
																name:   'float'
																params: []
															}
														}
														.string {
															ast.Type{
																name:   'string'
																params: []
															}
														}
														.atom {
															ast.Type{
																name:   'atom'
																params: []
															}
														}
														.boolean {
															ast.Type{
																name:   'boolean'
																params: []
															}
														}
														else {
															ast.Type{
																name:   'any'
																params: []
															}
														}
													}
													element_types << elem_type
												}
											}
											// Use first element type or 'any' if empty
											elem_type := if element_types.len > 0 {
												element_types[0]
											} else {
												ast.Type{
													name:   'any'
													params: []
												}
											}
											ast.Type{
												name:   'list'
												params: [elem_type]
											}
										}
										.list_cons {
											// For list cons [head | tail], analyze head element
											mut elem_type := ast.Type{
												name:   'any'
												params: []
											}
											if actual_arg.children.len > 0 {
												list_head := actual_arg.children[0]
												// Handle both direct identifiers and parentheses-wrapped identifiers
												mut actual_head := list_head
												if list_head.kind == .parentheses
													&& list_head.children.len > 0 {
													actual_head = list_head.children[0]
												}

												if actual_head.kind == .identifier
													&& actual_head.children.len > 0
													&& actual_head.children[0].kind == .identifier {
													// Head has type annotation
													elem_type = a.extract_type_from_annotation(actual_head.children[0]) or {
														ast.Type{
															name:   'any'
															params: []
														}
													}
												} else {
													// No annotation, infer from head kind
													elem_type = match list_head.kind {
														.integer {
															ast.Type{
																name:   'integer'
																params: []
															}
														}
														.float {
															ast.Type{
																name:   'float'
																params: []
															}
														}
														.string {
															ast.Type{
																name:   'string'
																params: []
															}
														}
														.atom {
															ast.Type{
																name:   'atom'
																params: []
															}
														}
														.boolean {
															ast.Type{
																name:   'boolean'
																params: []
															}
														}
														else {
															ast.Type{
																name:   'any'
																params: []
															}
														}
													}
												}
											}
											ast.Type{
												name:   'list'
												params: [elem_type]
											}
										}
										else {
											ast.Type{
												name:   'any'
												params: []
											}
										}
									}
								}
								all_arg_types << arg_type
							}
						}

						// Create union type from all argument types
						if all_arg_types.len > 0 {
							mut unified_type := all_arg_types[0]
							for i in 1 .. all_arg_types.len {
								unified_type = a.unify_types(unified_type, all_arg_types[i])
							}
							parameters << unified_type
							parameter_names << 'arg_0'
						}
					} else {
						// Multi-argument function - collect types from all heads for each position
						// First, determine the number of arguments from the first head
						num_args := first_head_args.children.len

						// For each argument position, collect all types from all heads
						for arg_pos in 0 .. num_args {
							mut all_types_for_position := []ast.Type{}

							for head in body.children {
								if head.kind == .function && head.children.len > 0 {
									head_args := head.children[0]
									if head_args.kind == .block && head_args.children.len > arg_pos {
										arg := head_args.children[arg_pos]

										// Extract type based on argument kind and annotations
										arg_type := if arg.kind == .identifier
											&& arg.children.len > 0
											&& arg.children[0].kind == .identifier {
											// Has type annotation (like x :: integer)
											a.extract_type_from_annotation(arg.children[0])!
										} else {
											// No type annotation, infer from kind
											match arg.kind {
												.integer {
													ast.Type{
														name:   'integer'
														params: []
													}
												}
												.float {
													ast.Type{
														name:   'float'
														params: []
													}
												}
												.string {
													ast.Type{
														name:   'string'
														params: []
													}
												}
												.boolean {
													ast.Type{
														name:   'boolean'
														params: []
													}
												}
												.atom {
													ast.Type{
														name:   'atom'
														params: []
													}
												}
												.nil {
													ast.Type{
														name:   'nil'
														params: []
													}
												}
												.identifier {
													ast.Type{
														name:   'any'
														params: []
													}
												} // Variable binding
												.list_literal {
													// Analyze elements to infer list element type
													mut element_types := []ast.Type{}
													for elem in arg.children {
														if elem.kind == .identifier
															&& elem.children.len > 0
															&& elem.children[0].kind == .identifier {
															// Element has type annotation
															elem_type := a.extract_type_from_annotation(elem.children[0]) or {
																ast.Type{
																	name:   'any'
																	params: []
																}
															}
															element_types << elem_type
														} else {
															// No annotation, infer from element kind
															elem_type := match elem.kind {
																.integer {
																	ast.Type{
																		name:   'integer'
																		params: []
																	}
																}
																.float {
																	ast.Type{
																		name:   'float'
																		params: []
																	}
																}
																.string {
																	ast.Type{
																		name:   'string'
																		params: []
																	}
																}
																.atom {
																	ast.Type{
																		name:   'atom'
																		params: []
																	}
																}
																.boolean {
																	ast.Type{
																		name:   'boolean'
																		params: []
																	}
																}
																else {
																	ast.Type{
																		name:   'any'
																		params: []
																	}
																}
															}
															element_types << elem_type
														}
													}
													// Use first element type or 'any' if empty
													elem_type := if element_types.len > 0 {
														element_types[0]
													} else {
														ast.Type{
															name:   'any'
															params: []
														}
													}
													ast.Type{
														name:   'list'
														params: [elem_type]
													}
												}
												.list_cons {
													// For list cons [head | tail], analyze head element
													mut elem_type := ast.Type{
														name:   'any'
														params: []
													}
													if arg.children.len > 0 {
														list_head := arg.children[0]
														if list_head.kind == .identifier
															&& list_head.children.len > 0
															&& list_head.children[0].kind == .identifier {
															// Head has type annotation
															elem_type = a.extract_type_from_annotation(list_head.children[0]) or {
																ast.Type{
																	name:   'any'
																	params: []
																}
															}
														} else {
															// No annotation, infer from head kind
															elem_type = match list_head.kind {
																.integer {
																	ast.Type{
																		name:   'integer'
																		params: []
																	}
																}
																.float {
																	ast.Type{
																		name:   'float'
																		params: []
																	}
																}
																.string {
																	ast.Type{
																		name:   'string'
																		params: []
																	}
																}
																.atom {
																	ast.Type{
																		name:   'atom'
																		params: []
																	}
																}
																.boolean {
																	ast.Type{
																		name:   'boolean'
																		params: []
																	}
																}
																else {
																	ast.Type{
																		name:   'any'
																		params: []
																	}
																}
															}
														}
													}
													ast.Type{
														name:   'list'
														params: [elem_type]
													}
												}
												else {
													ast.Type{
														name:   'any'
														params: []
													}
												}
											}
										}
										all_types_for_position << arg_type
									}
								}
							}

							// Create union type for this argument position
							if all_types_for_position.len > 0 {
								mut unified_type := all_types_for_position[0]
								for i in 1 .. all_types_for_position.len {
									unified_type = a.unify_types(unified_type, all_types_for_position[i])
								}
								parameters << unified_type
								parameter_names << 'arg_${arg_pos}'
							}
						}
					}
				}
			}
		}

		// Register function with extracted parameters
		temp_return_type := if return_type_annotation.name != 'any' {
			return_type_annotation
		} else {
			ast.Type{
				name:   'any'
				params: []
			}
		}
		a.type_table.register_function_type(function_name, parameters, temp_return_type)
	}

	// Analyze function body
	mut analyzed_body := a.analyze_node(body)!

	// Check if function body is empty (only for named functions)
	if function_name != '' {
		is_empty_body := (analyzed_body.kind == .block && analyzed_body.children.len == 0)
			|| (analyzed_body.kind != .block && analyzed_body.value == ''
			&& analyzed_body.children.len == 0)

		if is_empty_body {
			a.error('Function ${function_name} cannot have empty body. Functions must contain at least one expression',
				node.position)
			return error('Function cannot have empty body')
		}
	}

	// Infer return type from body
	mut return_type := ast.Type{
		name:   'any'
		params: []
	}

	// Check if this is a function with multiple heads
	if analyzed_body.kind == .block {
		// Check if this has function heads
		has_function_heads := analyzed_body.children.len > 0
			&& analyzed_body.children[0].kind == .function

		if has_function_heads {
			// Analyze each head and collect return types for unification
			mut head_return_types := []ast.Type{}
			for head in analyzed_body.children {
				if head.kind == .function {
					// Register head arguments in the function's environment
					if head.children.len > 0 {
						head_args := head.children[0]
						for arg in head_args.children {
							a.register_pattern_variables(arg)
						}
					}

					// Analyze head body and collect return type
					if head.children.len > 1 {
						head_body := a.analyze_node(head.children[1])!
						head_return_type := a.type_table.get_type(head_body.id) or {
							ast.Type{
								name:   'any'
								params: []
							}
						}
						head_return_types << head_return_type
					}
				}
			}

			// Unify return types from all heads
			if head_return_types.len > 0 {
				// Filter out 'any' types if we have specific types (for recursive functions)
				mut specific_types := head_return_types.filter(it.name != 'any')

				if specific_types.len > 0 {
					// Use only specific types for unification
					mut unified_type := specific_types[0]
					for i in 1 .. specific_types.len {
						unified_type = a.unify_types(unified_type, specific_types[i])
					}
					return_type = unified_type
				} else {
					// All types are 'any', use standard unification
					mut unified_type := head_return_types[0]
					for i in 1 .. head_return_types.len {
						unified_type = a.unify_types(unified_type, head_return_types[i])
					}
					return_type = unified_type
				}
			}
		} else {
			// Single function body
			body_type := a.type_table.get_type(analyzed_body.id) or {
				ast.Type{
					name:   'unknown'
					params: []
				}
			}
			return_type = body_type
		}
	} else {
		// Single expression body
		body_type := a.type_table.get_type(analyzed_body.id) or {
			ast.Type{
				name:   'unknown'
				params: []
			}
		}
		return_type = body_type
	}

	// Validate and resolve return type annotation if present
	if return_type_annotation.name != 'any' {
		// We have a return type annotation, validate it against inferred type
		inferred_type := return_type
		annotated_type := return_type_annotation

		// Rule 1: If inferred type is different from annotated type, check compatibility
		if !a.are_types_compatible(inferred_type, annotated_type) {
			a.error('Return type mismatch: function returns ${inferred_type.name} but annotated as ${annotated_type.name}',
				node.position)
			return error('Return type mismatch')
		}

		// Rule 2 & 3: If types are compatible, use the annotated type
		// This handles cases where inferred is 'atom' and annotated is 'custom_type :: atom'
		// or where inferred is 'any' and annotated is a specific type
		return_type = annotated_type
	}

	// Para funções com operadores aritméticos, force parâmetros para 'integer'
	if body.kind == .function_caller && body.value in ['+', '-', '*', '/'] {
		for i, _ in parameter_names {
			if parameters[i].name == 'any' {
				parameters[i] = ast.Type{
					name:   'integer'
					params: []
				}
			}
		}
	}

	// Update function type in global type table (only for named functions)
	if function_name != '' {
		a.type_table.register_function_type(function_name, parameters, return_type)
		// Create function type for this node - use the actual return type, not 'function'
		a.type_table.assign_type(node.id, return_type)

		// Check for unused variables before exiting function scope
		a.check_unused_variables()!

		// Restore the previous environment
		a.current_env = 0
	}

	return node
}

fn (mut a Analyzer) analyze_literal(node ast.Node) !ast.Node {
	match node.kind {
		.integer {
			if node.value.len == 0 {
				a.error('Integer value cannot be empty', node.position)
				return error('Integer value cannot be empty')
			}
			// Create specialized integer type with the specific value
			a.type_table.assign_type(node.id, ast.Type{
				name:              'integer'
				params:            []
				specialized_value: node.value
			})
		}
		.float {
			if node.value.len == 0 {
				a.error('Float value cannot be empty', node.position)
				return error('Float value cannot be empty')
			}
			a.type_table.assign_type(node.id, ast.Type{
				name:   'float'
				params: []
			})
		}
		.string {
			// Create specialized binary type with the specific value (strings are binaries in Erlang)
			a.type_table.assign_type(node.id, ast.Type{
				name:              'string'
				params:            []
				specialized_value: node.value
			})
		}
		.string_charlist {
			// Charlist: list of integers
			a.type_table.assign_type(node.id, ast.Type{
				name:   'list'
				params: [ast.Type{
					name:   'integer'
					params: []
				}]
			})
		}
		.boolean {
			if node.value != 'true' && node.value != 'false' {
				a.error('Invalid boolean value: ${node.value}', node.position)
				return error('Invalid boolean value: ${node.value}')
			}
			a.type_table.assign_type(node.id, ast.Type{
				name:   'boolean'
				params: []
			})
		}
		.atom {
			if node.value.len == 0 {
				a.error('Atom name cannot be empty', node.position)
				return error('Atom name cannot be empty')
			}
			if !node.value[0].is_letter() {
				a.error('Atom name must start with letter', node.position)
				return error('Atom name must start with letter')
			}
			// Create specialized atom type with the specific atom value
			a.type_table.assign_type(node.id, ast.Type{
				name:              'atom'
				params:            []
				specialized_value: node.value
			})
		}
		.nil {
			a.type_table.assign_type(node.id, ast.Type{
				name:   'nil'
				params: []
			})
		}
		else {
			a.error('Unknown literal type: ${node.kind}', node.position)
			return error('Unknown literal type: ${node.kind}')
		}
	}

	return node
}

fn (mut a Analyzer) analyze_binding(node ast.Node) !ast.Node {
	if node.children.len != 1 {
		return error('Binding must have name and value')
	}

	var_name := node.value

	// Check for rebind only in the current scope. Shadowing in inner scopes is allowed.
	if var_name in a.type_envs[a.current_env].bindings {
		a.error('Variable "${var_name}" cannot be reassigned. Variables in LX are immutable.',
			node.position)
		return error('Variable "${var_name}" cannot be reassigned')
	}

	// Pre-check raw value for anonymous recursion before deep analysis
	raw_value := node.children[0]
	if raw_value.kind == .lambda_expression {
		if a.lambda_has_self_call(raw_value, var_name) {
			a.error('Anonymous functions cannot be recursive (self-call of "${var_name}")',
				raw_value.position)
			return error('Anonymous functions cannot be recursive')
		}
	}

	value_node := a.analyze_node(raw_value)!

	value_type := a.type_table.get_type(value_node.id) or {
		ast.Type{
			name:   'any'
			params: []
		}
	}

	// Bind variable in current function/block scope with position
	a.bind_with_position(var_name, TypeScheme{
		quantified_vars: []
		body:            value_type
	}, node.position)

	// Assign type to binding node
	a.type_table.assign_type(node.id, value_type)

	return node
}

// Recursively checks if a lambda body performs a lambda call on the same variable name
fn (mut a Analyzer) lambda_has_self_call(lambda_node ast.Node, var_name string) bool {
	// The lambda body is the last child of the lambda expression
	if lambda_node.children.len == 0 {
		return false
	}
	body := lambda_node.children[lambda_node.children.len - 1]
	return a.node_has_lambda_self_call(body, var_name)
}

fn (mut a Analyzer) node_has_lambda_self_call(node ast.Node, var_name string) bool {
	// Detect pattern: lambda_call where first child is a variable_ref with same name
	if node.kind == .lambda_call && node.children.len > 0 {
		callee := node.children[0]
		if (callee.kind == .variable_ref || callee.kind == .identifier) && callee.value == var_name {
			return true
		}
	}
	// Recurse children
	for child in node.children {
		if a.node_has_lambda_self_call(child, var_name) {
			return true
		}
	}
	return false
}

fn (mut a Analyzer) analyze_variable_ref(node ast.Node) !ast.Node {
	var_name := node.value

	if a.analysis_context == .pattern {
		if scheme := a.lookup(var_name) {
			a.type_table.assign_type(node.id, scheme.body)
			return node
		} else {
			a.type_table.assign_type(node.id, ast.Type{
				name:   'any'
				params: []
			})
			return node
		}
	}

	// Look up variable in current function's type environment
	if scheme := a.lookup(var_name) {
		// Mark variable as used
		a.mark_variable_used(var_name)
		a.type_table.assign_type(node.id, scheme.body)
		return node
	} else {
		a.error('Undefined variable: ${var_name}', node.position)
		return error('Undefined variable: ${var_name}')
	}
}

fn (mut a Analyzer) analyze_identifier(node ast.Node) !ast.Node {
	// Check for invalid type annotations in expression context
	if node.children.len > 0 && node.children[0].kind == .identifier
		&& a.analysis_context == .expression {
		a.error('Type annotations with :: are not allowed in expression context. Use :: only in function parameters, pattern matching, or record field definitions',
			node.position)
		return error('Invalid type annotation in expression context')
	}

	// Identifiers should be treated as variable references
	var_name := node.value

	// Look up variable in current function's type environment
	if scheme := a.lookup(var_name) {
		// Mark variable as used
		a.mark_variable_used(var_name)
		a.type_table.assign_type(node.id, scheme.body)
		return node
	} else {
		// If not found, assign a placeholder type
		a.type_table.assign_type(node.id, ast.Type{
			name:   'identifier'
			params: []
		})
		return node
	}
}

fn (mut a Analyzer) analyze_block(node ast.Node) !ast.Node {
	// Enter a new scope for this block so that variables declared inside
	// do not conflict with variables from outer scopes or sibling blocks
	// But don't create nested scopes within OTP components (supervisor/worker)
	mut should_create_scope := true
	if a.current_env > 0 {
		current_scope_name := a.type_envs[a.current_env].scope_name
		if current_scope_name.starts_with('supervisor_')
			|| current_scope_name.starts_with('worker_') {
			should_create_scope = false
		}
	}

	if should_create_scope {
		a.enter_scope('block')
		defer { a.exit_scope() }
	}

	mut analyzed_exprs := []ast.Node{}

	for expr in node.children {
		analyzed_expr := a.analyze_node(expr)!
		analyzed_exprs << analyzed_expr
	}

	// Type of block is type of last expression
	if analyzed_exprs.len > 0 {
		last_expr := analyzed_exprs.last()

		// Mark the last expression as used if it's a variable reference
		// because in Erlang/LX the last expression is the return value
		if last_expr.kind == .variable_ref || last_expr.kind == .identifier {
			a.mark_variable_used(last_expr.value)
		}

		last_type := a.type_table.get_type(last_expr.id) or {
			ast.Type{
				name:   'any'
				params: []
			}
		}
		a.type_table.assign_type(node.id, last_type)
	}

	return ast.Node{
		...node
		children: analyzed_exprs
	}
}

pub fn (a &Analyzer) get_type_table() &TypeTable {
	return &a.type_table
}

fn (mut a Analyzer) analyze_function_caller(node ast.Node) !ast.Node {
	function_name := node.value // Nome da função (ex: '+', '*', '>')

	// Try kernel first
	function_info := kernel.get_function_info(function_name) or {
		// In OTP submodule contexts (worker/supervisor), require external qualifier unless the function is local
		if a.otp_contexts.len > 0 {
			if function_name !in a.otp_contexts[a.otp_contexts.len - 1].local_functions {
				a.error('Undefined local function "${function_name}" in OTP context. Use _:${function_name}(...) or {module}:${function_name}(...).',
					node.position)
				return error('Undefined local function in OTP context')
			}
		}

		// First check if it's a variable in scope (e.g., function parameter)
		if scheme := a.lookup(function_name) {
			// It's a variable, check if it's a function type
			if scheme.body.name == 'function' || scheme.body.name == 'any' {
				// Analyze arguments
				mut analyzed_args := []ast.Node{}
				for arg in node.children {
					analyzed_args << a.analyze_node(arg)!
				}

				// Try to infer return type from function type
				mut return_type := ast.Type{
					name:   'any'
					params: []
				}
				if scheme.body.name == 'function' && scheme.body.params.len > 0 {
					// Extract return type (last parameter in function type)
					return_type = scheme.body.params[scheme.body.params.len - 1]
				}

				a.type_table.assign_type(node.id, return_type)

				return ast.Node{
					id:       node.id
					kind:     node.kind
					value:    node.value
					children: analyzed_args
					position: node.position
				}
			} else {
				a.error('Variable ${function_name} is not a function', node.position)
				return error('Variable ${function_name} is not a function')
			}
		}

		// Try to get from type table for user-defined functions
		if function_type := a.type_table.get_function_type(function_name) {
			// This is a user-defined function
			mut analyzed_args := []ast.Node{}
			for arg in node.children {
				analyzed_args << a.analyze_node(arg)!
			}

			// Validate argument count
			if analyzed_args.len != function_type.parameters.len {
				a.error('Function ${function_name} expects ${function_type.parameters.len} arguments, got ${analyzed_args.len}',
					node.position)
				return error('Function ${function_name} expects ${function_type.parameters.len} arguments, got ${analyzed_args.len}')
			}

			// Assign return type to this node
			a.type_table.assign_type(node.id, function_type.return_type)

			return ast.Node{
				id:       node.id
				kind:     node.kind
				value:    node.value
				children: analyzed_args
				position: node.position
			}
		} else {
			a.error('Unknown function: ${function_name}', node.position)
			return error('Unknown function: ${function_name}')
		}
	}

	mut analyzed_args := []ast.Node{}
	for arg in node.children {
		analyzed_args << a.analyze_node(arg)!
	}

	// Special-case list concatenation to compute precise union of element types
	if function_name == '++' {
		element_types := collect_list_types_rec(node, &a.type_table)
		common_type := a.infer_common_type(element_types) or {
			ast.Type{
				name:   'any'
				params: []
			}
		}
		a.type_table.assign_type(node.id, ast.Type{
			name:   'list'
			params: [
				common_type,
			]
		})
		return ast.Node{
			...node
			children: analyzed_args
		}
	}

	// Para todas as funções do kernel, usa a primeira assinatura válida
	if function_info.signatures.len > 0 {
		// Para funções prefix com múltiplos argumentos
		if function_info.fixity == .prefix && analyzed_args.len > 1 {
			// Aceita a primeira assinatura (assumindo que é válida)
			sig := function_info.signatures[0]
			mut return_type := sig.return_type

			// Special handling for setelement function
			if function_name == 'setelement' && analyzed_args.len == 3 {
				// setelement(index, tuple, new_value) returns a tuple with the same structure
				tuple_type := a.type_table.get_type(analyzed_args[1].id) or {
					ast.Type{
						name:   'unknown'
						params: []
					}
				}

				// If the second argument is a tuple, return tuple() instead of {any()}
				if tuple_type.name == 'tuple' {
					return_type = ast.Type{
						name:   'tuple'
						params: []
					}
				}
			}

			a.type_table.assign_type(node.id, return_type)
			return ast.Node{
				...node
				children: analyzed_args
			}
		}

		// Para operadores infix
		if function_info.fixity == .infix && analyzed_args.len == 2 {
			left_type := a.type_table.get_type(analyzed_args[0].id) or {
				ast.Type{
					name:   'any'
					params: []
				}
			}
			right_type := a.type_table.get_type(analyzed_args[1].id) or {
				ast.Type{
					name:   'any'
					params: []
				}
			}

			// Special case: if both types are 'any' and it's a math operator, assume numeric
			mut effective_left_type := left_type
			mut effective_right_type := right_type

			if function_name in ['+', '-', '*', '/', '<', '>', '<=', '>=', '==', '!='] {
				// If both are 'any', assume integer
				if left_type.name == 'any' && right_type.name == 'any' {
					effective_left_type = ast.Type{
						name:   'integer'
						params: []
					}
					effective_right_type = ast.Type{
						name:   'integer'
						params: []
					}
				}
				// If one is 'any' and the other is numeric, use the numeric type for both
				else if left_type.name == 'any' && right_type.name in ['integer', 'float'] {
					effective_left_type = right_type
				} else if right_type.name == 'any' && left_type.name in ['integer', 'float'] {
					effective_right_type = left_type
				}
			}

			// Special case for list concatenation operator ++
			if function_name == '++' {
				// If both are 'any', assume list
				if left_type.name == 'any' && right_type.name == 'any' {
					effective_left_type = ast.Type{
						name:   'list'
						params: [ast.Type{
							name:   'any'
							params: []
						}]
					}
					effective_right_type = ast.Type{
						name:   'list'
						params: [ast.Type{
							name:   'any'
							params: []
						}]
					}
				}
				// If one is 'any' and the other is list, use list for both
				else if left_type.name == 'any' && right_type.name == 'list' {
					effective_left_type = right_type
				} else if right_type.name == 'any' && left_type.name == 'list' {
					effective_right_type = left_type
				}
			}

			// Tenta encontrar uma assinatura válida
			if return_type := a.check_function_signatures(function_name, effective_left_type,
				effective_right_type, function_info.signatures)
			{
				// Special handling for list concatenation operator
				mut final_return_type := return_type
				if function_name == '++' {
					// If both arguments are lists, try to unify their element types
					if left_type.name == 'list' && right_type.name == 'list'
						&& left_type.params.len == 1 && right_type.params.len == 1 {
						left_elem_type := left_type.params[0]
						right_elem_type := right_type.params[0]

						// Unify element types
						unified_elem_type := a.unify_types(left_elem_type, right_elem_type)

						// Create more specific list type
						final_return_type = ast.Type{
							name:   'list'
							params: [unified_elem_type]
						}
					}
				}

				a.type_table.assign_type(node.id, final_return_type)
				return ast.Node{
					...node
					children: analyzed_args
				}
			} else {
				// Se não encontrou assinatura válida, gera erro
				a.error('Invalid operator: ${left_type.name} ${function_name} ${right_type.name}',
					node.position)
				return error('Invalid operator: ${left_type.name} ${function_name} ${right_type.name}')
			}
		}

		// Para funções prefix com um argumento
		if function_info.fixity == .prefix && analyzed_args.len == 1 {
			sig := function_info.signatures[0]
			a.type_table.assign_type(node.id, sig.return_type)
			return ast.Node{
				...node
				children: analyzed_args
			}
		}
	}

	return ast.Node{
		...node
		children: analyzed_args
	}
}

fn (mut a Analyzer) analyze_external_function_call(node ast.Node) !ast.Node {
	// Parse module:function from the value field
	parts := node.value.split(':')
	if parts.len != 2 {
		a.error('Invalid external function call format: ${node.value}', node.position)
		return error('Invalid external function call format')
	}

	_ := parts[0]
	function_name := parts[1]

	// Analyze all arguments first
	mut analyzed_args := []ast.Node{}
	for arg in node.children {
		analyzed_arg := a.analyze_node(arg)!
		analyzed_args << analyzed_arg
	}

	// Try to use known function type if available (e.g., calls to root module functions)
	if function_type := a.type_table.get_function_type(function_name) {
		// Optional: could validate arity here if needed
		a.type_table.assign_type(node.id, function_type.return_type)
		return ast.Node{
			...node
			children: analyzed_args
		}
	}

	// Fallback: generic external function result when we don't know the type
	a.type_table.assign_type(node.id, ast.Type{ name: 'any', params: [] })
	return ast.Node{
		...node
		children: analyzed_args
	}
}

fn (mut a Analyzer) check_function_signatures(function_name string, left_type ast.Type, right_type ast.Type, signatures []kernel.TypeSignature) !ast.Type {
	for signature in signatures {
		if signature.parameters.len != 2 {
			continue
		}
		expected_left := signature.parameters[0]
		expected_right := signature.parameters[1]

		if a.types_compatible(left_type, expected_left)
			&& a.types_compatible(right_type, expected_right) {
			return signature.return_type
		}
	}

	return error('No matching signature found for function ${function_name}(${left_type.name}, ${right_type.name})')
}

fn (a Analyzer) types_compatible(actual ast.Type, expected ast.Type) bool {
	// Tipos exatos são sempre compatíveis
	if actual.name == expected.name {
		return true
	}

	// Tipos específicos são compatíveis com 'any'
	if expected.name == 'any' {
		return true
	}
	return false
}

fn (mut a Analyzer) check_prefix_function_signatures(function_name string, arg_type ast.Type, signatures []kernel.TypeSignature) !ast.Type {
	for signature in signatures {
		if signature.parameters.len != 1 {
			continue
		}

		expected_arg := signature.parameters[0]

		if a.types_compatible(arg_type, expected_arg) {
			return signature.return_type
		}
	}

	return error('No matching signature found for function ${function_name}(${arg_type.name})')
}

fn (mut a Analyzer) analyze_parentheses(node ast.Node) !ast.Node {
	if node.children.len != 1 {
		return error('Parentheses must contain exactly one expression')
	}

	expr := a.analyze_node(node.children[0])!
	expr_type := a.type_table.get_type(expr.id) or {
		ast.Type{
			name:   'any'
			params: []
		}
	}

	// Parentheses don't change the type
	a.type_table.assign_type(node.id, expr_type)

	return node
}

fn (mut a Analyzer) analyze_directive_call(node ast.Node) !ast.Node {
	directive_name := node.value
	directive_info := get_directive_info(directive_name) or {
		a.error('Unknown directive: $${directive_name}', node.position)
		return error('Unknown directive')
	}

	mut analyzed_args := []ast.Node{}
	for arg in node.children {
		analyzed_arg := a.analyze_node(arg)!
		analyzed_args << analyzed_arg
	}
	if analyzed_args.len != directive_info.argument_count {
		a.error('Directive $${directive_name} requires ${directive_info.argument_count} arguments, got ${analyzed_args.len}',
			node.position)
		return error('Invalid directive argument count')
	}

	if handler := directive_info.handler {
		handler(analyzed_args, a) or {
			a.error('Directive $${directive_name} failed: ${err}', node.position)
			return error('Directive execution failed')
		}
	}

	return node
}

fn (mut a Analyzer) analyze_list_literal(node ast.Node) !ast.Node {
	if node.children.len == 0 {
		// Empty list
		a.type_table.assign_type(node.id, ast.Type{
			name:   'list'
			params: [ast.Type{
				name:   'any'
				params: []
			}]
		})
		return node
	}

	// Analyze all elements
	mut analyzed_elements := []ast.Node{}
	mut element_types := []ast.Type{}

	for element in node.children {
		analyzed_element := a.analyze_node(element)!
		analyzed_elements << analyzed_element

		element_type := a.type_table.get_type(analyzed_element.id) or {
			ast.Type{
				name:   'unknown'
				params: []
			}
		}
		element_types << element_type
	}

	// Infer common type for list elements
	common_type := a.infer_common_type(element_types) or {
		ast.Type{
			name:   'any'
			params: []
		}
	}

	// Assign list type
	list_type := ast.Type{
		name:   'list'
		params: [common_type]
	}
	a.type_table.assign_type(node.id, list_type)

	return ast.Node{
		id:       node.id
		kind:     node.kind
		value:    node.value
		children: analyzed_elements
		position: node.position
	}
}

fn (mut a Analyzer) analyze_list_cons(node ast.Node) !ast.Node {
	if node.children.len != 2 {
		return error('List cons must have exactly 2 children (head and tail)')
	}

	head := a.analyze_node(node.children[0])!
	tail := a.analyze_node(node.children[1])!

	head_type := a.type_table.get_type(head.id) or {
		ast.Type{
			name:   'unknown'
			params: []
		}
	}
	tail_type := a.type_table.get_type(tail.id) or {
		ast.Type{
			name:   'unknown'
			params: []
		}
	}

	// Verify tail is a list
	if tail_type.name != 'list' {
		a.error('Tail must be a list, got ${tail_type.name}', node.position)
		return error('Type mismatch in list cons')
	}

	// Determine the element type for the resulting list
	mut element_type := head_type

	// If tail has elements, create union type with head
	if tail_type.params.len == 1 {
		tail_elem_type := tail_type.params[0]

		// If tail element is 'any' (empty list), preserve head type
		if tail_elem_type.name == 'any' {
			element_type = head_type
		} else if head_type.name != tail_elem_type.name {
			// If head and tail element types are different, create union
			element_type = ast.Type{
				name:   'union'
				params: [head_type, tail_elem_type]
			}
		}
	}

	// Create list type with determined element type
	list_type := ast.Type{
		name:   'list'
		params: [element_type]
	}
	a.type_table.assign_type(node.id, list_type)

	return ast.Node{
		id:       node.id
		kind:     node.kind
		value:    node.value
		children: [head, tail]
		position: node.position
	}
}

fn (mut a Analyzer) infer_common_type(types []ast.Type) ?ast.Type {
	if types.len == 0 {
		return ast.Type{
			name:   'any'
			params: []
		}
	}

	if types.len == 1 {
		return types[0]
	}

	// Check if all types are the same
	mut all_same := true
	first_type := types[0]
	for i := 1; i < types.len; i++ {
		if types[i].name != first_type.name {
			all_same = false
			break
		}
	}

	if all_same {
		return first_type
	}

	// Create union type with all unique types
	mut unique_types := []ast.Type{}
	mut seen_types := map[string]bool{}

	for typ in types {
		if typ.name == 'any' {
			// If any type is 'any', the result is 'any'
			return ast.Type{
				name:   'any'
				params: []
			}
		}

		if !seen_types[typ.name] {
			seen_types[typ.name] = true
			unique_types << typ
		}
	}

	// If we have multiple different types, create a union type
	if unique_types.len > 1 {
		return ast.Type{
			name:   'union'
			params: unique_types
		}
	}

	// If we have only one unique type, return it
	return unique_types[0]
}

fn collect_list_types_rec(node ast.Node, type_table &TypeTable) []ast.Type {
	mut types := []ast.Type{}
	if node.kind == .function_caller && node.value == '++' && node.children.len == 2 {
		types << collect_list_types_rec(node.children[0], type_table)
		types << collect_list_types_rec(node.children[1], type_table)
	} else {
		typ := type_table.get_type(node.id) or {
			ast.Type{
				name:   'any'
				params: []
			}
		}
		if typ.name == 'list' && typ.params.len == 1 {
			if typ.params[0].name == 'union' {
				types << typ.params[0].params
			} else {
				types << typ.params[0]
			}
		}
	}
	return types
}

fn (mut a Analyzer) analyze_tuple_literal(node ast.Node) !ast.Node {
	if node.children.len == 0 {
		// Empty tuple
		tuple_type := ast.Type{
			name:   'tuple'
			params: [ast.Type{
				name:   'any'
				params: []
			}]
		}
		a.type_table.assign_type(node.id, tuple_type)
		return node
	}

	// Analyze all elements
	mut analyzed_elements := []ast.Node{}
	mut element_types := []ast.Type{}

	for element in node.children {
		analyzed_element := a.analyze_node(element)!
		analyzed_elements << analyzed_element

		element_type := a.type_table.get_type(analyzed_element.id) or {
			ast.Type{
				name:   'any'
				params: []
			}
		}
		element_types << element_type
	}

	// Create tuple type with specific element types
	tuple_type := ast.Type{
		name:   'tuple'
		params: element_types
	}
	a.type_table.assign_type(node.id, tuple_type)

	return ast.Node{
		id:       node.id
		kind:     node.kind
		value:    node.value
		children: analyzed_elements
		position: node.position
	}
}

fn (a Analyzer) is_multi_arg_prefix_function(function_name string) bool {
	// Lista de funções nativas prefix que recebem múltiplos argumentos
	multi_arg_prefix_functions := ['element', 'setelement', 'map_size', 'map_get', 'map_put',
		'map_remove']
	return function_name in multi_arg_prefix_functions
}

fn (mut a Analyzer) analyze_map_literal(node ast.Node) !ast.Node {
	if node.children.len == 0 {
		// Empty map
		map_type := ast.Type{
			name:   'map'
			params: [ast.Type{
				name:   'any'
				params: []
			}]
		}
		a.type_table.assign_type(node.id, map_type)
		return node
	}

	// Analyze all key-value pairs
	mut analyzed_entries := []ast.Node{}
	mut key_types := []ast.Type{}
	mut value_types := []ast.Type{}

	for i := 0; i < node.children.len; i += 2 {
		if i + 1 >= node.children.len {
			return error('Map must have key-value pairs')
		}

		key := a.analyze_node(node.children[i])!
		value := a.analyze_node(node.children[i + 1])!

		analyzed_entries << key
		analyzed_entries << value

		key_type := a.type_table.get_type(key.id) or {
			ast.Type{
				name:   'unknown'
				params: []
			}
		}
		value_type := a.type_table.get_type(value.id) or {
			ast.Type{
				name:   'unknown'
				params: []
			}
		}

		key_types << key_type
		value_types << value_type
	}

	// Create map type with key and value types
	// For now, use 'any' for both key and value types since maps can have mixed types
	map_type := ast.Type{
		name:   'map'
		params: [ast.Type{
			name:   'any'
			params: []
		}, ast.Type{
			name:   'any'
			params: []
		}]
	}
	a.type_table.assign_type(node.id, map_type)

	return ast.Node{
		id:       node.id
		kind:     node.kind
		value:    node.value
		children: analyzed_entries
		position: node.position
	}
}

fn (mut a Analyzer) analyze_map_access(node ast.Node) !ast.Node {
	if node.children.len != 2 {
		return error('Map access must have exactly 2 children (map and key)')
	}

	map_expr := a.analyze_node(node.children[0])!
	key_expr := a.analyze_node(node.children[1])!

	// Get types
	map_type := a.type_table.get_type(map_expr.id) or {
		ast.Type{
			name:   'unknown'
			params: []
		}
	}
	_ := a.type_table.get_type(key_expr.id) or {
		ast.Type{
			name:   'unknown'
			params: []
		}
	}

	// Check if map_expr is actually a map
	if map_type.name != 'map' {
		a.error('Cannot access key on non-map type: ${map_type.name}', node.position)
		return error('Cannot access key on non-map type: ${map_type.name}')
	}

	// Try to infer the result type based on the map's value types
	result_type := if map_type.params.len >= 2 {
		// Use the value type from the map
		map_type.params[1]
	} else {
		// Fallback to 'any' if we don't have value type information
		ast.Type{
			name:   'any'
			params: []
		}
	}
	a.type_table.assign_type(node.id, result_type)

	return ast.Node{
		id:       node.id
		kind:     node.kind
		value:    node.value
		children: [map_expr, key_expr]
		position: node.position
	}
}

// Record analysis functions
fn (mut a Analyzer) analyze_record_definition(node ast.Node) !ast.Node {
	// Validate that record definitions are only allowed in global scope
	if a.current_env > 0 {
		a.error('Record definitions are only allowed in global scope, not inside functions',
			node.position)
		return error('Record definitions are only allowed in global scope, not inside functions')
	}

	record_name := node.value
	mut field_types := map[string]ast.Type{}
	mut field_defaults := map[string]ast.Node{}

	// Analyze all fields and build field type map
	for field in node.children {
		if field.kind != .record_field {
			a.error('Expected record field, got ${field.kind}', node.position)
			return error('Expected record field, got ${field.kind}')
		}

		field_name := field.value
		field_type_node := field.children[0]

		// Check if field has default value
		if field.children.len > 1 {
			// Field has default value
			default_value := a.analyze_node(field.children[1])!
			default_type := a.type_table.get_type(default_value.id) or {
				ast.Type{
					name:   'unknown'
					params: []
				}
			}

			// Get field type
			mut field_type := ast.Type{}
			if field_type_node.value != '' {
				// Has explicit type - use extract_type_from_annotation to handle parameterized types
				field_type = a.extract_type_from_annotation(field_type_node) or {
					ast.Type{
						name:   field_type_node.value
						params: []
					}
				}

				// Validate that default value type matches field type
				if !unify_with_records(field_type, default_type, &a.type_table) {
					a.error('Type mismatch in default value for field ${field_name}: expected ${field_type}, got ${default_type}',
						node.position)
					return error('Type mismatch in default value for field ${field_name}: expected ${field_type}, got ${default_type}')
				}
			} else {
				// Infer type from default value, but use generic type for record fields
				field_type = ast.Type{
					name:   default_type.name
					params: default_type.params
					// Don't copy specialized_value - use generic type for record fields
				}
			}

			field_types[field_name] = field_type
			field_defaults[field_name] = default_value
		} else {
			// Field without default value
			if field_type_node.value == '' {
				a.error('Field ${field_name} must have a type or default value', node.position)
				return error('Field ${field_name} must have a type or default value')
			}

			// Create field type - use extract_type_from_annotation to handle parameterized types
			field_type := a.extract_type_from_annotation(field_type_node) or {
				ast.Type{
					name:   field_type_node.value
					params: []
				}
			}
			field_types[field_name] = field_type
		}
	}

	// Register record type in global type table with defaults
	a.type_table.register_record_type(record_name, field_types, field_defaults)

	// Create record type for this node
	record_type := ast.Type{
		name:   record_name
		params: []
	}
	a.type_table.assign_type(node.id, record_type)

	return node
}

fn (mut a Analyzer) analyze_record_literal(node ast.Node) !ast.Node {
	record_name := node.value

	// Check if record type exists
	record_type_info := a.type_table.get_record_type(record_name) or {
		a.error('Undefined record type: ${record_name}', node.position)
		return error('Undefined record type: ${record_name}')
	}

	// Mark record as used when creating a literal
	a.type_table.mark_type_used(record_name)

	mut analyzed_fields := []ast.Node{}
	mut provided_fields := map[string]bool{}

	// Analyze all provided field values and validate types using HM unification
	for field in node.children {
		field_name := field.value
		field_value := a.analyze_node(field.children[0])!
		provided_fields[field_name] = true

		// Get expected field type
		expected_type := a.type_table.get_field_type(record_name, field_name) or {
			// Get available fields for suggestion
			record_type := a.type_table.get_record_type(record_name) or {
				a.error('Unknown field ${field_name} in record ${record_name}', node.position)
				return error('Unknown field ${field_name} in record ${record_name}')
			}
			available_fields := record_type.fields.keys()
			suggestion := 'Available fields: ${available_fields.join(', ')}'
			a.error_with_suggestion('Unknown field ${field_name} in record ${record_name}',
				node.position, suggestion)
			return error('Unknown field ${field_name} in record ${record_name}')
		}

		// Get actual field value type
		actual_type := a.type_table.get_type(field_value.id) or {
			ast.Type{
				name:   'unknown'
				params: []
			}
		}

		// Use HM unification to validate type compatibility
		if !unify_with_records(expected_type, actual_type, &a.type_table) {
			a.error('Type mismatch in field ${field_name}: expected ${expected_type}, got ${actual_type}',
				node.position)
			return error('Type mismatch in field ${field_name}: expected ${expected_type}, got ${actual_type}')
		}

		analyzed_fields << ast.Node{
			id:       field.id
			kind:     field.kind
			value:    field_name
			children: [field_value]
			position: field.position
		}
	}

	// Add default values for fields that were not provided
	for field_name, _ in record_type_info.fields {
		if !provided_fields[field_name] {
			// Check if this field has a default value
			if default_value := a.type_table.get_field_default(record_name, field_name) {
				// Create a field node with the default value
				default_field := ast.Node{
					id:       a.type_table.generate_id()
					kind:     .identifier
					value:    field_name
					children: [default_value]
					position: node.position
				}
				analyzed_fields << default_field
			}
		}
	}

	// Create record type using HM type inference
	record_type_result := ast.Type{
		name:   record_name
		params: []
	}
	a.type_table.assign_type(node.id, record_type_result)

	return ast.Node{
		id:       node.id
		kind:     node.kind
		value:    node.value
		children: analyzed_fields
		position: node.position
	}
}

fn (mut a Analyzer) analyze_record_access(node ast.Node) !ast.Node {
	if node.children.len != 1 {
		return error('Record access must have exactly one child (record expression)')
	}

	record_expr := a.analyze_node(node.children[0])!
	field_name := node.value

	// Get record type
	record_type := a.type_table.get_type(record_expr.id) or {
		a.error('Cannot determine type of record expression', node.position)
		return error('Cannot determine type of record expression')
	}

	// Check if it's actually a record type
	if record_type.name == 'unknown' {
		a.error('Cannot access field ${field_name} on unknown type', node.position)
		return error('Cannot access field ${field_name} on unknown type')
	}

	// Get field type from record definition
	field_type := a.type_table.get_field_type(record_type.name, field_name) or {
		// Check if the record type is actually a record or just an identifier
		if record_type.name == 'identifier' || record_type.name == 'unknown' {
			a.error_with_suggestion('Cannot access field ${field_name} on undefined variable',
				node.position, 'Define the variable first or use a record instance')
			return error('Cannot access field ${field_name} on undefined variable')
		} else {
			// Get available fields for suggestion
			record_type_info := a.type_table.get_record_type(record_type.name) or {
				a.error('Unknown field ${field_name} in record ${record_type.name}', node.position)
				return error('Unknown field ${field_name} in record ${record_type.name}')
			}
			available_fields := record_type_info.fields.keys()
			suggestion := 'Available fields: ${available_fields.join(', ')}'
			a.error_with_suggestion('Unknown field ${field_name} in record ${record_type.name}',
				node.position, suggestion)
			return error('Unknown field ${field_name} in record ${record_type.name}')
		}
	}

	// Mark record as used when accessing its fields
	a.type_table.mark_type_used(record_type.name)

	// Assign the field type to this node
	a.type_table.assign_type(node.id, field_type)

	return ast.Node{
		id:       node.id
		kind:     node.kind
		value:    node.value
		children: [record_expr]
		position: node.position
	}
}

fn (mut a Analyzer) analyze_record_update(node ast.Node) !ast.Node {
	if node.children.len != 3 {
		return error('Record update must have exactly 3 children (record, field_name, field_value)')
	}

	record_expr := a.analyze_node(node.children[0])!
	field_name_node := node.children[1]
	field_value := a.analyze_node(node.children[2])!

	// Get record type using HM type inference
	record_type := a.type_table.get_type(record_expr.id) or {
		a.error('Cannot determine type of record expression', node.position)
		return error('Cannot determine type of record expression')
	}

	// Check if it's actually a record type
	if record_type.name == 'unknown' {
		a.error('Cannot update field on unknown type', node.position)
		return error('Cannot update field on unknown type')
	}

	field_name := field_name_node.value

	// Check if field exists in record
	expected_field_type := a.type_table.get_field_type(record_type.name, field_name) or {
		a.error('Unknown field ${field_name} in record ${record_type.name}', node.position)
		return error('Unknown field ${field_name} in record ${record_type.name}')
	}

	// Get actual field value type
	actual_field_type := a.type_table.get_type(field_value.id) or {
		ast.Type{
			name:   'unknown'
			params: []
		}
	}

	// Use HM unification to validate type compatibility
	if !unify_with_records(expected_field_type, actual_field_type, &a.type_table) {
		a.error('Type mismatch in field update ${field_name}: expected ${expected_field_type}, got ${actual_field_type}',
			node.position)
		return error('Type mismatch in field update ${field_name}: expected ${expected_field_type}, got ${actual_field_type}')
	}

	// Result type is the same as the original record (HM type preservation)
	a.type_table.assign_type(node.id, record_type)

	return ast.Node{
		id:       node.id
		kind:     node.kind
		value:    node.value
		children: [record_expr, field_name_node, field_value]
		position: node.position
	}
}

// Function analysis functions
fn (mut a Analyzer) analyze_function_definition(node ast.Node) !ast.Node {
	// Validate that function definitions are only allowed in global scope
	if a.current_env > 0 {
		// Supervisor and worker contexts are treated as global scope since they generate separate files
		current_scope_name := a.type_envs[a.current_env].scope_name
		is_otp_context := current_scope_name.starts_with('supervisor_')
			|| current_scope_name.starts_with('worker_')

		if !is_otp_context {
			a.error('Function definitions are only allowed in global scope, not inside functions',
				node.position)
			return error('Function definitions are only allowed in global scope, not inside functions')
		}
	}

	function_name := node.value
	parameters_node := node.children[0]
	body := node.children[1]
	return_type_node := node.children[2]

	mut parameters := []ast.Type{}
	mut parameter_names := []string{}

	// Create a new environment for this function
	a.type_envs << new_type_env(function_name)
	a.current_env = a.type_envs.len - 1

	// Analyze parameters and add them to the function's environment
	for param in parameters_node.children {
		param_name := param.value
		param_type := if param.children.len > 0 {
			a.extract_type_from_annotation(param.children[0])!
		} else {
			ast.Type{
				name:   'any'
				params: []
			}
		}

		parameters << param_type
		parameter_names << param_name

		// Add parameter to the function's environment
		a.bind(param_name, TypeScheme{
			quantified_vars: []
			body:            param_type
		})
	}

	// Analyze return type
	mut return_type := if return_type_node.children.len > 0 {
		a.extract_type_from_annotation(return_type_node)!
	} else {
		ast.Type{
			name:   'any'
			params: []
		}
	}

	// Analyze function body
	mut analyzed_body := a.analyze_node(body)!

	// Se o corpo é um block com múltiplas heads, analise cada head primeiro
	if analyzed_body.kind == .block {
		// Check if this has function heads
		has_function_heads := analyzed_body.children.len > 0
			&& analyzed_body.children[0].kind == .function

		if has_function_heads {
			// For multiple heads, infer return type from the first head's body
			if analyzed_body.children.len > 0 {
				first_head := analyzed_body.children[0]
				if first_head.kind == .function && first_head.children.len > 1 {
					head_body := first_head.children[1]
					head_body_type := a.type_table.get_type(head_body.id) or {
						ast.Type{
							name:   'unknown'
							params: []
						}
					}
					return_type = head_body_type
				}
			}

			for head in analyzed_body.children {
				if head.kind == .function {
					a.analyze_node(head)!
				}
			}
			// Agora analise o corpo novamente para propagar os tipos das variáveis
			analyzed_body = a.analyze_node(body)!
		}
	}

	// Para funções com operadores aritméticos, force parâmetros para 'integer'
	if body.kind == .function_caller && body.value in ['+', '-', '*', '/'] {
		for i, _ in parameter_names {
			if parameters[i].name == 'any' {
				parameters[i] = ast.Type{
					name:   'integer'
					params: []
				}
			}
		}
	}

	// Infer return type from body if not specified
	if return_type.name == 'any' {
		body_type := a.type_table.get_type(analyzed_body.id) or {
			ast.Type{
				name:   'unknown'
				params: []
			}
		}
		return_type = body_type
	}

	// Register function type in global type table
	a.type_table.register_function_type(function_name, parameters, return_type)

	// Create function type for this node - use the actual return type, not 'function'
	a.type_table.assign_type(node.id, return_type)

	// Restore the previous environment
	a.current_env = 0

	return node
}

fn (mut a Analyzer) analyze_function_head(node ast.Node) !ast.Node {
	pattern := a.analyze_node(node.children[0])!

	// Register pattern variables in the current function's environment BEFORE analyzing body
	if pattern.kind == .identifier {
		// Simple variable pattern
		a.bind(pattern.value, TypeScheme{
			quantified_vars: []
			body:            ast.Type{
				name:   'any'
				params: []
			}
		})
	} else if pattern.kind == .variable_ref {
		// Variable reference pattern
		a.bind(pattern.value, TypeScheme{
			quantified_vars: []
			body:            ast.Type{
				name:   'any'
				params: []
			}
		})
	} else if pattern.kind == .tuple_literal {
		// Tuple pattern (multiple arguments)
		for arg in pattern.children {
			if arg.kind == .identifier {
				a.bind(arg.value, TypeScheme{
					quantified_vars: []
					body:            ast.Type{
						name:   'any'
						params: []
					}
				})
			}
		}
	}

	body := a.analyze_node(node.children[1])!
	return_type_node := node.children[2]

	// Analyze return type
	mut return_type := if return_type_node.children.len > 0 {
		a.extract_type_from_annotation(return_type_node)!
	} else {
		ast.Type{
			name:   'any'
			params: []
		}
	}

	// Infer return type from body if not specified
	if return_type.name == 'any' {
		body_type := a.type_table.get_type(body.id) or {
			ast.Type{
				name:   'unknown'
				params: []
			}
		}
		return_type = body_type
	}

	// Check for unused variables in function head scope
	a.check_unused_variables()!

	return ast.Node{
		id:       node.id
		kind:     node.kind
		value:    node.value
		children: [pattern, body, return_type_node]
		position: node.position
	}
}

fn (mut a Analyzer) analyze_function_call(node ast.Node) !ast.Node {
	function_name := node.value
	mut analyzed_args := []ast.Node{}

	// Analyze arguments
	for arg in node.children {
		analyzed_arg := a.analyze_node(arg)!
		analyzed_args << analyzed_arg
	}

	// Try to get function type - first check if it's a variable (parameter)
	mut function_type := FunctionType{}

	// Check if it's a variable in scope (e.g., function parameter)
	if scheme := a.lookup(function_name) {
		// It's a variable, check if it's a function type
		if scheme.body.name == 'function' || scheme.body.name == 'any' {
			// Create a generic function type for variables of function type
			function_type = FunctionType{
				name:        function_name
				parameters:  []ast.Type{} // Will be inferred
				return_type: ast.Type{
					name:   'any'
					params: []
				}
				heads:       []
			}
		} else {
			a.error('Variable ${function_name} is not a function', node.position)
			return error('Variable ${function_name} is not a function')
		}
	} else {
		// Try as named function
		function_type = a.type_table.get_function_type(function_name) or {
			a.error('Unknown function: ${function_name}', node.position)
			return error('Unknown function: ${function_name}')
		}
	}

	// For function variables (parameters), skip specific argument validation
	// and let HM inference handle it
	if function_type.parameters.len > 0 {
		// Validate argument types using HM unification
		if analyzed_args.len != function_type.parameters.len {
			a.error('Function ${function_name} expects ${function_type.parameters.len} arguments, got ${analyzed_args.len}',
				node.position)
			return error('Function ${function_name} expects ${function_type.parameters.len} arguments, got ${analyzed_args.len}')
		}
	}

	// Only validate types for functions with known parameters
	if function_type.parameters.len > 0 {
		for i, arg in analyzed_args {
			expected_type := function_type.parameters[i]
			actual_type := a.type_table.get_type(arg.id) or {
				ast.Type{
					name:   'unknown'
					params: []
				}
			}

			if !unify_with_functions(expected_type, actual_type, &a.type_table) {
				a.error('Type mismatch in parameter ${i}: expected ${expected_type}, got ${actual_type}',
					node.position)
				return error('Type mismatch in parameter ${i}: expected ${expected_type}, got ${actual_type}')
			}
		}
	}

	// Try to infer a more specific return type based on arguments
	mut return_type := function_type.return_type

	// Special handling for list concatenation operator
	if function_name == '++' && analyzed_args.len == 2 {
		left_type := a.type_table.get_type(analyzed_args[0].id) or {
			ast.Type{
				name:   'unknown'
				params: []
			}
		}
		right_type := a.type_table.get_type(analyzed_args[1].id) or {
			ast.Type{
				name:   'unknown'
				params: []
			}
		}

		// If both arguments are lists, try to unify their element types
		if left_type.name == 'list' && right_type.name == 'list' && left_type.params.len == 1
			&& right_type.params.len == 1 {
			left_elem_type := left_type.params[0]
			right_elem_type := right_type.params[0]

			// Unify element types
			unified_elem_type := a.unify_types(left_elem_type, right_elem_type)

			// Create more specific list type
			return_type = ast.Type{
				name:   'list'
				params: [unified_elem_type]
			}
		}
	}

	// Special handling for setelement function
	if function_name == 'setelement' && analyzed_args.len == 3 {
		// setelement(index, tuple, new_value) returns a tuple with the same structure
		tuple_type := a.type_table.get_type(analyzed_args[1].id) or {
			ast.Type{
				name:   'unknown'
				params: []
			}
		}

		// If the second argument is a tuple, return tuple() instead of {any()}
		if tuple_type.name == 'tuple' {
			return_type = ast.Type{
				name:   'tuple'
				params: []
			}
		}
	}

	// Assign return type to this node
	a.type_table.assign_type(node.id, return_type)

	return ast.Node{
		id:       node.id
		kind:     node.kind
		value:    node.value
		children: analyzed_args
		position: node.position
	}
}

fn (mut a Analyzer) analyze_parameter(node ast.Node) !ast.Node {
	// Parameters are analyzed in the context of function definitions
	return node
}

fn (mut a Analyzer) analyze_block_expression(node ast.Node) !ast.Node {
	mut analyzed_expressions := []ast.Node{}

	// Analyze all expressions in the block
	for expr in node.children {
		analyzed_expr := a.analyze_node(expr)!
		analyzed_expressions << analyzed_expr
	}

	// The type of a block is the type of its last expression
	if analyzed_expressions.len > 0 {
		last_expr := analyzed_expressions[analyzed_expressions.len - 1]
		last_type := a.type_table.get_type(last_expr.id) or {
			ast.Type{
				name:   'unknown'
				params: []
			}
		}
		a.type_table.assign_type(node.id, last_type)
	}

	return ast.Node{
		id:       node.id
		kind:     node.kind
		value:    node.value
		children: analyzed_expressions
		position: node.position
	}
}

fn (a Analyzer) collect_types_from_union(typ ast.Type, mut all_types []ast.Type, mut seen_types map[string]bool) {
	if typ.name == 'union' {
		// If it's a union, recursively collect all its types
		for param in typ.params {
			a.collect_types_from_union(param, mut all_types, mut seen_types)
		}
	} else {
		// If it's not a union, add it if we haven't seen it
		if !seen_types[typ.name] {
			seen_types[typ.name] = true
			all_types << typ
		}
	}
}

fn (a Analyzer) types_are_identical(t1 ast.Type, t2 ast.Type) bool {
	// Check if two types are completely identical
	if t1.name != t2.name {
		return false
	}

	// Check specialized values (for atom types) - handle optional strings properly
	t1_spec := t1.specialized_value or { '' }
	t2_spec := t2.specialized_value or { '' }
	if t1_spec != t2_spec {
		return false
	}

	// Check parameter count
	if t1.params.len != t2.params.len {
		return false
	}

	// Check all parameters recursively
	for i in 0 .. t1.params.len {
		if !a.types_are_identical(t1.params[i], t2.params[i]) {
			return false
		}
	}

	return true
}

fn (a Analyzer) unify_types(t1 ast.Type, t2 ast.Type) ast.Type {
	// If both types are the same and not union (but not lists, which need parameter checking), return the type
	if t1.name == t2.name && t1.name != 'union' && t1.name != 'list' {
		return t1
	}

	// Special case for lists: if one is list(any) and other is list(specific), prefer the specific one
	if t1.name == 'list' && t2.name == 'list' && t1.params.len == 1 && t2.params.len == 1 {
		elem1 := t1.params[0]
		elem2 := t2.params[0]

		// If one element type is 'any' and the other is specific, use the specific one
		if elem1.name == 'any' && elem2.name != 'any' {
			return t2 // Return list with specific element type
		}
		if elem2.name == 'any' && elem1.name != 'any' {
			return t1 // Return list with specific element type
		}

		// If both have specific element types, unify them
		if elem1.name != 'any' && elem2.name != 'any' {
			unified_elem := a.unify_types(elem1, elem2)
			return ast.Type{
				name:   'list'
				params: [unified_elem]
			}
		}
	}

	// If one type is 'any', return 'any' (any is the most general type)
	if t1.name == 'any' || t2.name == 'any' {
		return ast.Type{
			name:   'any'
			params: []
		}
	}

	// Special case: if we have string and integer, create a union type
	// This is common in pattern matching functions
	if (t1.name == 'string' && t2.name == 'integer')
		|| (t1.name == 'integer' && t2.name == 'string') {
		return ast.Type{
			name:   'union'
			params: [t1, t2]
		}
	}

	// Handle union types intelligently
	if t1.name == 'union' || t2.name == 'union' {
		mut all_types := []ast.Type{}

		// Collect all types from t1
		if t1.name == 'union' {
			all_types << t1.params
		} else {
			all_types << t1
		}

		// Collect all types from t2
		if t2.name == 'union' {
			all_types << t2.params
		} else {
			all_types << t2
		}

		// Remove duplicates and create new union
		mut unique_types := []ast.Type{}

		for typ in all_types {
			// Check if this exact type is already in unique_types
			mut is_duplicate := false
			for existing in unique_types {
				if a.types_are_identical(typ, existing) {
					is_duplicate = true
					break
				}
			}
			if !is_duplicate {
				unique_types << typ
			}
		}

		// If we only have one unique type, return it directly
		if unique_types.len == 1 {
			return unique_types[0]
		}

		// Otherwise, create a union with all unique types
		return ast.Type{
			name:   'union'
			params: unique_types
		}
	}

	// For other different concrete types, create a union type
	if t1.name != t2.name {
		return ast.Type{
			name:   'union'
			params: [t1, t2]
		}
	}

	// Fallback to any for complex cases
	return ast.Type{
		name:   'any'
		params: []
	}
}

// New analysis functions for additional functionality

fn (mut a Analyzer) analyze_function_parameter(node ast.Node) !ast.Node {
	mut param_type := ast.Type{
		name:   'any' // Use 'any' instead of 'unknown' for better compatibility
		params: []
	}

	// Check if parameter has type annotation (same logic as normal functions)
	if node.children.len > 0 {
		param_type = a.extract_type_from_annotation(node.children[0])!
	}

	a.type_table.assign_type(node.id, param_type)
	return node
}

fn (mut a Analyzer) analyze_lambda_expression(node ast.Node) !ast.Node {
	if node.children.len < 1 {
		a.error('Lambda expression must have body', node.position)
		return error('Invalid lambda expression')
	}

	// Create new type environment for lambda scope
	a.enter_scope('lambda')
	defer { a.exit_scope() }

	// Check for self-references in lambda body (recursion not allowed in anonymous functions)
	a.check_lambda_recursion(node)

	// Analyze parameters (all children except the last one, which is the body)
	params := if node.children.len > 1 {
		node.children[0..node.children.len - 1]
	} else {
		[]ast.Node{}
	}
	mut param_types := []ast.Type{}
	for param in params {
		_ := a.analyze_node(param)!
		param_type := a.type_table.get_type(param.id) or {
			ast.Type{
				name:   'any' // Use 'any' for better compatibility
				params: []
			}
		}
		param_types << param_type

		// Bind parameter in environment
		scheme := TypeScheme{
			quantified_vars: []
			body:            param_type
		}
		a.bind(param.value, scheme)
	}

	// Analyze body
	body := node.children[node.children.len - 1]
	analyzed_body := a.analyze_node(body)!
	body_type := a.type_table.get_type(body.id) or {
		ast.Type{
			name:   'unknown'
			params: []
		}
	}

	// Lambda type is function type: (param_types...) -> body_type
	mut all_params := param_types.clone()
	all_params << body_type
	lambda_type := ast.Type{
		name:   'function'
		params: all_params
	}
	a.type_table.assign_type(node.id, lambda_type)

	mut analyzed_children := []ast.Node{}
	for param in params {
		analyzed_children << a.analyze_node(param)!
	}
	analyzed_children << analyzed_body

	return ast.Node{
		...node
		children: analyzed_children
	}
}

// Check for self-references in lambda expressions (recursion not allowed)
fn (mut a Analyzer) check_lambda_recursion(lambda_node ast.Node) {
	// Get the body of the lambda (last child)
	if lambda_node.children.len == 0 {
		return
	}

	body := lambda_node.children[lambda_node.children.len - 1]
	mut visited := map[string]bool{}
	a.check_node_for_recursion(body, mut visited)
}

// Recursively check a node for self-references
fn (mut a Analyzer) check_node_for_recursion(node ast.Node, mut visited map[string]bool) {
	// Check if this node is a function call to a variable
	if node.kind == .function_caller && node.value in visited {
		a.error('Recursion not allowed in anonymous functions: ${node.value}', node.position)
		return
	}

	// Mark this node as visited if it's a variable reference
	if node.kind == .identifier {
		visited[node.value] = true
	}

	// Recursively check all children
	for child in node.children {
		a.check_node_for_recursion(child, mut visited)
	}
}

fn (mut a Analyzer) analyze_case_expression(node ast.Node) !ast.Node {
	if node.children.len < 2 {
		a.error('Case expression must have expression and clauses', node.position)
		return error('Invalid case expression')
	}

	// Analyze the expression being matched
	expr := node.children[0]
	analyzed_expr := a.analyze_node(expr)!
	expr_type := a.type_table.get_type(expr.id) or {
		ast.Type{
			name:   'unknown'
			params: []
		}
	}

	// Analyze all clauses
	clauses := node.children[1..]
	mut analyzed_clauses := []ast.Node{}
	mut result_types := []ast.Type{}

	for clause in clauses {
		analyzed_clause := a.analyze_case_clause_with_type(clause, expr_type)!
		analyzed_clauses << analyzed_clause

		clause_type := a.type_table.get_type(clause.id) or {
			ast.Type{
				name:   'unknown'
				params: []
			}
		}
		result_types << clause_type
	}

	// Unify all clause result types
	mut case_type := if result_types.len > 0 {
		result_types[0]
	} else {
		ast.Type{
			name:   'any'
			params: []
		}
	}
	for i in 1 .. result_types.len {
		case_type = a.unify_types(case_type, result_types[i])
	}

	a.type_table.assign_type(node.id, case_type)

	mut analyzed_children := []ast.Node{}
	analyzed_children << analyzed_expr
	analyzed_children << analyzed_clauses

	return ast.Node{
		...node
		children: analyzed_children
	}
}

fn (mut a Analyzer) analyze_case_clause(node ast.Node) !ast.Node {
	return a.analyze_case_clause_with_type(node, ast.Type{ name: 'unknown', params: [] })
}

fn (mut a Analyzer) analyze_case_clause_with_type(node ast.Node, match_type ast.Type) !ast.Node {
	if node.children.len < 2 || node.children.len > 3 {
		a.error('Case clause must have pattern and body, optionally with guard', node.position)
		return error('Invalid case clause')
	}

	// Create new scope for pattern variables
	a.enter_scope('case_clause')
	defer { a.exit_scope() }

	// Analyze pattern with expected type
	pattern := node.children[0]
	analyzed_pattern := a.analyze_pattern_with_type(pattern, match_type)!

	// Analyze body
	body := node.children[1]
	analyzed_body := a.analyze_node(body)!
	body_type := a.type_table.get_type(body.id) or {
		ast.Type{
			name:   'unknown'
			params: []
		}
	}

	a.type_table.assign_type(node.id, body_type)

	mut children := [analyzed_pattern, analyzed_body]

	// If there's a guard (3rd child), analyze it
	if node.children.len == 3 {
		guard := node.children[2]
		analyzed_guard := a.analyze_node(guard)!
		// Store guard as third child
		children << analyzed_guard
	}

	return ast.Node{
		...node
		children: children
	}
}

fn (mut a Analyzer) analyze_pattern_match(node ast.Node) !ast.Node {
	if node.children.len != 1 {
		a.error('Pattern match must have one pattern', node.position)
		return error('Invalid pattern match')
	}

	pattern := node.children[0]
	analyzed_pattern := a.analyze_node(pattern)!
	pattern_type := a.type_table.get_type(pattern.id) or {
		ast.Type{
			name:   'unknown'
			params: []
		}
	}

	a.type_table.assign_type(node.id, pattern_type)

	return ast.Node{
		...node
		children: [analyzed_pattern]
	}
}

fn (mut a Analyzer) analyze_pattern_with_type(node ast.Node, expected_type ast.Type) !ast.Node {
	match node.kind {
		.identifier, .variable_ref {
			// Variable pattern - bind to environment
			scheme := TypeScheme{
				quantified_vars: []
				body:            expected_type
			}
			a.bind_with_position(node.value, scheme, node.position)
			a.type_table.assign_type(node.id, expected_type)
			return node
		}
		.list_literal {
			// List pattern - elements should match list element type
			mut element_type := ast.Type{
				name:   'any'
				params: []
			}
			if expected_type.name == 'list' && expected_type.params.len > 0 {
				element_type = expected_type.params[0]
			}

			mut analyzed_children := []ast.Node{}
			for child in node.children {
				analyzed_child := a.analyze_pattern_with_type(child, element_type)!
				analyzed_children << analyzed_child
			}

			a.type_table.assign_type(node.id, expected_type)
			return ast.Node{
				...node
				children: analyzed_children
			}
		}
		.list_cons {
			// List cons pattern [head | tail]
			mut element_type := ast.Type{
				name:   'any'
				params: []
			}
			if expected_type.name == 'list' && expected_type.params.len > 0 {
				element_type = expected_type.params[0]
			}

			head := node.children[0]
			tail := node.children[1]

			analyzed_head := a.analyze_pattern_with_type(head, element_type)!
			analyzed_tail := a.analyze_pattern_with_type(tail, expected_type)! // tail is also a list

			a.type_table.assign_type(node.id, expected_type)
			return ast.Node{
				...node
				children: [analyzed_head, analyzed_tail]
			}
		}
		.tuple_literal {
			// Tuple pattern - elements should match tuple element types
			mut analyzed_children := []ast.Node{}
			for child in node.children {
				// For simplicity, assume all tuple elements have 'any' type
				// (proper tuple type inference would be more complex)
				child_type := ast.Type{
					name:   'any'
					params: []
				}
				analyzed_child := a.analyze_pattern_with_type(child, child_type)!
				analyzed_children << analyzed_child
			}

			a.type_table.assign_type(node.id, expected_type)
			return ast.Node{
				...node
				children: analyzed_children
			}
		}
		.record_literal {
			// Record pattern - analyze field patterns
			record_name := node.value

			// Mark record as used when used in patterns
			a.type_table.mark_type_used(record_name)

			mut analyzed_children := []ast.Node{}
			for field in node.children {
				// Each field is an identifier node with the field name,
				// and the field value (variable to bind) is in field.children[0]
				if field.children.len > 0 {
					analyzed_field_value := a.analyze_pattern_with_type(field.children[0],
						ast.Type{
						name:   'any'
						params: []
					})!
					analyzed_children << ast.Node{
						...field
						children: [analyzed_field_value]
					}
				} else {
					analyzed_children << field
				}
			}

			a.type_table.assign_type(node.id, expected_type)
			return ast.Node{
				...node
				children: analyzed_children
			}
		}
		.map_literal {
			// Map pattern - analyze key-value patterns
			mut analyzed_children := []ast.Node{}
			for i := 0; i < node.children.len; i += 2 {
				// Keys are typically literals or atoms (not variables in pattern context)
				key := node.children[i]
				analyzed_key := a.analyze_node(key)!
				analyzed_children << analyzed_key

				// Values can be variables that should be bound
				if i + 1 < node.children.len {
					value := node.children[i + 1]
					analyzed_value := a.analyze_pattern_with_type(value, ast.Type{
						name:   'any'
						params: []
					})!
					analyzed_children << analyzed_value
				}
			}

			a.type_table.assign_type(node.id, expected_type)
			return ast.Node{
				...node
				children: analyzed_children
			}
		}
		.binary_pattern {
			// Binary pattern - analyze segments in pattern context
			mut analyzed_children := []ast.Node{}
			for child in node.children {
				analyzed_child := a.analyze_binary_segment_pattern(child)!
				analyzed_children << analyzed_child
			}

			// Binary patterns should match binary type
			binary_type := ast.Type{
				name:   'binary'
				params: []
			}
			a.type_table.assign_type(node.id, binary_type)
			return ast.Node{
				...node
				children: analyzed_children
			}
		}
		.block {
			// Block pattern (for function arguments) - analyze each child as pattern
			mut analyzed_children := []ast.Node{}
			for child in node.children {
				analyzed_child := a.analyze_pattern_with_type(child, ast.Type{
					name:   'any'
					params: []
				})!
				analyzed_children << analyzed_child
			}

			a.type_table.assign_type(node.id, expected_type)
			return ast.Node{
				...node
				children: analyzed_children
			}
		}
		else {
			// Literal patterns
			return a.analyze_node(node)
		}
	}
}

fn (mut a Analyzer) analyze_pattern_binding(node ast.Node) !ast.Node {
	if node.children.len != 2 {
		a.error('Pattern binding must have pattern and expression', node.position)
		return error('Invalid pattern binding')
	}

	// Analyze expression first to get its type
	expr := node.children[1]
	analyzed_expr := a.analyze_node(expr)!
	expr_type := a.type_table.get_type(expr.id) or {
		ast.Type{
			name:   'unknown'
			params: []
		}
	}

	// Analyze pattern with expression type
	pattern := node.children[0]

	analyzed_pattern := a.analyze_pattern_with_type(pattern, expr_type)!

	a.type_table.assign_type(node.id, expr_type)

	return ast.Node{
		...node
		children: [analyzed_pattern, analyzed_expr]
	}
}

fn (mut a Analyzer) analyze_type_alias(node ast.Node) !ast.Node {
	if node.children.len != 1 {
		a.error('Type alias must have one type definition', node.position)
		return error('Invalid type alias')
	}

	type_def := node.children[0]
	// Build the alias type directly from the type expression node
	alias_type := a.type_node_to_type(type_def)

	// Register the custom type in the type table
	a.type_table.register_custom_type(node.value, alias_type)

	// Bind alias in the environment for later use
	scheme := TypeScheme{
		quantified_vars: []
		body:            alias_type
	}
	a.bind(node.value, scheme)

	// Assign type to the alias node for reference
	a.type_table.assign_type(node.id, alias_type)

	// Still analyze the child for consistency in traversal
	analyzed_type_def := a.analyze_node(type_def)!

	return ast.Node{
		...node
		children: [analyzed_type_def]
	}
}

fn (mut a Analyzer) analyze_type_annotation(node ast.Node) !ast.Node {
	if node.children.len != 1 {
		a.error('Type annotation must have one type', node.position)
		return error('Invalid type annotation')
	}

	type_node := node.children[0]
	// Convert the type expression to a concrete type and assign it
	type_info := a.type_node_to_type(type_node)
	// Store on both the child and the annotation node ids
	a.type_table.assign_type(type_node.id, type_info)
	a.type_table.assign_type(node.id, type_info)

	// Analyze child for traversal consistency
	analyzed_type_node := a.analyze_node(type_node)!

	return ast.Node{
		...node
		children: [analyzed_type_node]
	}
}

// register_pattern_variables extracts variables from complex patterns and registers them
fn (mut a Analyzer) register_pattern_variables(pattern ast.Node) {
	// Set context to pattern when analyzing pattern variables
	old_context := a.analysis_context
	a.analysis_context = .pattern
	defer { a.analysis_context = old_context }
	match pattern.kind {
		.identifier, .variable_ref {
			// Simple variable pattern - in pattern context, both identifier and variable_ref represent variable binding
			arg_type := if pattern.children.len > 0 && pattern.children[0].kind == .identifier {
				a.extract_type_from_annotation(pattern.children[0]) or {
					ast.Type{
						name:   'any'
						params: []
					}
				}
			} else {
				ast.Type{
					name:   'any'
					params: []
				}
			}

			a.bind_with_position(pattern.value, TypeScheme{
				quantified_vars: []
				body:            arg_type
			}, pattern.position)
		}
		.list_cons {
			// List cons pattern [head | tail]
			if pattern.children.len >= 2 {
				head := pattern.children[0]
				tail := pattern.children[1]

				// First, determine the element type from the head
				mut element_type := ast.Type{
					name:   'any'
					params: []
				}

				// Handle both direct identifiers and parentheses-wrapped identifiers for head
				mut actual_head := head
				if head.kind == .parentheses && head.children.len > 0 {
					actual_head = head.children[0]
				}

				if actual_head.kind == .identifier {
					if actual_head.children.len > 0 && actual_head.children[0].kind == .identifier {
						// Head has type annotation
						element_type = a.extract_type_from_annotation(actual_head.children[0]) or {
							ast.Type{
								name:   'any'
								params: []
							}
						}
					}
					// Register head variable
					a.bind_with_position(actual_head.value, TypeScheme{
						quantified_vars: []
						body:            element_type
					}, actual_head.position)
				} else {
					// Head is not a simple identifier, recurse into it
					a.register_pattern_variables(head)
				}

				// Register tail variable (should be a list of the same element type as head)
				if tail.kind == .identifier || tail.kind == .variable_ref {
					// If tail has explicit type annotation, use it; otherwise use list of head element type
					tail_type := if tail.children.len > 0 && tail.children[0].kind == .identifier {
						a.extract_type_from_annotation(tail.children[0]) or {
							ast.Type{
								name:   'list'
								params: [element_type]
							}
						}
					} else {
						ast.Type{
							name:   'list'
							params: [element_type]
						}
					}

					a.bind_with_position(tail.value, TypeScheme{
						quantified_vars: []
						body:            tail_type
					}, tail.position)
				} else {
					// Tail is not a simple identifier, recurse into it
					a.register_pattern_variables(tail)
				}
			}
		}
		.list_literal {
			// List literal pattern [x, y, z]
			for elem in pattern.children {
				a.register_pattern_variables(elem)
			}
		}
		.tuple_literal {
			// Tuple pattern {x, y, z}
			for _, elem in pattern.children {
				a.register_pattern_variables(elem)
			}
		}
		.map_literal {
			// Map pattern %{key: value, key2: value2}
			for i := 0; i < pattern.children.len; i += 2 {
				// Keys are typically literals (don't register as variables)
				// Values can be variables that should be registered
				if i + 1 < pattern.children.len {
					value := pattern.children[i + 1]
					a.register_pattern_variables(value)
				}
			}
		}
		.parentheses {
			// Parentheses pattern (pattern) - recurse into the inner pattern
			if pattern.children.len > 0 {
				a.register_pattern_variables(pattern.children[0])
			}
		}
		.record_literal {
			// Record pattern User{name: _name, age: age} - register variables in field values
			for field in pattern.children {
				// Each field is an identifier node with the field name,
				// and the field value (variable to bind) is in field.children[0]
				if field.children.len > 0 {
					a.register_pattern_variables(field.children[0])
				}
			}
		}
		else {
			// For literals (integers, strings, etc.), no variables to register
		}
	}
}

// ============ Task 11: Control Flow Analysis ============

fn (mut a Analyzer) analyze_if_expr(node ast.Node) !ast.Node {
	if node.children.len < 2 {
		a.error('If expression must have at least condition and then branch', node.position)
		return error('Invalid if expression')
	}

	// Analyze condition
	condition := a.analyze_node(node.children[0])!

	// Analyze then branch
	then_branch := a.analyze_node(node.children[1])!
	then_type := a.type_table.get_type(then_branch.id) or {
		ast.Type{
			name:   'any'
			params: []
		}
	}

	mut result_type := then_type
	mut analyzed_children := [condition, then_branch]

	// Analyze else branch if present
	if node.children.len > 2 {
		else_branch := a.analyze_node(node.children[2])!
		else_type := a.type_table.get_type(else_branch.id) or {
			ast.Type{
				name:   'any'
				params: []
			}
		}

		// Unify then and else types
		result_type = if then_type.name == else_type.name {
			then_type
		} else {
			ast.Type{
				name:   'any'
				params: []
			}
		}
		analyzed_children << else_branch
	}

	analyzed_node := ast.Node{
		...node
		children: analyzed_children
	}

	a.type_table.assign_type(analyzed_node.id, result_type)
	return analyzed_node
}

fn (mut a Analyzer) analyze_with_expr(node ast.Node) !ast.Node {
	if node.children.len < 2 {
		a.error('With expression must have at least one clause and body', node.position)
		return error('Invalid with expression')
	}

	// Check if this is old format (3 children: pattern, expr, body) or new format (multiple clauses + body)
	if node.children.len == 3 && node.children[0].kind != .pattern_match {
		// Old format: single pattern, expr, body
		return a.analyze_with_expr_single(node)
	}

	// New format: multiple clauses + body (+ optional else)
	return a.analyze_with_expr_multi(node)
}

fn (mut a Analyzer) analyze_with_expr_single(node ast.Node) !ast.Node {
	// Analyze expression first
	expr := a.analyze_node(node.children[1])!

	// Enter scope for pattern binding
	a.enter_scope('with_scope')
	defer { a.exit_scope() }

	// Register pattern variables first (from original node)
	a.register_pattern_variables(node.children[0])

	// Then analyze pattern in pattern context
	pattern := a.with_context(.pattern, fn [mut a, node] (mut analyzer Analyzer) !ast.Node {
		return analyzer.analyze_node(node.children[0])
	})!

	// Analyze body
	body := a.analyze_node(node.children[2])!
	body_type := a.type_table.get_type(body.id) or {
		ast.Type{
			name:   'any'
			params: []
		}
	}

	mut result_type := body_type
	mut analyzed_children := [pattern, expr, body]

	// Analyze else body if present
	if node.children.len > 3 {
		else_body := a.analyze_node(node.children[3])!
		else_type := a.type_table.get_type(else_body.id) or {
			ast.Type{
				name:   'any'
				params: []
			}
		}

		result_type = if body_type.name == else_type.name {
			body_type
		} else {
			ast.Type{
				name:   'any'
				params: []
			}
		}
		analyzed_children << else_body
	}

	analyzed_node := ast.Node{
		...node
		children: analyzed_children
	}

	a.type_table.assign_type(analyzed_node.id, result_type)
	return analyzed_node
}

fn (mut a Analyzer) analyze_with_expr_multi(node ast.Node) !ast.Node {
	// Find where body starts (after all pattern_match clauses)
	mut body_index := 0
	for i, child in node.children {
		if child.kind != .pattern_match {
			body_index = i
			break
		}
	}

	if body_index == 0 {
		a.error('With expression must have at least one clause', node.position)
		return error('Invalid with expression')
	}

	// Enter scope for all pattern bindings
	a.enter_scope('with_scope')
	defer { a.exit_scope() }

	// Register variables from all clauses first
	for i in 0 .. body_index {
		clause := node.children[i]
		if clause.kind == .pattern_match && clause.children.len >= 1 {
			a.register_pattern_variables(clause.children[0])
		}
	}

	// Analyze all clauses
	mut analyzed_clauses := []ast.Node{}
	for i in 0 .. body_index {
		clause := node.children[i]
		if clause.kind == .pattern_match && clause.children.len >= 2 {
			// Analyze expression
			analyzed_expr := a.analyze_node(clause.children[1])!

			// Analyze pattern
			analyzed_pattern := a.with_context(.pattern, fn [mut a, clause] (mut analyzer Analyzer) !ast.Node {
				return analyzer.analyze_node(clause.children[0])
			})!

			analyzed_clause := ast.Node{
				...clause
				children: [analyzed_pattern, analyzed_expr]
			}
			analyzed_clauses << analyzed_clause
		}
	}

	// Analyze body
	body := a.analyze_node(node.children[body_index])!
	body_type := a.type_table.get_type(body.id) or {
		ast.Type{
			name:   'any'
			params: []
		}
	}

	mut result_type := body_type
	mut analyzed_children := analyzed_clauses.clone()
	analyzed_children << body

	// Analyze else body if present
	if node.children.len > body_index + 1 {
		else_body := a.analyze_node(node.children[body_index + 1])!
		else_type := a.type_table.get_type(else_body.id) or {
			ast.Type{
				name:   'any'
				params: []
			}
		}

		result_type = if body_type.name == else_type.name {
			body_type
		} else {
			ast.Type{
				name:   'any'
				params: []
			}
		}
		analyzed_children << else_body
	}

	analyzed_node := ast.Node{
		...node
		children: analyzed_children
	}

	a.type_table.assign_type(analyzed_node.id, result_type)
	return analyzed_node
}

fn (mut a Analyzer) analyze_match_expr(node ast.Node) !ast.Node {
	if node.children.len < 2 {
		a.error('Match expression must have pattern and expression', node.position)
		return error('Invalid match expression')
	}

	// Analyze expression
	expr := a.analyze_node(node.children[1])!

	// Enter scope for pattern binding and continuation
	a.enter_scope('match_scope')
	defer { a.exit_scope() }

	// Register pattern variables first (from original node)
	a.register_pattern_variables(node.children[0])

	// Analyze pattern in pattern context
	pattern := a.with_context(.pattern, fn [mut a, node] (mut analyzer Analyzer) !ast.Node {
		return analyzer.analyze_node(node.children[0])
	})!

	mut analyzed_children := [pattern, expr]

	// Analyze rescue body if present (child 2)
	if node.children.len > 2 && node.children[2].kind != .block {
		rescue_body := a.analyze_node(node.children[2])!
		analyzed_children << rescue_body
	} else if node.children.len > 2 && node.children[2].kind == .block {
		// This is a rescue block with error pattern and body
		rescue_block := node.children[2]
		if rescue_block.children.len >= 2 {
			// First child is error pattern, register it as a variable
			error_pattern := rescue_block.children[0]
			a.register_pattern_variables(error_pattern)

			// Analyze the rescue block normally
			rescue_body := a.analyze_node(rescue_block)!
			analyzed_children << rescue_body
		}
	}

	// Analyze continuation body if present (child 2 or 3 depending on rescue)
	mut continuation_index := 2
	if node.children.len > 2 && node.children[2].kind != .block {
		continuation_index = 3
	}

	if node.children.len > continuation_index {
		continuation_body := a.analyze_node(node.children[continuation_index])!
		analyzed_children << continuation_body
	}

	analyzed_node := ast.Node{
		...node
		children: analyzed_children
	}

	// Match expressions return the type of the continuation or the matched value
	mut result_type := ast.Type{
		name:   'any'
		params: []
	}

	if continuation_index < analyzed_children.len {
		// Has continuation - return continuation type
		continuation := analyzed_children[continuation_index]
		result_type = a.type_table.get_type(continuation.id) or {
			ast.Type{
				name:   'any'
				params: []
			}
		}
	} else {
		// No continuation - return matched value type
		result_type = a.type_table.get_type(expr.id) or {
			ast.Type{
				name:   'any'
				params: []
			}
		}
	}

	a.type_table.assign_type(analyzed_node.id, result_type)
	return analyzed_node
}

// ============ Task 11: Concurrency Analysis ============

fn (mut a Analyzer) analyze_spawn_expr(node ast.Node) !ast.Node {
	if node.children.len != 1 {
		a.error('Spawn expression must have one argument', node.position)
		return error('Invalid spawn expression')
	}

	// Analyze function expression
	func_expr := a.analyze_node(node.children[0])!

	analyzed_node := ast.Node{
		...node
		children: [func_expr]
	}

	// Spawn returns a PID (process identifier)
	pid_type := ast.Type{
		name:   'pid'
		params: []
	}
	a.type_table.assign_type(analyzed_node.id, pid_type)
	return analyzed_node
}

fn (mut a Analyzer) analyze_send_expr(node ast.Node) !ast.Node {
	if node.children.len != 2 {
		a.error('Send expression must have target and message', node.position)
		return error('Invalid send expression')
	}

	// Analyze target and message
	target := a.analyze_node(node.children[0])!
	message := a.analyze_node(node.children[1])!

	analyzed_node := ast.Node{
		...node
		children: [target, message]
	}

	// Send returns the message that was sent
	message_type := a.type_table.get_type(message.id) or {
		ast.Type{
			name:   'any'
			params: []
		}
	}
	a.type_table.assign_type(analyzed_node.id, message_type)
	return analyzed_node
}

fn (mut a Analyzer) analyze_receive_expr(node ast.Node) !ast.Node {
	mut analyzed_children := []ast.Node{}

	for child in node.children {
		analyzed_child := a.analyze_node(child)!
		analyzed_children << analyzed_child
	}

	analyzed_node := ast.Node{
		...node
		children: analyzed_children
	}

	// Receive returns any type based on the patterns
	receive_type := ast.Type{
		name:   'any'
		params: []
	}
	a.type_table.assign_type(analyzed_node.id, receive_type)
	return analyzed_node
}

fn (mut a Analyzer) analyze_supervisor_def(node ast.Node) !ast.Node {
	if node.children.len != 1 {
		a.error('Supervisor definition must have body', node.position)
		return error('Invalid supervisor definition')
	}

	// Enter scope for supervisor and prepare OTP context function set
	ctx_name := 'supervisor_${node.value}'
	a.enter_scope(ctx_name)
	defer { a.exit_scope() }
	local_funcs := a.collect_local_function_names(node.children[0])
	a.otp_contexts << OtpContextInfo{
		name:            ctx_name
		local_functions: local_funcs
	}
	defer {
		if a.otp_contexts.len > 0 {
			a.otp_contexts = a.otp_contexts[..a.otp_contexts.len - 1]
		}
	}

	body := a.analyze_node(node.children[0])!

	analyzed_node := ast.Node{
		...node
		children: [body]
	}

	// Supervisors return supervisor specification
	supervisor_type := ast.Type{
		name:   'supervisor_spec'
		params: []
	}
	a.type_table.assign_type(analyzed_node.id, supervisor_type)
	return analyzed_node
}

fn (mut a Analyzer) analyze_worker_def(node ast.Node) !ast.Node {
	if node.children.len != 1 {
		a.error('Worker definition must have body', node.position)
		return error('Invalid worker definition')
	}

	// Enter scope for worker and prepare OTP context function set
	ctx_name := 'worker_${node.value}'
	a.enter_scope(ctx_name)
	defer { a.exit_scope() }
	local_funcs := a.collect_local_function_names(node.children[0])
	a.otp_contexts << OtpContextInfo{
		name:            ctx_name
		local_functions: local_funcs
	}
	defer {
		if a.otp_contexts.len > 0 {
			a.otp_contexts = a.otp_contexts[..a.otp_contexts.len - 1]
		}
	}

	body := a.analyze_node(node.children[0])!

	analyzed_node := ast.Node{
		...node
		children: [body]
	}

	// Workers return worker specification
	worker_type := ast.Type{
		name:   'worker_spec'
		params: []
	}
	a.type_table.assign_type(analyzed_node.id, worker_type)
	return analyzed_node
}

// ============ Task 11: Binaries Analysis ============

fn (mut a Analyzer) analyze_binary_literal(node ast.Node) !ast.Node {
	mut analyzed_children := []ast.Node{}

	for child in node.children {
		analyzed_child := a.analyze_node(child)!
		analyzed_children << analyzed_child
	}

	analyzed_node := ast.Node{
		...node
		children: analyzed_children
	}

	// Binary literals are of type binary
	binary_type := ast.Type{
		name:   'binary'
		params: []
	}
	a.type_table.assign_type(analyzed_node.id, binary_type)
	return analyzed_node
}

fn (mut a Analyzer) analyze_binary_pattern(node ast.Node) !ast.Node {
	return a.analyze_binary_literal(node) // Same analysis as literal
}

// Analyze binary segment in pattern context - variables are declarations, not references
fn (mut a Analyzer) analyze_binary_segment_pattern(node ast.Node) !ast.Node {
	if node.kind != .binary_segment {
		return a.analyze_pattern_with_type(node, ast.Type{ name: 'any', params: [] })
	}

	mut analyzed_children := []ast.Node{}

	// The first child is the variable being declared
	if node.children.len > 0 {
		variable := node.children[0]
		if variable.kind == .identifier {
			// This is a new variable declaration in the pattern
			scheme := TypeScheme{
				quantified_vars: []
				body:            ast.Type{
					name:   'any'
					params: []
				}
			}
			a.bind(variable.value, scheme)
			a.type_table.assign_type(variable.id, ast.Type{ name: 'any', params: [] })
			analyzed_children << variable
		} else {
			// Not an identifier, analyze normally
			analyzed_child := a.analyze_pattern_with_type(variable, ast.Type{
				name:   'any'
				params: []
			})!
			analyzed_children << analyzed_child
		}
	}

	// Analyze size expression if present (second child)
	if node.children.len > 1 {
		size_child := a.analyze_node(node.children[1])!
		analyzed_children << size_child
	}

	// Validate binary segment modifiers (same as before)
	if node.value.len > 0 {
		options := node.value.split(',')
		for option in options {
			if !is_valid_binary_modifier(option.trim_space()) {
				a.error('Invalid binary segment modifier: ${option}', node.position)
				return node
			}
		}
	}

	analyzed_node := ast.Node{
		...node
		children: analyzed_children
	}

	// Binary segments are of type any (can be integers, binaries, etc.)
	segment_type := ast.Type{
		name:   'any'
		params: []
	}
	a.type_table.assign_type(analyzed_node.id, segment_type)
	return analyzed_node
}

fn (mut a Analyzer) analyze_binary_segment(node ast.Node) !ast.Node {
	mut analyzed_children := []ast.Node{}

	for child in node.children {
		analyzed_child := a.analyze_node(child)!
		analyzed_children << analyzed_child
	}

	// Validate binary segment modifiers
	if node.value.len > 0 {
		options := node.value.split(',')
		for option in options {
			if !is_valid_binary_modifier(option.trim_space()) {
				a.error('Invalid binary segment modifier: ${option}', node.position)
				return node
			}
		}
	}

	analyzed_node := ast.Node{
		...node
		children: analyzed_children
	}

	// Binary segments are of type any (can be integers, binaries, etc.)
	segment_type := ast.Type{
		name:   'any'
		params: []
	}
	a.type_table.assign_type(analyzed_node.id, segment_type)
	return analyzed_node
}

// Validate binary segment modifiers according to Erlang specification
fn is_valid_binary_modifier(modifier string) bool {
	// Type modifiers
	type_modifiers := ['integer', 'float', 'binary', 'bytes', 'bitstring', 'bits']

	// Endianness modifiers
	endianness_modifiers := ['big', 'little', 'native']

	// Sign modifiers
	sign_modifiers := ['signed', 'unsigned']

	// Unit modifier (format: unit:N where N is 1-256)
	if modifier.starts_with('unit:') {
		unit_part := modifier[5..]
		unit_val := unit_part.int()
		return unit_val >= 1 && unit_val <= 256
	}

	// Size modifier (just numbers, handled separately)
	if modifier.bytes().all(it.is_digit()) {
		return true
	}

	// Check against known modifier lists
	return modifier in type_modifiers || modifier in endianness_modifiers
		|| modifier in sign_modifiers
}

// ============ Task 11: Custom Types Analysis ============

// Analyze type expressions (different from regular expressions)
fn (mut a Analyzer) analyze_type_expression_node(node ast.Node) !ast.Node {
	return match node.kind {
		.atom, .identifier, .list_literal, .tuple_literal {
			// These are valid in type expressions - just return them as-is
			node
		}
		.union_type {
			// Recursively analyze union type variants
			mut analyzed_children := []ast.Node{}
			for child in node.children {
				analyzed_child := a.analyze_type_expression_node(child)!
				analyzed_children << analyzed_child
			}
			ast.Node{
				...node
				children: analyzed_children
			}
		}
		else {
			// For other node types, analyze normally but in type context
			old_context := a.analysis_context
			a.analysis_context = .expression
			defer { a.analysis_context = old_context }
			a.analyze_node(node)!
		}
	}
}

fn (mut a Analyzer) analyze_type_def(node ast.Node) !ast.Node {
	// Register the type definition
	mut analyzed_children := []ast.Node{}

	// For type definitions, we need to analyze children as type expressions, not regular expressions
	for child in node.children {
		analyzed_child := a.analyze_type_expression_node(child)!
		analyzed_children << analyzed_child
	}

	analyzed_node := ast.Node{
		...node
		children: analyzed_children
	}

	// Get the type definition from the first child (the type expression)
	if analyzed_children.len > 0 {
		// Convert the original (pre-analyzed) child node into a type
		base_type := a.type_node_to_type(node.children[0])

		// Extract base type name for registration (remove generic parameters)
		mut type_name_for_registration := node.value
		if node.value.contains('(') {
			type_name_for_registration = node.value.split('(')[0]
		}

		// Register the custom type in the type table
		a.type_table.register_custom_type(type_name_for_registration, base_type)
	}

	// Type definitions are metadata - no runtime type
	type_def_type := ast.Type{
		name:   'type_definition'
		params: []
	}
	a.type_table.assign_type(analyzed_node.id, type_def_type)
	return analyzed_node
}

fn (mut a Analyzer) analyze_union_type(node ast.Node) !ast.Node {
	mut analyzed_children := []ast.Node{}

	for child in node.children {
		analyzed_child := a.analyze_type_expression_node(child)!
		analyzed_children << analyzed_child
	}

	analyzed_node := ast.Node{
		...node
		children: analyzed_children
	}

	// Union types represent multiple possible types
	union_type := ast.Type{
		name:   'union'
		params: []
	}
	a.type_table.assign_type(analyzed_node.id, union_type)
	return analyzed_node
}

fn (mut a Analyzer) analyze_generic_type(node ast.Node) !ast.Node {
	mut analyzed_children := []ast.Node{}

	for child in node.children {
		analyzed_child := a.analyze_node(child)!
		analyzed_children << analyzed_child
	}

	analyzed_node := ast.Node{
		...node
		children: analyzed_children
	}

	// Generic types
	generic_type := ast.Type{
		name:   node.value
		params: analyzed_children.map(a.type_table.get_type(it.id) or {
			ast.Type{ name: 'any', params: [] }
		})
	}
	a.type_table.assign_type(analyzed_node.id, generic_type)
	return analyzed_node
}

fn (mut a Analyzer) analyze_opaque_type(node ast.Node) !ast.Node {
	mut analyzed_children := []ast.Node{}

	for child in node.children {
		analyzed_child := a.analyze_node(child)!
		analyzed_children << analyzed_child
	}

	analyzed_node := ast.Node{
		...node
		children: analyzed_children
	}

	// Get the base type definition from the first child (the type expression)
	if analyzed_children.len > 0 {
		// Convert the original (pre-analyzed) child node into a type
		base_type := a.type_node_to_type(node.children[0])
		// Register the custom type in the type table
		a.type_table.register_custom_type(node.value, base_type)
	}

	// Opaque types hide the underlying type
	opaque_type := ast.Type{
		name:   node.value
		params: []
	}
	a.type_table.assign_type(analyzed_node.id, opaque_type)
	return analyzed_node
}

fn (mut a Analyzer) analyze_nominal_type(node ast.Node) !ast.Node {
	mut analyzed_children := []ast.Node{}

	for child in node.children {
		analyzed_child := a.analyze_node(child)!
		analyzed_children << analyzed_child
	}

	analyzed_node := ast.Node{
		...node
		children: analyzed_children
	}

	// Get the base type definition from the first child (the type expression)
	if analyzed_children.len > 0 {
		// Convert the original (pre-analyzed) child node into a type
		base_type := a.type_node_to_type(node.children[0])
		// Register the custom type in the type table
		a.type_table.register_custom_type(node.value, base_type)
	}

	// Nominal types are distinct from their underlying type
	nominal_type := ast.Type{
		name:   node.value
		params: []
	}
	a.type_table.assign_type(analyzed_node.id, nominal_type)
	return analyzed_node
}

// ============ Task 11: Module System Analysis ============

fn (mut a Analyzer) analyze_deps_declaration(node ast.Node) !ast.Node {
	mut analyzed_children := []ast.Node{}

	for child in node.children {
		analyzed_child := a.analyze_node(child)!
		analyzed_children << analyzed_child
	}

	analyzed_node := ast.Node{
		...node
		children: analyzed_children
	}

	// Dependencies are metadata
	deps_type := ast.Type{
		name:   'dependencies'
		params: []
	}
	a.type_table.assign_type(analyzed_node.id, deps_type)
	return analyzed_node
}

fn (mut a Analyzer) analyze_application_config(node ast.Node) !ast.Node {
	mut analyzed_children := []ast.Node{}

	for child in node.children {
		analyzed_child := a.analyze_node(child)!
		analyzed_children << analyzed_child
	}

	analyzed_node := ast.Node{
		...node
		children: analyzed_children
	}

	// Application config is metadata
	app_type := ast.Type{
		name:   'application_config'
		params: []
	}
	a.type_table.assign_type(analyzed_node.id, app_type)
	return analyzed_node
}

fn (mut a Analyzer) analyze_import_statement(node ast.Node) !ast.Node {
	analyzed_node := ast.Node{
		...node
		children: []
	}

	// Import statements are metadata
	import_type := ast.Type{
		name:   'import'
		params: []
	}
	a.type_table.assign_type(analyzed_node.id, import_type)
	return analyzed_node
}

// ============ Task 11: Advanced Features Analysis ============

fn (mut a Analyzer) analyze_string_interpolation(node ast.Node) !ast.Node {
	mut analyzed_children := []ast.Node{}

	for child in node.children {
		analyzed_child := a.analyze_node(child)!
		analyzed_children << analyzed_child
	}

	analyzed_node := ast.Node{
		...node
		children: analyzed_children
	}

	// String interpolation results in a binary (string)
	string_type := ast.Type{
		name:   'binary'
		params: []
	}
	a.type_table.assign_type(analyzed_node.id, string_type)
	return analyzed_node
}

fn (mut a Analyzer) analyze_anonymous_function(node ast.Node) !ast.Node {
	if node.children.len == 0 {
		a.error('Anonymous function must have at least a body', node.position)
		return error('Invalid anonymous function')
	}

	// Enter scope for anonymous function
	a.enter_scope('anonymous_function')
	defer { a.exit_scope() }

	// Parameters are all children except the last; last is the body
	params := if node.children.len > 1 {
		node.children[0..node.children.len - 1]
	} else {
		[]ast.Node{}
	}

	mut analyzed_params := []ast.Node{}
	mut param_types := []ast.Type{}
	for param in params {
		analyzed_param := a.analyze_node(param)!
		analyzed_params << analyzed_param
		param_type := a.type_table.get_type(analyzed_param.id) or {
			ast.Type{
				name:   'any'
				params: []
			}
		}
		param_types << param_type
		// Bind parameter in environment
		scheme := TypeScheme{
			quantified_vars: []
			body:            param_type
		}
		a.bind(analyzed_param.value, scheme)
	}

	// Analyze body with parameters bound
	body := node.children[node.children.len - 1]
	analyzed_body := a.analyze_node(body)!

	// Rebuild children list
	mut analyzed_children := analyzed_params.clone()
	analyzed_children << analyzed_body

	analyzed_node := ast.Node{
		...node
		children: analyzed_children
	}

	// Anonymous functions are of type fun (keep it simple for now)
	fun_type := ast.Type{
		name:   'fun'
		params: []
	}
	a.type_table.assign_type(analyzed_node.id, fun_type)
	return analyzed_node
}

fn (mut a Analyzer) analyze_list_comprehension(node ast.Node) !ast.Node {
	if node.children.len < 3 {
		a.error('List comprehension must have variable, list, and body', node.position)
		return error('Invalid list comprehension')
	}

	// Children: [variable, list, body, condition?]
	var_node := node.children[0]
	list_node := node.children[1]
	body_node := node.children[2]

	// Analyze list expression first
	analyzed_list := a.analyze_node(list_node)!

	// Enter new scope for comprehension variable
	a.enter_scope('list_comprehension')
	defer { a.exit_scope() }

	// Bind comprehension variable with element type from list
	list_type := a.type_table.get_type(analyzed_list.id) or {
		ast.Type{
			name:   'list'
			params: [ast.Type{
				name:   'any'
				params: []
			}]
		}
	}

	element_type := if list_type.name == 'list' && list_type.params.len > 0 {
		list_type.params[0]
	} else {
		ast.Type{
			name:   'any'
			params: []
		}
	}

	a.bind(var_node.value, TypeScheme{
		quantified_vars: []
		body:            element_type
	})

	// Analyze variable node
	analyzed_var := a.analyze_node(var_node)!

	// Analyze condition if present
	mut analyzed_condition := ast.Node{}
	mut has_condition := false
	if node.children.len > 3 {
		analyzed_condition = a.analyze_node(node.children[3])!
		has_condition = true
	}

	// Analyze body expression
	analyzed_body := a.analyze_node(body_node)!

	// Build analyzed children
	mut analyzed_children := [analyzed_var, analyzed_list, analyzed_body]
	if has_condition {
		analyzed_children << analyzed_condition
	}

	analyzed_node := ast.Node{
		...node
		children: analyzed_children
	}

	// List comprehensions return a list of the body expression type
	body_type := a.type_table.get_type(analyzed_body.id) or {
		ast.Type{
			name:   'any'
			params: []
		}
	}

	list_result_type := ast.Type{
		name:   'list'
		params: [body_type]
	}
	a.type_table.assign_type(analyzed_node.id, list_result_type)
	return analyzed_node
}

fn (mut a Analyzer) analyze_directive(node ast.Node) !ast.Node {
	mut analyzed_children := []ast.Node{}

	for child in node.children {
		analyzed_child := a.analyze_node(child)!
		analyzed_children << analyzed_child
	}

	analyzed_node := ast.Node{
		...node
		children: analyzed_children
	}

	// Directives are metadata
	directive_type := ast.Type{
		name:   'directive'
		params: []
	}
	a.type_table.assign_type(analyzed_node.id, directive_type)
	return analyzed_node
}

fn (mut a Analyzer) analyze_test_block(node ast.Node) !ast.Node {
	if node.children.len != 1 {
		a.error('Test block must have body', node.position)
		return error('Invalid test block')
	}

	// Save current scope
	saved_env := a.current_env

	// Enter scope for test
	a.enter_scope('test_${node.value}')

	body := a.analyze_node(node.children[0])!

	// Ensure we exit scope completely
	a.exit_scope()
	a.current_env = saved_env

	analyzed_node := ast.Node{
		...node
		children: [body]
	}

	// Test blocks return test specification
	test_type := ast.Type{
		name:   'test_spec'
		params: []
	}
	a.type_table.assign_type(analyzed_node.id, test_type)
	return analyzed_node
}

fn (mut a Analyzer) analyze_lambda_call(node ast.Node) !ast.Node {
	if node.children.len < 1 {
		a.error('Lambda call must have lambda expression', node.position)
		return node
	}

	// Analyze lambda expression
	lambda := a.analyze_node(node.children[0])!
	mut analyzed_children := [lambda]

	// Analyze arguments
	for i in 1 .. node.children.len {
		arg := a.analyze_node(node.children[i])!
		analyzed_children << arg
	}

	// Try to infer return type from lambda expression
	lambda_type := a.type_table.get_type(lambda.id) or {
		ast.Type{
			name:   'any'
			params: []
		}
	}

	// If lambda is a function type, extract return type (last parameter)
	return_type := if lambda_type.name == 'function' && lambda_type.params.len > 0 {
		lambda_type.params[lambda_type.params.len - 1]
	} else {
		ast.Type{
			name:   'any'
			params: []
		}
	}

	a.type_table.assign_type(node.id, return_type)

	return ast.Node{
		...node
		children: analyzed_children
	}
}

fn (a Analyzer) collect_local_function_names(body ast.Node) map[string]bool {
	mut m := map[string]bool{}
	for child in body.children {
		if (child.kind == .function || child.kind == .private_function) && child.value != '' {
			m[child.value] = true
		}
	}
	return m
}
