module analysis

import ast
import errors
import kernel

pub struct Analyzer {
mut:
	error_reporter errors.ErrorReporter
	type_table     TypeTable
	type_envs      []TypeEnv
	current_env    int
}

pub fn (a Analyzer) lookup(name string) ?TypeScheme {
	if a.current_env >= 0 && a.current_env < a.type_envs.len {
		if env := a.type_envs[a.current_env] {
			if name in env.bindings {
				return env.bindings[name]
			}
		}
	}
	return none
}

pub fn (mut a Analyzer) bind(name string, scheme TypeScheme) {
	a.type_envs[a.current_env].bindings[name] = scheme
}

pub fn new_analyzer() Analyzer {
	return Analyzer{
		type_table:     new_type_table()
		type_envs:      [new_type_env('root')]
		error_reporter: errors.new_error_reporter()
	}
}

pub fn (mut a Analyzer) analyze(node ast.Node) !ast.Node {
	return a.analyze_node(node)
}

pub fn (a Analyzer) get_errors() []errors.Err {
	return a.error_reporter.all()
}

fn (mut a Analyzer) error(msg string, pos ast.Position) {
	a.error_reporter.report(.analysis, msg, pos)
}

fn (mut a Analyzer) error_with_suggestion(msg string, pos ast.Position, suggestion string) {
	a.error_reporter.report_with_suggestion(.analysis, msg, pos, suggestion)
}

fn (mut a Analyzer) analyze_node(node ast.Node) !ast.Node {
	return match node.kind {
		.module {
			a.analyze_module(node)
		}
		.function {
			// All functions are named functions (no anonymous functions)
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
		.integer, .float, .string, .boolean, .atom, .nil {
			a.analyze_literal(node)
		}
		.function_caller {
			a.analyze_function_caller(node)
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
		else {
			a.error('Unsupported node type: ${node.kind}', node.position)
			return error('Unsupported node type: ${node.kind}')
		}
	}
}

fn (mut a Analyzer) analyze_module(node ast.Node) !ast.Node {
	mut analyzed_children := []ast.Node{}

	mut function_names := map[string]bool{}

	for child in node.children {
		if child.kind == .function {
			func_name := child.value
			if func_name in function_names {
				a.error('Duplicate function name: ${func_name}', child.position)
			}
			function_names[func_name] = true
		}

		analyzed_child := a.analyze_node(child)!
		analyzed_children << analyzed_child
	}

	a.type_table.assign_type(node.id, ast.Type{
		name:   'module'
		params: []
	})

	return ast.Node{
		...node
		children: analyzed_children
	}
}

fn (mut a Analyzer) analyze_function(node ast.Node) !ast.Node {
	// Only check scope for named functions, not anonymous functions (heads)
	if node.value != '' && a.current_env > 0 {
		a.error('Function definitions are only allowed in global scope, not inside functions',
			node.position)
		return error('Function definitions are only allowed in global scope, not inside functions')
	}

	function_name := node.value

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
		return_type_annotation = a.analyze_type_annotation(node.children[2])!
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
		arg_name := arg.value
		arg_type := if arg.children.len > 0 {
			a.analyze_type_annotation(arg.children[0])!
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
			body: arg_type
		})
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
									&& actual_arg.children.len > 0 {
									// Has type annotation
									a.analyze_type_annotation(actual_arg.children[0])!
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
											&& arg.children.len > 0 {
											// Has type annotation
											a.analyze_type_annotation(arg.children[0])!
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
							if arg.kind == .identifier {
								arg_type := if arg.children.len > 0 {
									a.analyze_type_annotation(arg.children[0])!
								} else {
									ast.Type{
										name:   'any'
										params: []
									}
								}
								a.bind(arg.value, TypeScheme{
									quantified_vars: []
									body: arg_type
								})
							}
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
				mut unified_type := head_return_types[0]
				for i in 1 .. head_return_types.len {
					unified_type = a.unify_types(unified_type, head_return_types[i])
				}
				return_type = unified_type
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
			a.type_table.assign_type(node.id, ast.Type{
				name:   'integer'
				params: []
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
			a.type_table.assign_type(node.id, ast.Type{
				name:   'string'
				params: []
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
			a.type_table.assign_type(node.id, ast.Type{
				name:   'atom'
				params: []
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

	// Check for rebind in current scope
	if _ := a.lookup(var_name) {
		a.error('Variable "${var_name}" cannot be reassigned. Variables in LX are immutable.',
			node.position)
		return error('Variable "${var_name}" cannot be reassigned')
	}

	value_node := a.analyze_node(node.children[0])!

	value_type := a.type_table.get_type(value_node.id) or {
		ast.Type{
			name:   'any'
			params: []
		}
	}

	// Bind variable in current function's type environment
	a.bind(var_name, TypeScheme{
		quantified_vars: []
		body: value_type
	})

	// Assign type to binding node
	a.type_table.assign_type(node.id, value_type)

	return node
}

fn (mut a Analyzer) analyze_variable_ref(node ast.Node) !ast.Node {
	var_name := node.value

	// Look up variable in current function's type environment
	if scheme := a.lookup(var_name) {
		a.type_table.assign_type(node.id, scheme.body)
		return node
	} else {
		a.error('Undefined variable: ${var_name}', node.position)
		return error('Undefined variable: ${var_name}')
	}
}

fn (mut a Analyzer) analyze_identifier(node ast.Node) !ast.Node {
	// Identifiers should be treated as variable references
	var_name := node.value

	// Look up variable in current function's type environment
	if scheme := a.lookup(var_name) {
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
	mut analyzed_exprs := []ast.Node{}

	for expr in node.children {
		analyzed_expr := a.analyze_node(expr)!
		analyzed_exprs << analyzed_expr
	}

	// Type of block is type of last expression
	if analyzed_exprs.len > 0 {
		last_type := a.type_table.get_type(analyzed_exprs.last().id) or {
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
	if node.children.len < 1 {
		return error('Function call must have at least one argument')
	}

	function_name := node.value // Nome da função (ex: '+', '*', '>')

	function_info := kernel.get_function_info(function_name) or {
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

			// Tenta encontrar uma assinatura válida
			if return_type := a.check_function_signatures(function_name, left_type, right_type,
				function_info.signatures)
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
				// Has explicit type
				field_type = ast.Type{
					name:   field_type_node.value
					params: []
				}

				// Validate that default value type matches field type
				if !unify_with_records(field_type, default_type, &a.type_table) {
					a.error('Type mismatch in default value for field ${field_name}: expected ${field_type}, got ${default_type}',
						node.position)
					return error('Type mismatch in default value for field ${field_name}: expected ${field_type}, got ${default_type}')
				}
			} else {
				// Infer type from default value
				field_type = default_type
			}

			field_types[field_name] = field_type
			field_defaults[field_name] = default_value
		} else {
			// Field without default value
			if field_type_node.value == '' {
				a.error('Field ${field_name} must have a type or default value', node.position)
				return error('Field ${field_name} must have a type or default value')
			}

			// Create field type
			field_type := ast.Type{
				name:   field_type_node.value
				params: []
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
		a.error('Function definitions are only allowed in global scope, not inside functions',
			node.position)
		return error('Function definitions are only allowed in global scope, not inside functions')
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
			a.analyze_type_annotation(param.children[0])!
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
			body: param_type
		})
	}

	// Analyze return type
	mut return_type := if return_type_node.children.len > 0 {
		a.analyze_type_annotation(return_type_node)!
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
			body: ast.Type{ name: 'any', params: [] }
		})
	} else if pattern.kind == .variable_ref {
		// Variable reference pattern
		a.bind(pattern.value, TypeScheme{
			quantified_vars: []
			body: ast.Type{ name: 'any', params: [] }
		})
	} else if pattern.kind == .tuple_literal {
		// Tuple pattern (multiple arguments)
		for arg in pattern.children {
			if arg.kind == .identifier {
				a.bind(arg.value, TypeScheme{
					quantified_vars: []
					body: ast.Type{ name: 'any', params: [] }
				})
			}
		}
	}

	body := a.analyze_node(node.children[1])!
	return_type_node := node.children[2]

	// Analyze return type
	mut return_type := if return_type_node.children.len > 0 {
		a.analyze_type_annotation(return_type_node)!
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

	// Get function type
	function_type := a.type_table.get_function_type(function_name) or {
		a.error('Undefined function: ${function_name}', node.position)
		return error('Undefined function: ${function_name}')
	}

	// Validate argument types using HM unification
	if analyzed_args.len != function_type.parameters.len {
		a.error('Function ${function_name} expects ${function_type.parameters.len} arguments, got ${analyzed_args.len}',
			node.position)
		return error('Function ${function_name} expects ${function_type.parameters.len} arguments, got ${analyzed_args.len}')
	}

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

fn (mut a Analyzer) analyze_type_annotation(node ast.Node) !ast.Type {
	// Convert AST node to Type
	if node.value != '' {
		return ast.Type{
			name:   node.value
			params: []
		}
	} else {
		return ast.Type{
			name:   'any'
			params: []
		}
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

fn (a Analyzer) unify_types(t1 ast.Type, t2 ast.Type) ast.Type {
	// If both types are the same and not union, return the type
	if t1.name == t2.name && t1.name != 'union' {
		return t1
	}

	// If one type is 'any', return 'any' (any is the most general type)
	if t1.name == 'any' || t2.name == 'any' {
		return ast.Type{
			name:   'any'
			params: []
		}
	}

	// Collect all types from both arguments (flattening unions)
	mut all_types := []ast.Type{}
	mut seen_types := map[string]bool{}

	// Collect types from both arguments
	a.collect_types_from_union(t1, mut all_types, mut seen_types)
	a.collect_types_from_union(t2, mut all_types, mut seen_types)

	// If we have only one type, return it
	if all_types.len == 1 {
		return all_types[0]
	}

	// If we have multiple types, create a union
	return ast.Type{
		name:   'union'
		params: all_types
	}
}
