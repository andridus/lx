module analysis

import ast

fn (mut a Analyzer) extract_return_type_function(node ast.Node) ast.Type {
	if node.children.len > 2 {
		return_type_node := node.children[2]
		if return_type_node.children.len > 0 {
			return a.extract_type_from_annotation(return_type_node) or {
				ast.Type{
					name:   'any'
					params: []
				}
			}
		}
	}
	return ast.Type{
		name:   'any'
		params: []
	}
}

// Pre-register function signature for forward reference resolution
fn (mut a Analyzer) preregister_function(node ast.Node) ! {
	args_block := node.children[0]
	body := node.children[1]
	has_function_heads := args_block.children.len == 0 && body.kind == .block
		&& body.children.len > 0 && body.children.all(it.kind == .function)
	function_name := node.value
	is_named_function := function_name != ''
	public := node.kind == .function
	// validations
	if node.kind != .function && node.kind != .private_function {
		return
	}
	if function_name == '' {
		return
	}
	if node.children.len < 2 {
		return
	}
	return_type := a.extract_return_type_function(node)

	mut heads := []FunctionHead{}
	if is_named_function {
		if has_function_heads {
			for head in body.children {
				if head.kind == .function {
					args, _ := a.parse_args(head.children[0])!
					heads << FunctionHead{
						patterns:    args
						return_type: return_type
						node_id:     head.id
					}
				}
			}
		} else {
			args, _ := a.parse_args(args_block)!
			heads = [
				FunctionHead{
					patterns:    args
					return_type: return_type
					node_id:     node.id
				},
			]
		}
	}
	a.type_table.register_function_type(function_name, heads, return_type, public)
}

fn (mut a Analyzer) analyze_function(node ast.Node) !ast.Node {
	args_block := node.children[0]
	body := node.children[1]
	function_name := node.value
	is_single_function := node.children.len == 2 && args_block.children.len > 0
	has_any_function_heads := body.children.len > 0 && body.children.any(it.kind == .function)
	has_function_heads := args_block.children.len == 0 && body.kind == .block
		&& body.children.len > 0 && body.children.all(it.kind == .function)
	is_named_function := function_name != ''
	public := node.kind == .function
	mut return_type_annotation := ast.Type{
		name:   'any'
		params: []
	}
	if is_named_function {
		a.current_function_name = function_name
		defer {
			a.current_function_name = ''
		}
	}
	if is_named_function && a.current_env > 0 {
		current_scope_name := a.type_envs[a.current_env].scope_name
		is_otp_context := current_scope_name.starts_with('supervisor_')
			|| current_scope_name.starts_with('worker_')

		if !is_otp_context {
			a.error('Function definitions are only allowed in global scope, not inside functions',
				node.position)
			return error('Function definitions are only allowed in global scope, not inside functions')
		}
	}

	if is_single_function && has_any_function_heads {
		a.error('Function ${function_name} cannot have both arguments in definition and multiple heads in body. Use either "def ${function_name}(args) do body end" or "def ${function_name} do (args) -> body end"',
			node.position)
		return error('Invalid function definition: cannot mix argument definition with multiple heads')
	}

	if node.children.len < 2 {
		a.error('Function must have args and body', node.position)
		return error('Function must have args and body')
	}
	if node.children.len == 3 && node.children[2].kind == .identifier {
		return_type_annotation = a.extract_type_from_annotation(node.children[2])!
	}

	// Create a new environment for this function (only for named functions)
	if is_named_function {
		a.type_envs << new_type_env(function_name)
		a.current_env = a.type_envs.len - 1
	}

	// Analyze args and add them to the function's environment
	mut return_type := a.extract_return_type_function(node)
	mut heads := []FunctionHead{}
	mut children := []ast.Node{}
	mut return_types := []ast.Type{}
	if has_function_heads {
		for head1 in body.children {
			if head1.kind == .function {
				parameters, _ := a.parse_args(head1.children[0])!
				hd := a.analyze_function(head1)!
				children << hd
				return_types << hd.type
				fn_head := FunctionHead{
					patterns:    parameters
					return_type: hd.type
					node_id:     head1.id
				}
				heads << fn_head
				a.type_table.update_function_head(a.current_function_name, head1.id, fn_head)
			}
		}
	} else {
		parameters, _ := a.parse_args(args_block)!
		children << args_block
		hd := a.analyze_node(body)!
		body_type := a.type_table.get_type(body.id) or {
			ast.Type{
				name:   'any'
				params: []
			}
		}
		children << hd
		return_types << body_type
		return_type = body_type
		heads = [
			FunctionHead{
				patterns:    parameters
				return_type: body_type
			},
		]
	}
	if is_named_function && (body.kind == .block && body.children.len == 0) {
		a.error('Function ${function_name} cannot have empty body. Functions must contain at least one expression',
			node.position)
		return error('Function cannot have empty body')
	}
	if return_type_annotation.name != 'any' {
		inferred_type := return_type
		annotated_type := return_type_annotation

		if !a.are_types_compatible(inferred_type, annotated_type) {
			a.error('Return type mismatch: function returns ${inferred_type.name} but annotated as ${annotated_type.name}',
				node.position)
			return error('Return type mismatch')
		}
		return_type = annotated_type
	}
	mut return_type_ := return_type
	if return_types.len > 1 {
		return_type_ = ast.Type{
			name:   'union'
			params: return_types
		}
	}
	if is_named_function {
		a.type_table.register_function_type(function_name, heads, return_type_, public)
		a.type_table.assign_type(node.id, return_type)
		a.check_unused_variables()!
		a.current_env = 0
	}

	a.type_table.assign_type(node.id, return_type_)
	return ast.Node{
		...node
		children: children
		type:     return_type_
	}
}

fn (mut a Analyzer) parse_args(args_block ast.Node) !([]ast.Type, []string) {
	mut args := []ast.Type{}
	mut arg_names := []string{}
	for arg in args_block.children {
		arg_type := a.type_node_to_type(arg)
		if arg.value == '' && arg.children.len > 0 {
			pattern := arg.children[0]
			a.register_pattern_variables(pattern)
			a.type_table.assign_type(arg.id, arg_type)
			args << arg_type
			arg_names << '_pattern'
			if arg.kind == .type_annotation {
				a.bind(arg.children[0].value, TypeScheme{
					quantified_vars: []
					body:            arg_type
				})
			}
		} else {
			a.register_pattern_variables(arg)
			arg_name := arg.value
			if arg.kind == .variable_ref {
				a.bind(arg_name, TypeScheme{
					quantified_vars: []
					body:            arg_type
				})
			}
			a.type_table.assign_type(arg.id, arg_type)
			args << arg_type
			arg_names << arg_name
		}
	}
	return args, arg_names
}
