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

pub fn (a Analyzer) lookup(name string) ?ast.Type {
	if env := a.type_envs[a.current_env] {
		if name in env.bindings {
			return env.bindings[name]
		}
	}
	return none
}

pub fn (mut a Analyzer) bind(name string, typ ast.Type) {
	a.type_envs[a.current_env].bindings[name] = typ
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

fn (mut a Analyzer) analyze_node(node ast.Node) !ast.Node {
	return match node.kind {
		.module {
			a.analyze_module(node)
		}
		.function {
			a.analyze_function(node)
		}
		.variable_binding {
			a.analyze_binding(node)
		}
		.variable_ref {
			a.analyze_variable_ref(node)
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
	a.type_envs << new_type_env(node.value)
	a.current_env = a.type_envs.len - 1

	if node.children.len != 1 {
		a.error('Function must have exactly one body expression', node.position)
		return error('Function must have exactly one body expression')
	}

	body := a.analyze_node(node.children[0])!

	body_type := a.type_table.get_type(body.id) or {
		ast.Type{
			name:   'unknown'
			params: []
		}
	}
	a.type_table.assign_type(node.id, body_type)

	a.current_env = 0
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
	value_node := a.analyze_node(node.children[0])!

	value_type := a.type_table.get_type(value_node.id) or {
		ast.Type{
			name:   'any'
			params: []
		}
	}

	// Check if variable is already defined in current function scope
	if _ := a.lookup(var_name) {
		a.error('Variable ${var_name} is already defined in this scope', node.position)
		return error('Variable already defined')
	}

	// Bind variable in current function's type environment
	a.bind(var_name, value_type)

	// Assign type to binding node
	a.type_table.assign_type(node.id, value_type)

	return node
}

fn (mut a Analyzer) analyze_variable_ref(node ast.Node) !ast.Node {
	var_name := node.value

	// Look up variable in current function's type environment
	if typ := a.lookup(var_name) {
		a.type_table.assign_type(node.id, typ)
		return node
	} else {
		a.error('Undefined variable: ${var_name}', node.position)
		return error('Undefined variable: ${var_name}')
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

	function_name := node.value // Nome da função (ex: "+", "*", ">")

	function_info := kernel.get_function_info(function_name) or {
		a.error('Unknown function: ${function_name}', node.position)
		return error('Unknown function')
	}

	mut analyzed_args := []ast.Node{}
	for arg in node.children {
		analyzed_arg := a.analyze_node(arg)!
		analyzed_args << analyzed_arg
	}

	if function_info.fixity == .infix && analyzed_args.len == 2 && function_info.signatures.len > 0 {
		expected_return := function_info.signatures[0].return_type
		if expected_return.name == 'list' {
			mut all_types := collect_list_types_rec(ast.Node{
				...node
				children: analyzed_args
			}, &a.type_table)
			mut unique_types := []ast.Type{}
			mut seen := map[string]bool{}
			for t in all_types {
				if !seen[t.name] {
					seen[t.name] = true
					unique_types << t
				}
			}
			final_type := if unique_types.len == 1 {
				unique_types[0]
			} else {
				ast.Type{
					name:   'union'
					params: unique_types
				}
			}
			a.type_table.assign_type(node.id, ast.Type{
				name:   'list'
				params: [
					final_type,
				]
			})
			return ast.Node{
				...node
				children: analyzed_args
			}
		}
	}

	expected_arity := function_info.signatures[0].parameters.len
	if analyzed_args.len != expected_arity {
		a.error('Function ${function_name}/${expected_arity} called with ${analyzed_args.len} arguments',
			node.position)
		return error('Invalid function arity')
	}

	match function_info.fixity {
		.infix {
			if analyzed_args.len != 2 {
				a.error('Infix operator ${function_name} requires exactly 2 arguments, got ${analyzed_args.len}',
					node.position)
				return error('Invalid number of arguments')
			}
		}
		.prefix {
			// Check if this is a multi-arg prefix function
			if a.is_multi_arg_prefix_function(function_name) {
				// Multi-arg prefix functions can have variable number of arguments
				// Just check that we have at least the minimum required
				min_args := function_info.signatures[0].parameters.len
				if analyzed_args.len < min_args {
					a.error('Function ${function_name} requires at least ${min_args} arguments, got ${analyzed_args.len}',
						node.position)
					return error('Invalid number of arguments')
				}
			} else {
				// Single-arg prefix functions
				if analyzed_args.len != 1 {
					a.error('Prefix operator ${function_name} requires exactly 1 argument, got ${analyzed_args.len}',
						node.position)
					return error('Invalid number of arguments')
				}
			}
		}
		.postfix {
			if analyzed_args.len != 1 {
				a.error('Postfix operator ${function_name} requires exactly 1 argument, got ${analyzed_args.len}',
					node.position)
				return error('Invalid number of arguments')
			}
		}
	}

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

		result_type := a.check_function_signatures(function_name, left_type, right_type,
			function_info.signatures) or {
			a.error('Invalid operator: ${function_name}(${left_type.name}, ${right_type.name})',
				node.position)
			return error('Type mismatch in function call')
		}

		a.type_table.assign_type(node.id, result_type)
	}

	if function_info.fixity == .prefix && analyzed_args.len == 1 {
		arg_type := a.type_table.get_type(analyzed_args[0].id) or {
			ast.Type{
				name:   'any'
				params: []
			}
		}

		result_type := a.check_prefix_function_signatures(function_name, arg_type, function_info.signatures) or {
			a.error('Invalid function: ${function_name}(${arg_type.name})', node.position)
			return error('Type mismatch in function call')
		}

		a.type_table.assign_type(node.id, result_type)
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
	multi_arg_prefix_functions := ['element', 'setelement']
	return function_name in multi_arg_prefix_functions
}
