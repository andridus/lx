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
			name:   'unknown'
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
				name:   'unknown'
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

	// Obtém informações da função nativa
	function_info := kernel.get_function_info(function_name) or {
		a.error('Unknown function: ${function_name}', node.position)
		return error('Unknown function')
	}

	// Analisa todos os argumentos
	mut analyzed_args := []ast.Node{}
	for arg in node.children {
		analyzed_arg := a.analyze_node(arg)!
		analyzed_args << analyzed_arg
	}

	// Verifica aridade da função (número de argumentos)
	expected_arity := function_info.signatures[0].parameters.len
	if analyzed_args.len != expected_arity {
		a.error('Function ${function_name}/${expected_arity} called with ${analyzed_args.len} arguments',
			node.position)
		return error('Invalid function arity')
	}

	// Verifica número de argumentos baseado no fixity
	match function_info.fixity {
		.infix {
			if analyzed_args.len != 2 {
				a.error('Infix operator ${function_name} requires exactly 2 arguments, got ${analyzed_args.len}',
					node.position)
				return error('Invalid number of arguments')
			}
		}
		.prefix {
			if analyzed_args.len != 1 {
				a.error('Prefix operator ${function_name} requires exactly 1 argument, got ${analyzed_args.len}',
					node.position)
				return error('Invalid number of arguments')
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

	// Para operadores binários, verifica tipos dos argumentos
	if function_info.fixity == .infix && analyzed_args.len == 2 {
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

		// Verifica se os tipos correspondem a alguma das assinaturas da função
		result_type := a.check_function_signatures(function_name, left_type, right_type,
			function_info.signatures) or {
			a.error('Invalid operator: ${function_name}(${left_type.name}, ${right_type.name})',
				node.position)
			return error('Type mismatch in function call')
		}

		// Assign result type to function call node
		a.type_table.assign_type(node.id, result_type)
	}

	return ast.Node{
		...node
		children: analyzed_args
	}
}

fn (mut a Analyzer) check_function_signatures(function_name string, left_type ast.Type, right_type ast.Type, signatures []kernel.TypeSignature) !ast.Type {
	// Tenta encontrar uma assinatura que corresponda aos tipos dos argumentos
	for signature in signatures {
		if signature.parameters.len != 2 {
			continue // Pula assinaturas inválidas
		}

		expected_left := signature.parameters[0]
		expected_right := signature.parameters[1]

		// Verifica se os tipos correspondem
		if left_type.name == expected_left.name && right_type.name == expected_right.name {
			return signature.return_type
		}
	}

	return error('No matching signature found for function ${function_name}(${left_type.name}, ${right_type.name})')
}

fn (mut a Analyzer) analyze_parentheses(node ast.Node) !ast.Node {
	if node.children.len != 1 {
		return error('Parentheses must contain exactly one expression')
	}

	expr := a.analyze_node(node.children[0])!
	expr_type := a.type_table.get_type(expr.id) or {
		ast.Type{
			name:   'unknown'
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
