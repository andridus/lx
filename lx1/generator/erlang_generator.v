module generator

import ast
import strings
import analysis
import kernel

@[heap]
pub struct ErlangGenerator {
mut:
	output     strings.Builder
	errors     []string
	type_table &analysis.TypeTable = unsafe { nil }
	var_map    map[string]string // Maps original var names to unique Erlang names
	next_hash  int = 1
}

pub fn new_generator() ErlangGenerator {
	return ErlangGenerator{
		var_map: map[string]string{}
	}
}

pub fn (mut g ErlangGenerator) generate(node ast.Node) !string {
	g.output = strings.new_builder(1024)
	g.errors = []

	g.generate_node(node)!

	if g.errors.len > 0 {
		return error('Generation errors: ${g.errors.join(', ')}')
	}

	return g.output.str()
}

pub fn (mut g ErlangGenerator) generate_with_types(node ast.Node, type_table &analysis.TypeTable) !string {
	g.type_table = type_table
	return g.generate(node)
}

pub fn (g ErlangGenerator) get_errors() []string {
	return g.errors
}

fn (mut g ErlangGenerator) get_unique_var_name(original_name string) string {
	if original_name in g.var_map {
		return g.var_map[original_name]
	}

	// Capitalize the first letter for Erlang convention
	capitalized := original_name.to_upper()
	unique_name := '${capitalized}_${g.next_hash}'
	g.var_map[original_name] = unique_name
	g.next_hash++

	return unique_name
}

fn (mut g ErlangGenerator) error(msg string) {
	g.errors << 'Generation error: ${msg}'
}

fn (mut g ErlangGenerator) generate_node(node ast.Node) ! {
	match node.kind {
		.module {
			g.generate_module(node)!
		}
		.function {
			g.generate_function(node)!
		}
		.variable_binding {
			g.generate_binding(node)!
		}
		.variable_ref {
			g.generate_variable_ref(node)!
		}
		.identifier {
			g.generate_identifier(node)!
		}
		.block {
			g.generate_block(node)!
		}
		.integer, .float, .string, .boolean, .atom, .nil {
			g.generate_literal(node)!
		}
		.function_caller {
			g.generate_function_caller(node)!
		}
		.parentheses {
			g.generate_parentheses(node)!
		}
		.list_literal {
			g.generate_list_literal(node)!
		}
		.list_cons {
			g.generate_list_cons(node)!
		}
		.tuple_literal {
			g.generate_tuple_literal(node)!
		}
		.map_literal {
			g.generate_map_literal(node)!
		}
		.map_access {
			g.generate_map_access(node)!
		}
		.record_definition {
			g.generate_record_definition(node)!
		}
		.record_literal {
			g.generate_record_literal(node)!
		}
		.record_access {
			g.generate_record_access(node)!
		}
		.record_update {
			g.generate_record_update(node)!
		}
		.function_parameter {
			g.generate_function_parameter(node)!
		}
		.lambda_expression {
			g.generate_lambda_expression(node)!
		}
		.case_expression {
			g.generate_case_expression(node)!
		}
		.case_clause {
			g.generate_case_clause(node)!
		}
		.pattern_match {
			g.generate_pattern_match(node)!
		}
		.pattern_binding {
			g.generate_pattern_binding(node)!
		}
		.type_alias {
			g.generate_type_alias(node)!
		}
		.type_annotation {
			g.generate_type_annotation(node)!
		}
		// Skip directive_call nodes (they are filtered out during analysis)
		.directive_call {
			// Do nothing - directives are not generated in output
		}
		else {
			return error('Unsupported node type: ${node.kind}')
		}
	}
}

fn (mut g ErlangGenerator) generate_binding(node ast.Node) ! {
	if node.children.len >= 1 {
		// Generate variable name with unique hash
		original_name := node.value
		unique_name := g.get_unique_var_name(original_name)
		g.output.write_string('${unique_name} = ')

		// Generate value
		g.generate_node(node.children[0])!
	}
}

fn (mut g ErlangGenerator) generate_variable_ref(node ast.Node) ! {
	// Generate variable name with unique hash
	original_name := node.value
	unique_name := g.get_unique_var_name(original_name)
	g.output.write_string(unique_name)
}

fn (mut g ErlangGenerator) generate_identifier(node ast.Node) ! {
	// Generate identifier name with unique hash
	original_name := node.value
	unique_name := g.get_unique_var_name(original_name)
	g.output.write_string(unique_name)
}

fn (mut g ErlangGenerator) generate_block(node ast.Node) ! {
	if node.children.len == 0 {
		return
	}

	for i, child in node.children {
		if child.kind == .directive_call {
			continue
		}
		g.generate_node(child)!
		if i < node.children.len - 1 {
			g.output.write_string(',\n    ')
		}
	}
}

fn (mut g ErlangGenerator) generate_module(node ast.Node) ! {
	module_name := node.value
	g.output.write_string('-module(${module_name}).\n')

	// Collect function exports
	mut exports := []string{}
	for child in node.children {
		if child.kind == .function {
			// Calculate arity from args block or from heads
			mut arity := 0
			if child.children.len > 0 {
				args_block := child.children[0]
				if args_block.children.len > 0 {
					// Function has arguments in definition
					arity = args_block.children.len
				} else if child.children.len > 1 {
					// Check if this has multiple heads
					body := child.children[1]
					if body.kind == .block && body.children.len > 0
						&& body.children[0].kind == .function {
						// Multiple heads - get arity from first head
						first_head := body.children[0]
						if first_head.children.len > 0 {
							head_args := first_head.children[0]
							arity = head_args.children.len
						}
					}
				}
			}
			exports << '${child.value}/${arity}'
		}
	}

	if exports.len > 0 {
		g.output.write_string('-export([${exports.join(', ')}]).\n\n')
	}

	// Generate record definitions first
	for child in node.children {
		if child.kind == .record_definition {
			g.generate_record_definition(child)!
		}
	}

	// Generate function definitions
	for child in node.children {
		if child.kind == .function {
			g.generate_function(child)!
		}
	}
}

fn (mut g ErlangGenerator) generate_function(node ast.Node) ! {
	function_name := node.value

	// Get function type for spec generation
	if function_type := g.type_table.get_function_type(function_name) {
		g.output.write_string('-spec ${function_name}(')
		if function_type.parameters.len > 0 {
			for i, param in function_type.parameters {
				if i > 0 {
					g.output.write_string(', ')
				}
				g.output.write_string(type_to_erlang_spec(param))
			}
		}
		g.output.write_string(') -> ${type_to_erlang_spec(function_type.return_type)}.\n')
	}

	// Generate function body
	if node.children.len >= 2 {
		args_block := node.children[0]
		body := node.children[1]

		// Check if this is a multi-head function
		has_function_heads := body.kind == .block && body.children.len > 0
			&& body.children[0].kind == .function

		if has_function_heads {
			// Multi-head function - generate each head
			for i, head in body.children {
				if head.kind == .function {
					// Generate head arguments
					if head.children.len > 0 {
						head_args := head.children[0]
						g.output.write_string('${function_name}(')
						if head_args.kind == .block {
							for j, arg in head_args.children {
								if j > 0 {
									g.output.write_string(', ')
								}
								// Generate argument as variable with unique hash
								if arg.kind == .identifier {
									unique_name := g.get_unique_var_name(arg.value)
									g.output.write_string(unique_name)
								} else {
									g.generate_node(arg)!
								}
							}
						} else {
							// Single argument
							if head_args.kind == .identifier {
								unique_name := g.get_unique_var_name(head_args.value)
								g.output.write_string(unique_name)
							} else {
								g.generate_node(head_args)!
							}
						}
						g.output.write_string(') ->\n    ')

						// Generate head body
						if head.children.len > 1 {
							g.generate_node(head.children[1])!
						}

						if i < body.children.len - 1 {
							g.output.write_string(';\n')
						} else {
							g.output.write_string('.\n')
						}
					}
				}
			}
		} else {
			// Single function - generate normally
			g.generate_single_function(function_name, args_block, body)!
		}
	}
}

fn (mut g ErlangGenerator) generate_literal(node ast.Node) ! {
	match node.kind {
		.integer {
			g.output.write_string(node.value)
		}
		.float {
			g.output.write_string(node.value)
		}
		.string {
			escaped := g.escape_string(node.value)
			g.output.write_string('<<"${escaped}"/utf8>>')
		}
		.boolean {
			g.output.write_string(node.value)
		}
		.atom {
			g.output.write_string(node.value)
		}
		.nil {
			g.output.write_string('nil')
		}
		else {
			return error('Unknown literal type: ${node.kind}')
		}
	}
}

fn (g ErlangGenerator) escape_string(s string) string {
	return s.replace('\\', '\\\\').replace('"', '\\"').replace('\n', '\\n').replace('\t',
		'\\t').replace('\r', '\\r')
}

fn type_to_erlang_spec(t ast.Type) string {
	return match t.name {
		'union' {
			if t.params.len > 0 {
				union_types := t.params.map(type_to_erlang_spec).join(' | ')
				union_types
			} else {
				'any()'
			}
		}
		'integer' {
			'integer()'
		}
		'float' {
			'float()'
		}
		'string' {
			'binary()'
		}
		'boolean' {
			'boolean()'
		}
		'atom' {
			'atom()'
		}
		'nil' {
			'nil'
		}
		'module' {
			'atom()'
		}
		'any' {
			'any()'
		}
		'term' {
			'term()'
		}
		'list' {
			if t.params.len == 1 {
				'[' + type_to_erlang_spec(t.params[0]) + ']'
			} else {
				'list()'
			}
		}
		'tuple' {
			if t.params.len > 0 {
				elems := t.params.map(type_to_erlang_spec).join(', ')
				'{' + elems + '}'
			} else {
				'tuple()'
			}
		}
		'map' {
			if t.params.len == 2 {
				'#{' + type_to_erlang_spec(t.params[0]) + ' => ' +
					type_to_erlang_spec(t.params[1]) + '}'
			} else {
				'map()'
			}
		}
		else {
			// Check if this is a record type (should be converted to lowercase)
			if t.name.len > 0 && t.name[0].is_capital() {
				'#${t.name.to_lower()}{}'
			} else if t.name.len == 0 {
				'any()'
			} else {
				t.name + '()'
			}
		}
	}
}

fn (mut g ErlangGenerator) generate_function_caller(node ast.Node) ! {
	function_name := node.value

	// First, try to get function type from type table (user-defined functions)
	if _ := g.type_table.get_function_type(function_name) {
		g.output.write_string('${function_name}(')
		for i, arg in node.children {
			if i > 0 {
				g.output.write_string(', ')
			}
			g.generate_node(arg)!
		}
		g.output.write_string(')')
		return
	}

	// Second, try kernel for built-in functions
	if function_info := kernel.get_function_info(function_name) {
		match function_info.fixity {
			.prefix {
				// Use kernel template for prefix functions
				if function_info.gen.len == 0 {
					return error('No templates found for function: ${function_name}')
				}
				template := function_info.gen[0]['erl'] or {
					return error('No Erlang template found for function: ${function_name}')
				}

				// Generate all arguments
				mut arg_codes := []string{}
				for child in node.children {
					arg_code := g.generate_node_to_string(child)!
					arg_codes << arg_code
				}

				// Replace placeholders in template
				mut result := template
				for i, arg_code in arg_codes {
					placeholder := '$${i + 1}'
					result = result.replace(placeholder, arg_code)
				}
				g.output.write_string(result)
			}
			.infix {
				if node.children.len == 2 {
					// Use kernel template for infix operators
					if function_info.gen.len == 0 {
						return error('No templates found for function: ${function_name}')
					}
					template := function_info.gen[0]['erl'] or {
						return error('No Erlang template found for function: ${function_name}')
					}
					left_code := g.generate_node_to_string(node.children[0])!
					right_code := g.generate_node_to_string(node.children[1])!
					result := template.replace('$1', left_code).replace('$2', right_code)
					g.output.write_string(result)
				} else {
					g.output.write_string('${function_name}(')
					for i, arg in node.children {
						if i > 0 {
							g.output.write_string(', ')
						}
						g.generate_node(arg)!
					}
					g.output.write_string(')')
				}
			}
			.postfix {
				g.output.write_string('${function_name}(')
				for i, arg in node.children {
					if i > 0 {
						g.output.write_string(', ')
					}
					g.generate_node(arg)!
				}
				g.output.write_string(')')
			}
		}
		return
	}

	// Finally, if not found anywhere
	return error('Unknown function: ${function_name}')
}

fn (mut g ErlangGenerator) substitute_template(template string, args ...ast.Node) !string {
	mut result := template

	// Substitui $1, $2, etc. pelos argumentos gerados
	for i, arg in args {
		placeholder := '$$${(i + 1).str()}'
		arg_code := g.generate_node_to_string(arg)!
		result = result.replace(placeholder, arg_code)
	}

	return result
}

fn (mut g ErlangGenerator) generate_node_to_string(node ast.Node) !string {
	match node.kind {
		.integer, .float, .string, .boolean, .atom, .nil {
			return g.generate_literal_to_string(node)
		}
		.variable_ref {
			return g.generate_variable_ref_to_string(node)
		}
		.identifier {
			return g.generate_identifier_to_string(node)
		}
		.function_caller {
			return g.generate_function_caller_to_string(node)
		}
		.parentheses {
			return g.generate_parentheses_to_string(node)
		}
		.list_literal {
			return g.generate_list_literal_to_string(node)
		}
		.list_cons {
			return g.generate_list_cons_to_string(node)
		}
		.tuple_literal {
			return g.generate_tuple_literal_to_string(node)
		}
		.map_literal {
			return g.generate_map_literal_to_string(node)
		}
		.map_access {
			return g.generate_map_access_to_string(node)
		}
		.record_access {
			return g.generate_record_access_to_string(node)
		}
		else {
			return error('Unsupported node type for string generation: ${node.kind}')
		}
	}
}

fn (mut g ErlangGenerator) generate_literal_to_string(node ast.Node) !string {
	match node.kind {
		.integer, .float, .boolean, .atom {
			return node.value
		}
		.string {
			escaped := g.escape_string(node.value)
			return '<<"${escaped}"/utf8>>'
		}
		.nil {
			return 'nil'
		}
		else {
			return error('Unknown literal type: ${node.kind}')
		}
	}
}

fn (mut g ErlangGenerator) generate_variable_ref_to_string(node ast.Node) !string {
	original_name := node.value
	unique_name := g.get_unique_var_name(original_name)
	return unique_name
}

fn (mut g ErlangGenerator) generate_identifier_to_string(node ast.Node) !string {
	original_name := node.value
	unique_name := g.get_unique_var_name(original_name)
	return unique_name
}

fn (mut g ErlangGenerator) generate_function_caller_to_string(node ast.Node) !string {
	if node.children.len < 1 {
		return error('Invalid function call node')
	}

	function_name := node.value

	// First, try to get function type from type table (user-defined functions)
	if _ := g.type_table.get_function_type(function_name) {
		mut result := '${function_name}('
		for i, arg in node.children {
			if i > 0 {
				result += ', '
			}
			arg_code := g.generate_node_to_string(arg)!
			result += arg_code
		}
		result += ')'
		return result
	}

	// Second, try kernel for built-in functions
	function_info := kernel.get_function_info(function_name) or {
		return error('Unknown function: ${function_name}')
	}

	match function_info.fixity {
		.infix {
			if node.children.len != 2 {
				return error('Infix operator requires exactly 2 arguments')
			}
			if function_info.gen.len == 0 {
				return error('No templates found for function: ${function_name}')
			}
			template := function_info.gen[0]['erl'] or {
				return error('No Erlang template found for function: ${function_name}')
			}
			left_code := g.generate_node_to_string(node.children[0])!
			right_code := g.generate_node_to_string(node.children[1])!
			return template.replace('$1', left_code).replace('$2', right_code)
		}
		.prefix {
			// Check if this is a multi-arg prefix function
			if g.is_multi_arg_prefix_function(function_name) {
				// Multi-arg prefix functions are called as regular functions
				if function_info.gen.len == 0 {
					return error('No templates found for function: ${function_name}')
				}
				template := function_info.gen[0]['erl'] or {
					return error('No Erlang template found for function: ${function_name}')
				}

				// Generate all arguments
				mut arg_codes := []string{}
				for child in node.children {
					arg_code := g.generate_node_to_string(child)!
					arg_codes << arg_code
				}

				// Replace placeholders in template
				mut result := template
				for i, arg_code in arg_codes {
					placeholder := '$${i + 1}'
					result = result.replace(placeholder, arg_code)
				}
				return result
			} else {
				// Single-arg prefix functions
				if node.children.len != 1 {
					return error('Prefix operator requires exactly 1 argument')
				}
				if function_info.gen.len == 0 {
					return error('No templates found for function: ${function_name}')
				}
				template := function_info.gen[0]['erl'] or {
					return error('No Erlang template found for function: ${function_name}')
				}
				arg_code := g.generate_node_to_string(node.children[0])!
				return template.replace('$1', arg_code)
			}
		}
		else {
			return error('Unsupported fixity: ${function_info.fixity}')
		}
	}
}

fn (mut g ErlangGenerator) generate_parentheses_to_string(node ast.Node) !string {
	if node.children.len != 1 {
		return error('Invalid parentheses node')
	}
	inner_code := g.generate_node_to_string(node.children[0])!
	return '(${inner_code})'
}

fn (mut g ErlangGenerator) generate_parentheses(node ast.Node) ! {
	if node.children.len != 1 {
		return error('Invalid parentheses node')
	}

	g.output.write_string('(')
	g.generate_node(node.children[0])!
	g.output.write_string(')')
}

fn (mut g ErlangGenerator) generate_list_literal(node ast.Node) ! {
	if node.children.len == 0 {
		g.output.write_string('[]')
		return
	}

	g.output.write_string('[')

	for i, element in node.children {
		if i > 0 {
			g.output.write_string(', ')
		}
		g.generate_node(element)!
	}

	g.output.write_string(']')
}

fn (mut g ErlangGenerator) generate_list_cons(node ast.Node) ! {
	if node.children.len != 2 {
		return error('List cons must have exactly 2 children')
	}

	g.output.write_string('[')
	g.generate_node(node.children[0])!
	g.output.write_string(' | ')
	g.generate_node(node.children[1])!
	g.output.write_string(']')
}

fn (mut g ErlangGenerator) generate_list_literal_to_string(node ast.Node) !string {
	if node.children.len == 0 {
		return '[]'
	}

	mut result := '['
	for i, element in node.children {
		if i > 0 {
			result += ', '
		}
		element_code := g.generate_node_to_string(element)!
		result += element_code
	}
	result += ']'
	return result
}

fn (mut g ErlangGenerator) generate_list_cons_to_string(node ast.Node) !string {
	if node.children.len != 2 {
		return error('List cons must have exactly 2 children')
	}

	head_code := g.generate_node_to_string(node.children[0])!
	tail_code := g.generate_node_to_string(node.children[1])!
	return '[${head_code} | ${tail_code}]'
}

fn (mut g ErlangGenerator) generate_tuple_literal(node ast.Node) ! {
	if node.children.len == 0 {
		g.output.write_string('{}')
		return
	}

	g.output.write_string('{')

	for i, element in node.children {
		if i > 0 {
			g.output.write_string(', ')
		}
		g.generate_node(element)!
	}

	g.output.write_string('}')
}

fn (mut g ErlangGenerator) generate_tuple_literal_to_string(node ast.Node) !string {
	if node.children.len == 0 {
		return '{}'
	}

	mut result := '{'
	for i, element in node.children {
		if i > 0 {
			result += ', '
		}
		element_code := g.generate_node_to_string(element)!
		result += element_code
	}
	result += '}'
	return result
}

fn (mut g ErlangGenerator) generate_map_literal(node ast.Node) ! {
	if node.children.len == 0 {
		g.output.write_string('#{}')
		return
	}

	g.output.write_string('#{')

	for i := 0; i < node.children.len; i += 2 {
		if i > 0 {
			g.output.write_string(', ')
		}

		// Generate key (can be any term LX)
		key := node.children[i]
		g.generate_node(key)!
		g.output.write_string(' => ')

		// Generate value
		value := node.children[i + 1]
		g.generate_node(value)!
	}

	g.output.write_string('}')
}

fn (mut g ErlangGenerator) generate_map_literal_to_string(node ast.Node) !string {
	if node.children.len == 0 {
		return '#{}'
	}

	mut result := '#{'
	for i := 0; i < node.children.len; i += 2 {
		if i > 0 {
			result += ', '
		}

		// Generate key
		key := node.children[i]
		key_code := g.generate_node_to_string(key)!
		result += key_code
		result += ' => '

		// Generate value
		value := node.children[i + 1]
		value_code := g.generate_node_to_string(value)!
		result += value_code
	}
	result += '}'
	return result
}

fn (g ErlangGenerator) is_multi_arg_prefix_function(function_name string) bool {
	// Lista de funções nativas prefix que recebem múltiplos argumentos
	multi_arg_prefix_functions := ['element', 'setelement', 'map_size', 'map_get', 'map_put',
		'map_remove']
	return function_name in multi_arg_prefix_functions
}

fn (mut g ErlangGenerator) generate_map_access(node ast.Node) ! {
	if node.children.len != 2 {
		return error('Map access must have exactly 2 children (map and key)')
	}

	map_expr := node.children[0]
	key_expr := node.children[1]

	// Generate maps:get(key, map)
	g.output.write_string('maps:get(')
	g.generate_node(key_expr)!
	g.output.write_string(', ')
	g.generate_node(map_expr)!
	g.output.write_string(')')
}

fn (mut g ErlangGenerator) generate_map_access_to_string(node ast.Node) !string {
	if node.children.len != 2 {
		return error('Map access must have exactly 2 children (map and key)')
	}

	map_expr := node.children[0]
	key_expr := node.children[1]

	// Generate maps:get(key, map)
	key_code := g.generate_node_to_string(key_expr)!
	map_code := g.generate_node_to_string(map_expr)!

	return 'maps:get(${key_code}, ${map_code})'
}

// Record generation functions
fn (mut g ErlangGenerator) generate_record_definition(node ast.Node) ! {
	record_name := node.value.to_lower() // Convert to lowercase for Erlang convention

	// Generate record definition header
	g.output.write_string('-record(${record_name}, {')

	// Generate field definitions
	for i, field in node.children {
		if i > 0 {
			g.output.write_string(', ')
		}

		if field.kind != .record_field {
			g.error('Expected record field, got ${field.kind}')
			return error('Expected record field, got ${field.kind}')
		}

		field_name := field.value
		field_type_node := field.children[0]

		// Determine field type
		mut field_type := ast.Type{}
		if field_type_node.value != '' {
			// Use explicit type
			field_type = ast.Type{
				name:   field_type_node.value
				params: []
			}
		} else if g.type_table != unsafe { nil } {
			// Try to get inferred type from type table
			if _ := g.type_table.get_record_type(node.value) {
				if inferred_type := g.type_table.get_field_type(node.value, field_name) {
					field_type = inferred_type
				} else {
					// Fallback to any() if type not found
					field_type = ast.Type{
						name:   'any'
						params: []
					}
				}
			} else {
				// Fallback to any() if record type not found
				field_type = ast.Type{
					name:   'any'
					params: []
				}
			}
		} else {
			// No type table available, fallback to any()
			field_type = ast.Type{
				name:   'any'
				params: []
			}
		}

		// Use the standard type conversion function
		erlang_type := type_to_erlang_spec(field_type)

		// Generate field with or without default value
		if field.children.len > 1 {
			// Field has default value
			default_value := field.children[1]
			g.output.write_string('${field_name} = ')
			g.generate_node(default_value)!
			g.output.write_string(' :: ${erlang_type}')
		} else {
			// Field without default value
			g.output.write_string('${field_name} = nil :: ${erlang_type}')
		}
	}

	g.output.write_string('}).\n')
}

fn (mut g ErlangGenerator) generate_record_literal(node ast.Node) ! {
	record_name := node.value.to_lower() // Convert to lowercase for Erlang convention

	g.output.write_string('#${record_name}{')

	for i, field in node.children {
		if i > 0 {
			g.output.write_string(', ')
		}

		field_name := field.value
		field_value := field.children[0]

		g.output.write_string('${field_name} = ')
		g.generate_node(field_value)!
	}

	g.output.write_string('}')
}

fn (mut g ErlangGenerator) generate_record_access(node ast.Node) ! {
	if node.children.len != 1 {
		return error('Record access must have exactly one child')
	}

	record_expr := node.children[0]
	field_name := node.value

	// Get the record name from the type table
	record_name := g.get_record_name_from_type(record_expr)

	g.generate_node(record_expr)!
	g.output.write_string('#${record_name}.${field_name}')
}

fn (g ErlangGenerator) get_record_name_from_type(record_expr ast.Node) string {
	// If we have access to type_table, try to get the record type
	if g.type_table != unsafe { nil } {
		if record_type := g.type_table.get_type(record_expr.id) {
			// If it's a record type, the name should be the type name
			if record_type.name.len > 0 && record_type.name[0].is_capital() {
				return record_type.name.to_lower()
			}
		}
	}

	// If it's a record literal, we can get the name directly
	if record_expr.kind == .record_literal {
		return record_expr.value.to_lower()
	}

	// Default fallback
	return 'record'
}

fn (mut g ErlangGenerator) generate_record_update(node ast.Node) ! {
	if node.children.len != 3 {
		return error('Record update must have exactly 3 children')
	}

	record_name := node.value.to_lower() // Convert to lowercase for Erlang convention
	record_expr := node.children[0]
	field_name_node := node.children[1]
	field_value := node.children[2]

	g.generate_node(record_expr)!
	g.output.write_string('#${record_name}{')
	g.output.write_string(field_name_node.value)
	g.output.write_string(' = ')
	g.generate_node(field_value)!
	g.output.write_string('}')
}

fn (mut g ErlangGenerator) generate_record_access_to_string(node ast.Node) !string {
	if node.children.len != 1 {
		return error('Record access must have exactly one child')
	}

	record_expr := node.children[0]
	field_name := node.value

	// Get the record name from the type table
	record_name := g.get_record_name_from_type(record_expr)

	record_code := g.generate_node_to_string(record_expr)!
	return '${record_code}#${record_name}.${field_name}'
}

fn (mut g ErlangGenerator) generate_single_function(function_name string, args_block ast.Node, body ast.Node) ! {
	// Generate function signature
	g.output.write_string('${function_name}(')

	// Generate arguments
	if args_block.kind == .block {
		for i, arg in args_block.children {
			if i > 0 {
				g.output.write_string(', ')
			}
			// Generate argument as variable with unique hash
			if arg.kind == .identifier {
				unique_name := g.get_unique_var_name(arg.value)
				g.output.write_string(unique_name)
			} else {
				g.generate_node(arg)!
			}
		}
	}

	g.output.write_string(') ->\n    ')

	// Generate function body
	if body.kind == .block {
		for i, expr in body.children {
			if expr.kind == .directive_call {
				continue
			}

			g.generate_node(expr)!
			if i < body.children.len - 1 {
				g.output.write_string(',\n    ')
			}
		}
	} else {
		g.generate_node(body)!
	}

	g.output.write_string('.\n')
}

fn (g ErlangGenerator) needs_parentheses(node ast.Node) bool {
	match node.kind {
		.integer, .float, .string, .boolean, .atom, .nil, .identifier, .variable_ref {
			return false
		}
		.function_caller {
			// Function calls don't need parentheses around them
			return false
		}
		.parentheses {
			return false
		}
		else {
			return true
		}
	}
}

// New generation functions for additional functionality

fn (mut g ErlangGenerator) generate_function_parameter(node ast.Node) ! {
	// Function parameters are just identifiers, generate as variable name
	unique_name := g.get_unique_var_name(node.value)
	g.output.write_string(unique_name)
}

fn (mut g ErlangGenerator) generate_lambda_expression(node ast.Node) ! {
	if node.children.len < 2 {
		return error('Lambda expression must have parameters and body')
	}

	g.output.write_string('fun(')

	// Generate parameters
	params := node.children[0..node.children.len - 1]
	for i, param in params {
		g.generate_node(param)!
		if i < params.len - 1 {
			g.output.write_string(', ')
		}
	}

	g.output.write_string(') ->\n        ')

	// Generate body
	body := node.children[node.children.len - 1]
	g.generate_node(body)!

	g.output.write_string('\n    end')
}

fn (mut g ErlangGenerator) generate_case_expression(node ast.Node) ! {
	if node.children.len < 2 {
		return error('Case expression must have expression and clauses')
	}

	g.output.write_string('case ')

	// Generate expression to match
	expr := node.children[0]
	g.generate_node(expr)!

	g.output.write_string(' of\n')

	// Generate clauses
	clauses := node.children[1..]
	for clause in clauses {
		g.output.write_string('        ')
		g.generate_node(clause)!
		g.output.write_string('\n')
	}

	g.output.write_string('    end')
}

fn (mut g ErlangGenerator) generate_case_clause(node ast.Node) ! {
	if node.children.len != 2 {
		return error('Case clause must have pattern and body')
	}

	pattern := node.children[0]
	body := node.children[1]

	// Generate pattern
	g.generate_pattern(pattern)!
	g.output.write_string(' ->\n            ')

	// Generate body
	g.generate_node(body)!
	g.output.write_string(';')
}

fn (mut g ErlangGenerator) generate_pattern(node ast.Node) ! {
	match node.kind {
		.identifier {
			// Variable pattern
			unique_name := g.get_unique_var_name(node.value)
			g.output.write_string(unique_name)
		}
		.list_literal {
			// List pattern
			g.output.write_string('[')
			for i, child in node.children {
				g.generate_pattern(child)!
				if i < node.children.len - 1 {
					g.output.write_string(', ')
				}
			}
			g.output.write_string(']')
		}
		.list_cons {
			// List cons pattern [head | tail]
			if node.children.len == 2 {
				g.output.write_string('[')
				g.generate_pattern(node.children[0])!
				g.output.write_string(' | ')
				g.generate_pattern(node.children[1])!
				g.output.write_string(']')
			}
		}
		else {
			// Literal patterns
			g.generate_node(node)!
		}
	}
}

fn (mut g ErlangGenerator) generate_pattern_match(node ast.Node) ! {
	if node.children.len == 1 {
		g.generate_pattern(node.children[0])!
	}
}

fn (mut g ErlangGenerator) generate_pattern_binding(node ast.Node) ! {
	if node.children.len == 2 {
		pattern := node.children[0]
		expr := node.children[1]

		// Generate pattern binding: Pattern = Expression
		g.generate_pattern(pattern)!
		g.output.write_string(' = ')
		g.generate_node(expr)!
	}
}

fn (mut g ErlangGenerator) generate_type_alias(node ast.Node) ! {
	// Type aliases are not generated in Erlang output
	// They are used only for type checking
}

fn (mut g ErlangGenerator) generate_type_annotation(node ast.Node) ! {
	// Type annotations are not generated in Erlang output
	// They are used only for type checking
}
