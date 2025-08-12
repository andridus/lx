module generator

import ast
import strings
import analysis
import kernel
import parser

@[heap]
pub struct ErlangGenerator {
mut:
	output           strings.Builder
	errors           []string
	type_table       &analysis.TypeTable = unsafe { nil }
	var_map          map[string]string // Maps original var names to unique Erlang names
	next_hash        int = 1
	directives_table &parser.DirectivesTable
}

pub fn new_generator(directives_table &parser.DirectivesTable) ErlangGenerator {
	return ErlangGenerator{
		directives_table: directives_table
		var_map:          map[string]string{}
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
	if original_name == '_' {
		return '_'
	}
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
		// Task 11: Control Flow
		.if_expr {
			g.generate_if_expr(node)!
		}
		.with_expr {
			g.generate_with_expr(node)!
		}
		.match_expr {
			g.generate_match_expr(node)!
		}
		// Task 11: Concurrency
		.spawn_expr {
			g.generate_spawn_expr(node)!
		}
		.send_expr {
			g.generate_send_expr(node)!
		}
		.receive_expr {
			g.generate_receive_expr(node)!
		}
		.supervisor_def {
			g.generate_supervisor_def(node)!
		}
		.worker_def {
			g.generate_worker_def(node)!
		}
		// Task 11: Binaries
		.binary_literal {
			g.generate_binary_literal(node)!
		}
		.binary_pattern {
			g.generate_binary_pattern(node)!
		}
		.binary_segment {
			g.generate_binary_segment(node)!
		}
		// Task 11: Custom Types
		.type_def {
			g.generate_type_def(node)!
		}
		.union_type {
			g.generate_union_type(node)!
		}
		.generic_type {
			g.generate_generic_type(node)!
		}
		.opaque_type {
			g.generate_opaque_type(node)!
		}
		.nominal_type {
			g.generate_nominal_type(node)!
		}
		// Task 11: Module System
		.deps_declaration {
			g.generate_deps_declaration(node)!
		}
		.application_config {
			g.generate_application_config(node)!
		}
		.import_statement {
			g.generate_import_statement(node)!
		}
		// Task 11: Advanced Features
		.string_interpolation {
			g.generate_string_interpolation(node)!
		}
		.anonymous_function {
			g.generate_anonymous_function(node)!
		}
		.lambda_call {
			g.generate_lambda_call(node)!
		}
		.list_comprehension {
			g.generate_list_comprehension(node)!
		}
		.directive {}
		.test_block {
			g.generate_test_block(node)!
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
	// Generate module-level directives (@moduledoc)
	moduledoc := g.directives_table.get_moduledoc()
	if moduledoc.len > 0 {
		g.output.write_string('-moduledoc "${moduledoc}" .\n')
	}

	// Collect function exports
	mut exports := []string{}
	for child in node.children {
		if child.kind == .function {
			// Calculate arity from args block or from heads
			mut arity := 0
			mut arities := []int{}

			if child.children.len > 0 {
				args_block := child.children[0]
				if args_block.children.len > 0 {
					// Function has arguments in definition
					arity = args_block.children.len
					arities << arity
				} else if child.children.len > 1 {
					// Check if this has multiple heads
					body := child.children[1]
					if body.kind == .block && body.children.len > 0
						&& body.children[0].kind == .function {
						// Multiple heads - collect all arities
						for head in body.children {
							if head.kind == .function && head.children.len > 0 {
								head_args := head.children[0]
								mut head_arity := 0
								if head_args.kind == .block {
									head_arity = head_args.children.len
								} else {
									head_arity = 1
								}
								if head_arity !in arities {
									arities << head_arity
								}
							}
						}
					}
				}
			}

			// Export all arities
			if arities.len > 0 {
				for arity_val in arities {
					exports << '${child.value}/${arity_val}'
				}
			} else {
				exports << '${child.value}/${arity}'
			}
		}
	}

	if exports.len > 0 {
		g.output.write_string('-export([${exports.join(', ')}]).\n\n')
	}

	// Module system comments (deps/import/application)
	mut comments_emitted := false
	for child in node.children {
		if child.kind == .deps_declaration {
			g.generate_deps_declaration(child)!
			comments_emitted = true
		}
		if child.kind == .import_statement {
			g.generate_import_statement(child)!
			comments_emitted = true
		}
		if child.kind == .application_config {
			g.generate_application_config(child)!
			comments_emitted = true
		}
	}
	// Blank line after comments block only if any were emitted
	if comments_emitted {
		g.output.write_string('\n')
	}

	// Generate record definitions first
	for child in node.children {
		if child.kind == .record_definition {
			g.generate_record_definition(child)!
		}
	}

	// Generate type definitions
	for child in node.children {
		if child.kind == .type_def {
			g.generate_type_def(child)!
		} else if child.kind == .opaque_type {
			g.generate_opaque_type(child)!
		} else if child.kind == .nominal_type {
			g.generate_nominal_type(child)!
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

	// Check if this is a multi-head function
	mut has_function_heads := false
	mut arities := []int{}

	if node.children.len >= 2 {
		body := node.children[1]
		has_function_heads = body.kind == .block && body.children.len > 0
			&& body.children[0].kind == .function

		if has_function_heads {
			// Collect all arities from heads
			for head in body.children {
				if head.kind == .function && head.children.len > 0 {
					head_args := head.children[0]
					mut arity := 0
					if head_args.kind == .block {
						arity = head_args.children.len
					} else {
						arity = 1
					}
					if arity !in arities {
						arities << arity
					}
				}
			}
		}
	}

	// Calculate function key (name/arity) for single function
	mut arity := 0
	if !has_function_heads && node.children.len > 0 {
		args_block := node.children[0]
		if args_block.kind == .block {
			arity = args_block.children.len
		}
	}

	// For multi-head functions, use the first arity for the main function key
	if has_function_heads && arities.len > 0 {
		arity = arities[0]
	}

	function_key := '${function_name}/${arity}'

	// Get and generate directives for this function
	directive := g.directives_table.get_doc(function_key)
	if directive.kind == .string {
		g.output.write_string('-doc "${directive.value}".\n')
	}

	// Generate specs for multi-head functions with different arities
	if has_function_heads && arities.len > 1 {
		// Don't generate specs here - they will be generated with each function
	} else if has_function_heads {
		// Single arity multi-head - generate spec with union types if needed
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
	} else {
		// Single function - generate normal spec
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
	}

	// Generate function body
	if node.children.len >= 2 {
		args_block := node.children[0]
		body := node.children[1]

		if has_function_heads {
			// Multi-head function - generate each head with proper pattern matching
			// Group heads by arity first
			mut heads_by_arity := map[int][]ast.Node{}

			for head in body.children {
				if head.kind == .function && head.children.len > 0 {
					head_args := head.children[0]
					mut head_arity := 0
					if head_args.kind == .block {
						head_arity = head_args.children.len
					} else {
						head_arity = 1
					}

					if head_arity !in heads_by_arity {
						heads_by_arity[head_arity] = []
					}
					heads_by_arity[head_arity] << head
				}
			}

			// Check if we have multiple arities (different arities = separate functions)
			if arities.len > 1 {
				// Generate separate function for each arity with its spec
				for arity_val, heads in heads_by_arity {
					// Generate spec for this specific arity
					g.output.write_string('-spec ${function_name}(')
					for i in 0 .. arity_val {
						if i > 0 {
							g.output.write_string(', ')
						}
						g.output.write_string('any()')
					}
					g.output.write_string(') -> any().\n')

					// Generate function definition for this arity
					for i, head in heads {
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

								if i < heads.len - 1 {
									g.output.write_string(';\n')
								} else {
									g.output.write_string('.\n')
								}
							}
						}
					}
				}
			} else {
				// Single arity with multiple heads - generate one function with pattern matching
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
			// Handle hexadecimal literals (0x -> decimal)
			if node.value.starts_with('0x') {
				hex_str := node.value[2..]
				decimal_value := parse_hex_to_decimal(hex_str) or {
					return error('Invalid hexadecimal literal: ${node.value}')
				}
				g.output.write_string(decimal_value.str())
			}
			// Handle octal literals (0o -> decimal)
			else if node.value.starts_with('0o') {
				octal_str := node.value[2..]
				decimal_value := parse_octal_to_decimal(octal_str) or {
					return error('Invalid octal literal: ${node.value}')
				}
				g.output.write_string(decimal_value.str())
			}
			// Handle binary literals (0b -> decimal)
			else if node.value.starts_with('0b') {
				binary_str := node.value[2..]
				decimal_value := parse_binary_to_decimal(binary_str) or {
					return error('Invalid binary literal: ${node.value}')
				}
				g.output.write_string(decimal_value.str())
			}
			// Handle base generic literals (BaseB -> Base#Value)
			else if node.value.contains('B') {
				base_parts := node.value.split('B')
				if base_parts.len == 2 {
					base := base_parts[0]
					value := base_parts[1]
					g.output.write_string('${base}#${value}')
				} else {
					return error('Invalid base generic literal: ${node.value}')
				}
			} else {
				g.output.write_string(node.value)
			}
		}
		.float {
			g.output.write_string(node.value)
		}
		.string {
			g.generate_string_literal(node)!
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
		'atom_literal' {
			// Expect first param to carry the literal atom name
			if t.params.len > 0 {
				return t.params[0].name
			}
			'atom()'
		}
		else {
			// Check if this is a record type (should be converted to lowercase)
			if t.name.len > 0 && t.name[0].is_capital() {
				'#${t.name.to_lower()}{}'
			} else if t.name.len == 0 {
				'any()'
			} else {
				// Default: assume custom type reference name()
				t.name + '()'
			}
		}
	}
}

fn (mut g ErlangGenerator) generate_function_caller(node ast.Node) ! {
	function_name := node.value

	// First, try kernel for built-in functions (including operators)
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

	// Second, try to get function type from type table (user-defined functions)
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

	// Third, check if it's a variable (first-class function)
	// Try to get type for this identifier - if it exists, it might be a variable
	if _ := g.type_table.get_type(node.id) {
		// This is likely a variable containing a function
		// In Erlang, call variables containing functions with the correct mapped name
		unique_name := g.get_unique_var_name(function_name)
		g.output.write_string('${unique_name}(')
		for i, arg in node.children {
			if i > 0 {
				g.output.write_string(', ')
			}
			g.generate_node(arg)!
		}
		g.output.write_string(')')
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

	// Second, try kernel for built-in functions (including operators)
	if function_info := kernel.get_function_info(function_name) {
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

	// Third, check if it's a variable (first-class function)
	// Try to get type for this identifier - if it exists, it might be a variable
	if _ := g.type_table.get_type(node.id) {
		// This is likely a variable containing a function
		// In Erlang, call variables containing functions with the correct mapped name
		unique_name := g.get_unique_var_name(function_name)
		mut result := '${unique_name}('
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

	// Finally, if not found anywhere
	return error('Unknown function: ${function_name}')
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
	if node.children.len < 1 {
		return error('Lambda expression must have body')
	}

	body := node.children[node.children.len - 1]

	// Check if this is a multi-head lambda (body is a block with function heads)
	if body.kind == .block && body.children.len > 0 && body.children[0].kind == .function {
		// Multi-head lambda: fun ... end with clauses
		g.output.write_string('fun\n')

		for i, head in body.children {
			g.output.write_string('        ')
			g.generate_function_clause(head)!
			if i < body.children.len - 1 {
				g.output.write_string(';\n')
			} else {
				g.output.write_string('\n')
			}
		}

		g.output.write_string('    end')
		return
	}

	// Regular lambda: fun(params) -> body end
	g.output.write_string('fun(')

	// Generate parameters (all children except the last one, which is the body)
	params := if node.children.len > 1 {
		node.children[0..node.children.len - 1]
	} else {
		[]ast.Node{}
	}
	for i, param in params {
		g.generate_node(param)!
		if i < params.len - 1 {
			g.output.write_string(', ')
		}
	}

	g.output.write_string(') ->\n        ')

	// Generate body
	g.generate_node(body)!

	g.output.write_string('\n    end')
}

fn (mut g ErlangGenerator) generate_function_clause(node ast.Node) ! {
	// Generate function clause for lambda (without function name)
	if node.children.len >= 2 {
		args_block := node.children[0]
		body := node.children[1]

		// Generate arguments
		g.output.write_string('(')
		if args_block.kind == .block {
			for j, arg in args_block.children {
				if j > 0 {
					g.output.write_string(', ')
				}
				// Generate argument as pattern
				g.generate_node(arg)!
			}
		} else {
			// Single argument
			g.generate_node(args_block)!
		}
		g.output.write_string(') ->\n            ')

		// Generate body
		g.generate_node(body)!
	}
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
	for i, clause in clauses {
		g.output.write_string('        ')
		g.generate_node(clause)!
		// Add semicolon except for the last clause
		if i < clauses.len - 1 {
			g.output.write_string(';\n')
		} else {
			g.output.write_string('\n')
		}
	}

	g.output.write_string('    end')
}

fn (mut g ErlangGenerator) generate_case_clause(node ast.Node) ! {
	if node.children.len < 2 || node.children.len > 3 {
		return error('Case clause must have pattern and body, optionally with guard')
	}

	pattern := node.children[0]
	body := node.children[1]

	// Generate pattern
	g.generate_pattern(pattern)!

	// Generate guard if present
	if node.children.len == 3 {
		guard := node.children[2]
		g.output.write_string(' when ')
		g.generate_node(guard)!
	}

	g.output.write_string(' ->\n            ')

	// Generate body
	g.generate_node(body)!

	// Reset default flag
}

fn (mut g ErlangGenerator) generate_pattern(node ast.Node) ! {
	match node.kind {
		.identifier, .variable_ref {
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
			g.output.write_string('[')
			g.generate_pattern(node.children[0])!
			g.output.write_string(' | ')
			g.generate_pattern(node.children[1])!
			g.output.write_string(']')
		}
		.tuple_literal {
			// Tuple pattern {a, b}
			g.output.write_string('{')
			for i, child in node.children {
				g.generate_pattern(child)!
				if i < node.children.len - 1 {
					g.output.write_string(', ')
				}
			}
			g.output.write_string('}')
		}
		.record_literal {
			// Record pattern #record_name{field = value}
			record_name := node.value.to_lower()
			g.output.write_string('#${record_name}{')
			for i, field in node.children {
				if i > 0 {
					g.output.write_string(', ')
				}

				field_name := field.value
				field_pattern := field.children[0]

				g.output.write_string('${field_name} = ')
				g.generate_pattern(field_pattern)!
			}
			g.output.write_string('}')
		}
		.binary_pattern {
			// Binary pattern <<...>>
			g.generate_binary_pattern(node)!
		}
		.atom, .integer, .float, .string, .boolean, .nil {
			g.generate_literal(node)!
		}
		else {
			return error('Unsupported pattern node: ${node.kind}')
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

// ============ Task 11: Control Flow Generation ============

// Generate if expressions
fn (mut g ErlangGenerator) generate_if_expr(node ast.Node) ! {
	if node.children.len < 2 {
		return error('If expression must have at least condition and then branch')
	}

	g.output.write_string('case ')
	g.generate_node(node.children[0])! // condition
	g.output.write_string(' of\n')
	g.output.write_string('        true -> ')
	g.generate_node(node.children[1])! // then branch

	if node.children.len > 2 {
		g.output.write_string(';\n        false -> ')
		g.generate_node(node.children[2])! // else branch
	} else {
		g.output.write_string(';\n        false -> nil')
	}

	g.output.write_string('\n    end')
}

// Generate with expressions (simplified as case in Erlang)
fn (mut g ErlangGenerator) generate_with_expr(node ast.Node) ! {
	if node.children.len < 2 {
		return error('With expression must have at least one clause and body')
	}

	// Check if this is old format (3 children: pattern, expr, body) or new format (multiple clauses + body)
	if node.children.len == 3 && node.children[0].kind != .pattern_match {
		// Old format: single pattern, expr, body
		g.generate_with_expr_single(node)!
	} else {
		// New format: multiple clauses + body (+ optional else)
		g.generate_with_expr_multi(node)!
	}
}

fn (mut g ErlangGenerator) generate_with_expr_single(node ast.Node) ! {
	g.output.write_string('case ')
	g.generate_node(node.children[1])! // expression
	g.output.write_string(' of\n')
	g.output.write_string('        ')
	g.generate_node(node.children[0])! // pattern
	g.output.write_string(' -> ')
	g.generate_node(node.children[2])! // body
	g.output.write_string(';\n        Error -> ')
	if node.children.len > 3 {
		g.generate_node(node.children[3])! // else body
	} else {
		g.output.write_string('Error')
	}
	g.output.write_string('\n    end')
}

fn (mut g ErlangGenerator) generate_with_expr_multi(node ast.Node) ! {
	// Find where body starts (after all pattern_match clauses)
	mut body_index := 0
	for i, child in node.children {
		if child.kind != .pattern_match {
			body_index = i
			break
		}
	}

	if body_index == 0 {
		return error('With expression must have at least one clause')
	}

	// For multiple clauses, we need to generate nested cases
	// But for single clause, we can optimize by putting else clauses in the same case
	if body_index == 1 {
		// Single with clause - can put else in same case
		g.generate_with_single_optimized(node, body_index)!
	} else {
		// Multiple clauses - need nested cases
		g.generate_with_nested(node, body_index)!
	}
}

fn (mut g ErlangGenerator) generate_with_single_optimized(node ast.Node, body_index int) ! {
	clause := node.children[0]
	if clause.kind == .pattern_match && clause.children.len >= 2 {
		g.output.write_string('case ')
		g.generate_node(clause.children[1])! // expression
		g.output.write_string(' of\n        ')
		g.generate_pattern(clause.children[0])! // pattern
		g.output.write_string(' ->\n            ')

		// Generate body
		g.generate_node(node.children[body_index])!
		g.output.write_string(';\n')

		// Generate else clauses in the same case
		if node.children.len > body_index + 1 {
			else_body := node.children[body_index + 1]
			if else_body.kind == .case_expression {
				// Generate case clauses directly inline
				g.generate_else_clauses_flat(else_body)!
			} else {
				g.output.write_string('        Error -> ')
				g.generate_node(else_body)!
				g.output.write_string('\n')
			}
		} else {
			g.output.write_string('        Error -> Error\n')
		}

		g.output.write_string('    end')
	}
}

fn (mut g ErlangGenerator) generate_with_nested(node ast.Node, body_index int) ! {
	g.generate_nested_cases_optimized(node, body_index, 0)!
}

fn (mut g ErlangGenerator) generate_nested_cases_optimized(node ast.Node, body_index int, current_level int) ! {
	if current_level >= body_index {
		// All clauses processed, generate body
		g.generate_node(node.children[body_index])!
		return
	}

	clause := node.children[current_level]
	if clause.kind == .pattern_match && clause.children.len >= 2 {
		g.output.write_string('case ')
		g.generate_node(clause.children[1])! // expression
		g.output.write_string(' of\n        ')
		g.generate_pattern(clause.children[0])! // pattern
		g.output.write_string(' ->\n            ')

		// Recursively generate next level
		g.generate_nested_cases_optimized(node, body_index, current_level + 1)!

		// Generate else clauses for this level
		g.output.write_string(';\n')
		g.generate_else_clauses_for_level(node, body_index)!
		g.output.write_string('\n    end')
	}
}

fn (mut g ErlangGenerator) generate_else_clauses_for_level(node ast.Node, body_index int) ! {
	if node.children.len > body_index + 1 {
		// Has else body
		else_body := node.children[body_index + 1]
		if else_body.kind == .case_expression {
			// Generate case clauses directly inline
			clauses := else_body.children[1..] // Skip dummy expression
			for i, clause in clauses {
				if clause.children.len >= 2 {
					g.output.write_string('        ')
					g.generate_pattern(clause.children[0])! // pattern
					g.output.write_string(' -> ')
					g.generate_node(clause.children[1])! // body
					if i < clauses.len - 1 {
						g.output.write_string(';\n')
					}
				}
			}
		} else {
			g.output.write_string('        Error -> ')
			g.generate_node(else_body)!
		}
	} else {
		g.output.write_string('        Error -> Error')
	}
}

fn (mut g ErlangGenerator) generate_else_clauses_flat(case_node ast.Node) ! {
	// Generate else clauses as flat case clauses (not nested)
	clauses := case_node.children[1..] // Skip dummy expression
	for i, clause in clauses {
		if clause.children.len >= 2 {
			g.output.write_string('        ')
			g.generate_pattern(clause.children[0])! // pattern
			g.output.write_string(' ->\n            ')
			g.generate_node(clause.children[1])! // body
			if i < clauses.len - 1 {
				g.output.write_string(';\n')
			} else {
				g.output.write_string('\n')
			}
		}
	}
}

// Generate case clauses inline (for else body in with)
fn (mut g ErlangGenerator) generate_case_clauses_inline(case_node ast.Node) ! {
	// Skip the first child (dummy expression) and process clauses
	clauses := case_node.children[1..]
	if clauses.len == 1 {
		// Single clause - generate directly
		clause := clauses[0]
		if clause.children.len >= 2 {
			// Check if it's a wildcard pattern
			pattern := clause.children[0]
			if pattern.kind == .identifier && pattern.value == '_' {
				// Just generate the body for wildcard
				g.generate_node(clause.children[1])!
			} else {
				// Generate as case
				g.output.write_string('case Error of\n        ')
				g.generate_pattern(pattern)!
				g.output.write_string(' -> ')
				g.generate_node(clause.children[1])!
				g.output.write_string(';\n        _ -> Error\n    end')
			}
		}
	} else {
		// Multiple clauses - generate as case
		g.output.write_string('case Error of\n')
		for i, clause in clauses {
			if clause.children.len >= 2 {
				g.output.write_string('        ')
				g.generate_pattern(clause.children[0])!
				g.output.write_string(' -> ')
				g.generate_node(clause.children[1])!
				if i < clauses.len - 1 {
					g.output.write_string(';\n')
				} else {
					g.output.write_string('\n')
				}
			}
		}
		g.output.write_string('    end')
	}
}

// Generate match expressions (try-catch in Erlang)
fn (mut g ErlangGenerator) generate_match_expr(node ast.Node) ! {
	if node.children.len < 2 {
		return error('Match expression must have pattern and expression')
	}

	// Generate case expression
	g.output.write_string('case ')
	g.generate_node(node.children[1])! // expression
	g.output.write_string(' of\n        ')
	g.generate_pattern(node.children[0])! // pattern
	g.output.write_string(' ->\n            ')

	// Determine structure: [pattern, expr, rescue_block?, continuation?]
	mut has_rescue := false
	mut continuation_index := 2

	if node.children.len > 2 && node.children[2].kind == .block {
		rescue_block := node.children[2]
		if rescue_block.children.len == 2 && rescue_block.children[0].kind == .variable_ref {
			has_rescue = true
			continuation_index = 3
		}
	}

	if node.children.len > continuation_index {
		g.generate_node(node.children[continuation_index])! // continuation
	} else if !has_rescue {
		g.output.write_string('ok') // fallback
	} else {
		g.output.write_string('ok')
	}

	g.output.write_string(';\n        ')

	// Generate rescue body or default error handling
	if has_rescue && node.children.len > 2 {
		rescue_block := node.children[2]
		if rescue_block.kind == .block && rescue_block.children.len >= 2 {
			// Use the error pattern variable as the match pattern
			error_pattern := rescue_block.children[0]
			g.generate_pattern(error_pattern)! // generates ERROR_3
			g.output.write_string(' ->\n            ')

			// Generate the rescue body
			rescue_body := rescue_block.children[1]
			g.generate_node(rescue_body)!
		} else {
			g.output.write_string('Otherwise ->\n            ')
			g.generate_node(node.children[2])! // fallback: generate the whole rescue node
		}
	} else {
		g.output.write_string('Otherwise ->\n            ')
		g.output.write_string('Otherwise') // default: return the unmatched value
	}

	g.output.write_string('\n    end')
}

// ============ Task 11: Concurrency Generation ============

// Generate spawn expressions
fn (mut g ErlangGenerator) generate_spawn_expr(node ast.Node) ! {
	if node.children.len != 1 {
		return error('Spawn expression must have one argument')
	}

	g.output.write_string('spawn(')
	g.generate_node(node.children[0])! // function expression
	g.output.write_string(')')
}

// Generate send expressions (handled by function_caller for ! operator)
fn (mut g ErlangGenerator) generate_send_expr(node ast.Node) ! {
	if node.children.len != 2 {
		return error('Send expression must have target and message')
	}

	g.generate_node(node.children[0])! // target
	g.output.write_string(' ! ')
	g.generate_node(node.children[1])! // message
}

// Generate receive expressions
fn (mut g ErlangGenerator) generate_receive_expr(node ast.Node) ! {
	g.output.write_string('receive\n')

	for i, clause in node.children {
		g.output.write_string('        ')
		g.generate_case_clause(clause)!
		if i < node.children.len - 1 {
			// produce double semicolon on intermediates (one here + one inside clause users expect)
			g.output.write_string(';;\n')
		} else {
			// last one with single semicolon
			g.output.write_string(';')
		}
	}

	g.output.write_string('\n    end')
}

// Generate supervisor definitions (as regular functions with metadata)
fn (mut g ErlangGenerator) generate_supervisor_def(node ast.Node) ! {
	if node.children.len != 1 {
		return error('Supervisor definition must have body')
	}

	g.output.write_string('supervisor_${node.value}() ->\n')
	g.output.write_string('    ')
	g.generate_node(node.children[0])! // body
	g.output.write_string('.\n\n')
}

// Generate worker definitions (as regular functions with metadata)
fn (mut g ErlangGenerator) generate_worker_def(node ast.Node) ! {
	if node.children.len != 1 {
		return error('Worker definition must have body')
	}

	g.output.write_string('worker_${node.value}() ->\n')
	g.output.write_string('    ')
	g.generate_node(node.children[0])! // body
	g.output.write_string('.\n\n')
}

// ============ Task 11: Binaries Generation ============

// Generate binary literals
fn (mut g ErlangGenerator) generate_binary_literal(node ast.Node) ! {
	g.output.write_string('<<')

	for i, segment in node.children {
		g.generate_node(segment)!
		if i < node.children.len - 1 {
			g.output.write_string(', ')
		}
	}

	g.output.write_string('>>')
}

// Generate binary patterns (same as literals)
fn (mut g ErlangGenerator) generate_binary_pattern(node ast.Node) ! {
	g.generate_binary_literal(node)!
}

// Generate binary segments
fn (mut g ErlangGenerator) generate_binary_segment(node ast.Node) ! {
	children := node.children

	// Expression (required)
	g.generate_node(children[0])!

	// Size (optional)
	if children.len > 1 {
		g.output.write_string(':')
		g.generate_node(children[1])!
	}

	// Options (optional)
	if node.value.len > 0 {
		g.output.write_string('/')
		g.output.write_string(node.value.replace(',', '-'))
	}
}

// ============ Task 11: Custom Types Generation ============

// Generate type definitions (as Erlang -type declarations)
fn (mut g ErlangGenerator) generate_type_def(node ast.Node) ! {
	// Check if we have a type table to get the actual type definition
	if g.type_table != unsafe { nil } {
		if custom_type := g.type_table.get_custom_type(node.value) {
			// Generate Erlang -type declaration
			g.output.write_string('-type ${node.value}() :: ${type_to_erlang_spec(custom_type)}.\n')
			return
		}
	}

	// Fallback to comment if no type information available
	g.output.write_string('%% Type definition: ${node.value}\n')
}

// Generate union types (as comments)
fn (mut g ErlangGenerator) generate_union_type(node ast.Node) ! {
	g.output.write_string('%% Union type: ')
	for i, variant in node.children {
		g.generate_node(variant)!
		if i < node.children.len - 1 {
			g.output.write_string(' | ')
		}
	}
	g.output.write_string('\n')
}

// Generate generic types (as comments)
fn (mut g ErlangGenerator) generate_generic_type(node ast.Node) ! {
	g.output.write_string('%% Generic type: ${node.value}(')
	for i, param in node.children {
		g.generate_node(param)!
		if i < node.children.len - 1 {
			g.output.write_string(', ')
		}
	}
	g.output.write_string(')\n')
}

// Generate opaque types
fn (mut g ErlangGenerator) generate_opaque_type(node ast.Node) ! {
	// Check if we have a type table to get the actual type definition
	if g.type_table != unsafe { nil } {
		if custom_type := g.type_table.get_custom_type(node.value) {
			// Generate Erlang -opaque declaration
			g.output.write_string('-opaque ${node.value}() :: ${type_to_erlang_spec(custom_type)}.\n')
			return
		}
	}

	// Fallback to comment if no type information available
	g.output.write_string('%% Opaque type: ${node.value}\n')
}

// Generate nominal types
fn (mut g ErlangGenerator) generate_nominal_type(node ast.Node) ! {
	// Check if we have a type table to get the actual type definition
	if g.type_table != unsafe { nil } {
		if custom_type := g.type_table.get_custom_type(node.value) {
			// Generate Erlang -type declaration (nominal types use -type, not -opaque)
			g.output.write_string('-nominal ${node.value}() :: ${type_to_erlang_spec(custom_type)}.\n')
			return
		}
	}

	// Fallback to comment if no type information available
	g.output.write_string('%% Nominal type: ${node.value}\n')
}

// ============ Task 11: Module System Generation ============

// Generate deps declarations (as comments)
fn (mut g ErlangGenerator) generate_deps_declaration(node ast.Node) ! {
	g.output.write_string('%% Dependencies: [')
	for i, dep in node.children {
		g.generate_node(dep)!
		if i < node.children.len - 1 {
			g.output.write_string(', ')
		}
	}
	g.output.write_string(']\n')
}

// Generate application config (as comments)
fn (mut g ErlangGenerator) generate_application_config(node ast.Node) ! {
	g.output.write_string('%% Application config:\n')
	for config_item in node.children {
		g.output.write_string('%% ')
		g.generate_node(config_item)!
		g.output.write_string('\n')
	}
}

// Generate import statements (as comments)
fn (mut g ErlangGenerator) generate_import_statement(node ast.Node) ! {
	g.output.write_string('%% Import: ${node.value}\n')
}

// ============ Task 11: Advanced Features Generation ============

// Generate string interpolation (as binary concatenation)
fn (mut g ErlangGenerator) generate_string_interpolation(node ast.Node) ! {
	g.output.write_string('<<')

	for i, segment in node.children {
		g.generate_node(segment)!
		if i < node.children.len - 1 {
			g.output.write_string('/binary, ')
		} else {
			g.output.write_string('/binary')
		}
	}

	g.output.write_string('>>')
}

// Generate anonymous functions
fn (mut g ErlangGenerator) generate_anonymous_function(node ast.Node) ! {
	if node.children.len == 0 {
		return error('Anonymous function must have at least a body')
	}

	g.output.write_string('fun(')

	// Parameters (all children except the last one)
	for i in 0 .. node.children.len - 1 {
		g.generate_node(node.children[i])!
		if i < node.children.len - 2 {
			g.output.write_string(', ')
		}
	}

	g.output.write_string(') -> ')

	// Body (last child)
	g.generate_node(node.children[node.children.len - 1])!

	g.output.write_string(' end')
}

// Generate list comprehensions
fn (mut g ErlangGenerator) generate_list_comprehension(node ast.Node) ! {
	if node.children.len < 3 {
		return error('List comprehension must have variable, list, and body')
	}

	// Children: [variable, list, body, condition?]
	var_node := node.children[0]
	list_node := node.children[1]
	body_node := node.children[2]

	g.output.write_string('[')
	g.generate_node(body_node)! // body expression
	g.output.write_string(' || ')

	// Generate variable <- list
	g.generate_node(var_node)! // variable
	g.output.write_string(' <- ')
	g.generate_node(list_node)! // list

	// Generate condition if present
	if node.children.len > 3 {
		g.output.write_string(', ')
		g.generate_node(node.children[3])! // condition
	}

	g.output.write_string(']')
}

// Generate test blocks (as functions with test_ prefix)
fn (mut g ErlangGenerator) generate_test_block(node ast.Node) ! {
	if node.children.len != 1 {
		return error('Test block must have body')
	}

	g.output.write_string('test_${node.value.replace(' ', '_')}() ->\n')
	g.output.write_string('    ')
	g.generate_node(node.children[0])! // body
	g.output.write_string('.\n\n')
}

fn (mut g ErlangGenerator) generate_lambda_call(node ast.Node) ! {
	if node.children.len < 1 {
		return error('Lambda call must have lambda expression')
	}

	// Generate lambda expression
	g.generate_node(node.children[0])!
	g.output.write_string('(')

	// Generate arguments
	for i in 1 .. node.children.len {
		if i > 1 {
			g.output.write_string(', ')
		}
		g.generate_node(node.children[i])!
	}

	g.output.write_string(')')
}

fn (mut g ErlangGenerator) generate_string_literal(node ast.Node) ! {
	value := node.value
	escaped := g.escape_string(value)
	g.output.write_string('<<"${escaped}"/utf8>>')
}

// parse_hex_to_decimal converts a hexadecimal string to decimal integer
fn parse_hex_to_decimal(hex_str string) !int {
	if hex_str.len == 0 {
		return error('Empty hexadecimal string')
	}

	mut result := 0
	for ch in hex_str {
		result *= 16
		if ch >= `0` && ch <= `9` {
			result += int(ch - `0`)
		} else if ch >= `a` && ch <= `f` {
			result += int(ch - `a` + 10)
		} else if ch >= `A` && ch <= `F` {
			result += int(ch - `A` + 10)
		} else {
			return error('Invalid hexadecimal character: ${ch.ascii_str()}')
		}
	}
	return result
}

// parse_octal_to_decimal converts an octal string to decimal integer
fn parse_octal_to_decimal(octal_str string) !int {
	if octal_str.len == 0 {
		return error('Empty octal string')
	}

	mut result := 0
	for ch in octal_str {
		result *= 8
		if ch >= `0` && ch <= `7` {
			result += int(ch - `0`)
		} else {
			return error('Invalid octal character: ${ch.ascii_str()}')
		}
	}
	return result
}

// parse_binary_to_decimal converts a binary string to decimal integer
fn parse_binary_to_decimal(binary_str string) !int {
	if binary_str.len == 0 {
		return error('Empty binary string')
	}

	mut result := 0
	for ch in binary_str {
		result *= 2
		if ch == `0` {
			result += 0
		} else if ch == `1` {
			result += 1
		} else {
			return error('Invalid binary character: ${ch.ascii_str()}')
		}
	}
	return result
}
