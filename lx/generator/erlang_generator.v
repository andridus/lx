module generator

import ast
import strings
import analysis
import kernel
import parser
import arrays

@[heap]
pub struct ErlangGenerator {
mut:
	output           strings.Builder
	errors           []string
	type_table       &analysis.TypeTable = unsafe { nil }
	var_map          map[string]string // Maps original var names to unique Erlang names
	next_hash        int = 1
	directives_table &parser.DirectivesTable
	in_pattern       bool // Track if we're in pattern matching context
	// New fields for .hrl file generation
	hrl_output  strings.Builder
	module_name string
	has_records bool
}

pub fn new_generator(directives_table &parser.DirectivesTable) ErlangGenerator {
	return ErlangGenerator{
		directives_table: directives_table
		var_map:          map[string]string{}
		hrl_output:       strings.new_builder(1024)
		module_name:      ''
		has_records:      false
	}
}

pub fn (mut g ErlangGenerator) generate(node ast.Node) !string {
	g.output = strings.new_builder(1024)
	g.hrl_output = strings.new_builder(1024)
	g.errors = []
	g.has_records = false

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

// Get the generated .hrl file content
pub fn (mut g ErlangGenerator) get_hrl_content() string {
	return g.hrl_output.str()
}

// Check if the generator has records (and thus generated a .hrl file)
pub fn (mut g ErlangGenerator) has_generated_records() bool {
	return g.has_records
}

// Set the module name for .hrl inclusion
pub fn (mut g ErlangGenerator) set_module_name(name string) {
	g.module_name = name
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
		.integer, .float, .string, .string_charlist, .boolean, .atom, .nil {
			g.generate_literal(node)!
		}
		.function_caller {
			g.generate_function_caller(node)!
		}
		.external_function_call {
			g.generate_external_function_call(node)!
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
			// Records are now handled by .hrl files, skip direct generation
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
		// .function_parameter {
		// 	g.generate_function_parameter(node)!
		// }
		.lambda_expression {
			g.generate_anonymous_function(node)!
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
	// Only set module_name if it hasn't been set externally (for .hrl inclusion)
	if g.module_name == '' {
		g.module_name = module_name
	}
	g.output.write_string('-module(${module_name}).\n')
	// Generate module-level directives (@moduledoc)
	moduledoc := g.directives_table.get_moduledoc()
	if moduledoc.len > 0 {
		g.output.write_string('-moduledoc "${moduledoc}" .\n')
	}

	// If this module wraps a supervisor or worker definition, delegate and return
	for child in node.children {
		if child.kind == .supervisor_def {
			g.generate_supervisor_def(child)!
			return
		}
		if child.kind == .worker_def {
			g.generate_worker_def(child)!
			return
		}
	}

	// Collect function exports
	mut exports := []string{}
	for child in node.children {
		if child.kind == .function {
			if function_type := g.type_table.get_function_type(child.value) {
				if function_type.public {
					for head in function_type.heads {
						exports << '${child.value}/${head.patterns.len}'
					}
				}
			}
		}
	}

	if exports.len > 0 {
		g.output.write_string('-export([${arrays.uniq(exports).join(', ')}]).\n\n')
	}

	// Module system comments (deps/import/application)
	mut comments_emitted := false
	for child in node.children {
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

	// Collect records and custom types by dependencies
	mut records := []ast.Node{}
	mut custom_types := []ast.Node{}
	for child in node.children {
		if child.kind == .record_definition {
			records << child
		} else if child.kind == .type_def || child.kind == .opaque_type
			|| child.kind == .nominal_type {
			custom_types << child
		}
	}

	// If we have records or custom types, generate .hrl file and include it
	if records.len > 0 || custom_types.len > 0 {
		g.has_records = true
		g.generate_hrl_file_with_types(records, custom_types)!
		g.output.write_string('-include("${g.module_name}.hrl").\n\n')
	} else if g.module_name != '' && g.module_name != node.value {
		// This module needs to include .hrl from another module
		g.output.write_string('-include("${g.module_name}.hrl").\n\n')
	}

	// Generate type definitions only if they won't be in .hrl
	// If we have records or custom types, they are generated in .hrl, so skip here
	if records.len == 0 && custom_types.len == 0 {
		for child in node.children {
			if child.kind == .type_def {
				g.generate_type_def(child)!
			} else if child.kind == .opaque_type {
				g.generate_opaque_type(child)!
			} else if child.kind == .nominal_type {
				g.generate_nominal_type(child)!
			}
		}
	}

	// Generate function definitions
	for child in node.children {
		if child.kind == .function || child.kind == .private_function {
			g.generate_function(child)!
		}
	}
}

fn (mut g ErlangGenerator) generate_function(node ast.Node) ! {
	function_name := node.value
	has_function_heads := node.children[0].kind == .function
	mut fn_sorted_by_arity := map[int][]ast.Node{}

	if has_function_heads {
		for head in node.children {
			fn_sorted_by_arity[head.children[0].children.len] << head
		}
	} else {
		fn_sorted_by_arity[node.children[0].children.len] << node
	}

	mut ending := '.'
	for arity, heads in fn_sorted_by_arity {
		function_key := '${function_name}/${arity}'
		// Get and generate directives for this function
		directive := g.directives_table.get_doc(function_key)
		if directive.kind == .string {
			g.output.write_string('-doc "${directive.value}".\n')
		}

		if _ := g.type_table.get_function_type(function_name) {
			mut arg_types := map[int][]string{}
			for i in 0 .. arity {
				for head in heads {
					arg := head.children[0].children[i]
					if type_ := g.type_table.get_type(arg.id) {
						arg_types[i] << type_to_erlang_spec(type_)
					} else {
						arg_types[i] << 'any()'
					}
				}
			}
			g.output.write_string('-spec ${function_name}(')
			mut args_str := []string{}
			for _, value in arg_types {
				if value.len > 1 {
					if value.contains('any()') {
						args_str << 'any()'
					} else {
						value1 := arrays.uniq(value).filter(it != 'empty_list()')
						if value1.len == 0 {
							args_str << 'list()'
						} else if value1.len == 1 {
							args_str << value1
						} else {
							args_str << '(' + value1.join(' | ') + ')'
						}
					}
				} else {
					args_str << value[0]
				}
			}
			g.output.write_string(args_str.join(', '))
			g.output.write_string(') -> ${type_to_erlang_spec(node.type)}.\n')
		}

		mut index := 0
		for head in heads {
			if index < heads.len - 1 {
				ending = ';\n'
			} else {
				ending = '.\n'
			}
			g.generate_single_function(function_name, head.children[0], head.children[1],
				ending)!
			index++
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
		.string_charlist {
			escaped := g.escape_string(node.value)
			g.output.write_string('"${escaped}"')
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
				mapped := t.params.map(type_to_erlang_spec)
				if mapped.any(it == 'any()') {
					'any()'
				} else {
					arrays.uniq(mapped).join(' | ')
				}
			} else {
				'any()'
			}
		}
		'binary' {
			'binary()'
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
			// For function specs, prefer atom() over specialized atoms
			// unless it's in a union type context
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
			// Check if this is a single letter uppercase (generic type variable)
			if t.name.len == 1 && t.name[0].is_capital() {
				// Generic type variable like T, U, V
				t.name
			} else if t.name.len > 0 && t.name[0].is_capital() {
				// Record type (should be converted to lowercase)
				'#${t.name.to_lower()}{}'
			} else if t.name.len == 0 {
				'any()'
			} else {
				// Check if this is a record type
				if specialized := t.specialized_value {
					if specialized == 'record_type' {
						'#${t.name}{}'
					} else if specialized == 'custom_type' {
						// Custom types always get () in function specs
						t.name + '()'
					} else {
						// For simple identifiers in type definitions, don't add ()
						// This handles cases like "type status :: active" where active is just an atom name
						t.name
					}
				} else {
					// Custom type reference with parameters
					result := t.name + '()'
					result
				}
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
		.integer, .float, .string, .string_charlist, .boolean, .atom, .nil {
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
		.external_function_call {
			return g.generate_external_function_call_to_string(node)
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
		.string_charlist {
			escaped := g.escape_string(node.value)
			return '"${escaped}"'
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

fn (mut g ErlangGenerator) generate_external_function_call_to_string(node ast.Node) !string {
	// Parse module:function from the value field
	parts := node.value.split(':')
	if parts.len != 2 {
		return error('Invalid external function call format: ${node.value}')
	}

	module_name := parts[0]
	function_name := parts[1]

	// Generate Erlang code: Module:Function(Args)
	mut result := '${module_name}:${function_name}('

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

	child := node.children[0]

	// Don't add parentheses around simple identifiers (including those with type annotations)
	if child.kind == .identifier {
		g.generate_node(child)!
		return
	}

	g.output.write_string('(')
	g.generate_node(child)!
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
	if node.children[0].kind == .parentheses {
		if node.children[0].children.len == 1
			&& node.children[0].children[0].kind == .type_annotation {
			g.generate_node(node.children[0].children[0].children[0])!
		} else {
			g.generate_node(node.children[0])!
		}
	} else {
		g.generate_node(node.children[0])!
	}
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

		// Use := for pattern matching, => for map creation/update
		if g.in_pattern {
			g.output.write_string(' := ')
		} else {
			g.output.write_string(' => ')
		}

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

		// Use := for pattern matching, => for map creation/update
		if g.in_pattern {
			result += ' := '
		} else {
			result += ' => '
		}

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

// Record generation functions - now handled by .hrl files

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

fn (mut g ErlangGenerator) generate_single_function(function_name string, args_block ast.Node, body ast.Node, ending string) ! {
	// Generate function signature
	g.output.write_string('${function_name}(')

	// Generate arguments
	mut guard_from_args := map[string]ast.Type{}
	if args_block.kind == .block {
		for i, arg in args_block.children {
			if i > 0 {
				g.output.write_string(', ')
			}
			// Generate argument as variable with unique hash
			if arg.kind == .identifier {
				unique_name := g.get_unique_var_name(arg.value)
				g.output.write_string(unique_name)
				if type_ := g.type_table.get_type(arg.id) {
					guard_from_args[unique_name] = type_
				}
			} else if arg.kind == .type_annotation {
				unique_name := g.get_unique_var_name(arg.children[0].value)
				g.output.write_string(unique_name)
				if type_ := g.type_table.get_type(arg.id) {
					guard_from_args[unique_name] = type_
				}
			} else {
				g.generate_node(arg)!
			}
		}
	}

	g.output.write_string(') ')
	// when type is defined set the guard for the function
	guard := g.generate_guard_from_args(args_block)!
	if guard.len > 0 {
		g.output.write_string('when ${guard} ->\n    ')
	} else {
		g.output.write_string('->\n    ')
	}

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

	g.output.write_string(ending)
}

fn (mut g ErlangGenerator) generate_guard_from_args(args_block ast.Node) !string {
	// Returns a string with the appropriate Erlang guards for each argument in the block
	mut guards := []string{}
	if args_block.kind == .block {
		for arg in args_block.children {
			mut arg1 := arg
			if arg.kind == .type_annotation {
				arg1 = arg.children[0]
			}
			if arg1.kind == .identifier {
				unique_name := g.get_unique_var_name(arg1.value)
				if type_ := g.type_table.get_type(arg.id) {
					guard := match type_.name {
						'integer' {
							'is_integer(${unique_name})'
						}
						'float' {
							'is_float(${unique_name})'
						}
						'number' {
							'is_number(${unique_name})'
						}
						'atom' {
							'is_atom(${unique_name})'
						}
						'binary' {
							'is_binary(${unique_name})'
						}
						'string' {
							'is_binary(${unique_name})'
						}
						'bitstring' {
							'is_bitstring(${unique_name})'
						}
						'boolean' {
							'is_boolean(${unique_name})'
						}
						'function' {
							'is_function(${unique_name})'
						}
						'list' {
							'is_list(${unique_name})'
						}
						'map' {
							'is_map(${unique_name})'
						}
						'pid' {
							'is_pid(${unique_name})'
						}
						'port' {
							'is_port(${unique_name})'
						}
						'reference' {
							'is_reference(${unique_name})'
						}
						'tuple' {
							'is_tuple(${unique_name})'
						}
						// For record, we need record name, which is in params[0] if present
						'record' {
							if type_.params.len > 0 {
								rec_name := type_.params[0].name
								'is_record(${unique_name}, ${rec_name})'
							} else {
								'is_record(${unique_name})'
							}
						}
						else {
							''
						}
					}
					if guard != '' {
						guards << guard
					}
				}
			}
		}
	}
	return guards.join(' andalso ')
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

// fn (mut g ErlangGenerator) generate_function_parameter(node ast.Node) ! {
// 	// Check if this parameter has a value (identifier) or is a pattern
// 	if node.value != '' {
// 		// This is an identifier parameter (with or without type annotation)
// 		unique_name := g.get_unique_var_name(node.value)
// 		g.output.write_string(unique_name)
// 	} else if node.children.len > 0 {
// 		// This is a pattern parameter (atom, tuple, list, etc.)
// 		g.generate_node(node.children[0])!
// 	} else {
// 		return error('Invalid function parameter: no value or pattern')
// 	}
// }

// fn (mut g ErlangGenerator) generate_lambda_expression(node ast.Node) ! {
// 	if node.children.len < 1 {
// 		return error('Lambda expression must have body')
// 	}

// 	body := node.children[node.children.len - 1]

// 	// Check if this is a multi-head lambda (body is a block with function heads)
// 	if body.kind == .block && body.children.len > 0 && body.children[0].kind == .function {
// 		// Multi-head lambda: fun ... end with clauses
// 		g.output.write_string('fun\n')

// 		for i, head in body.children {
// 			g.output.write_string('        ')
// 			g.generate_function_clause(head)!
// 			if i < body.children.len - 1 {
// 				g.output.write_string(';\n')
// 			} else {
// 				g.output.write_string('\n')
// 			}
// 		}

// 		g.output.write_string('    end')
// 		return
// 	}

// 	// Regular lambda: fun(params) -> body end
// 	g.output.write_string('fun(')

// 	// Generate parameters (all children except the last one, which is the body)
// 	params := if node.children.len > 1 {
// 		node.children[0..node.children.len - 1]
// 	} else {
// 		[]ast.Node{}
// 	}
// 	for i, param in params {
// 		g.generate_node(param)!
// 		if i < params.len - 1 {
// 			g.output.write_string(', ')
// 		}
// 	}

// 	g.output.write_string(') ->\n        ')

// 	// Generate body
// 	g.generate_node(body)!

// 	g.output.write_string('\n    end')
// }

fn (mut g ErlangGenerator) generate_function_clause(node ast.Node) ! {
	// Generate function clause for lambda (without function name)
	if node.children.len >= 2 {
		args_block := node.children[0]
		body := node.children[1]
		guard := if node.children.len >= 3 { node.children[2] } else { ast.Node{} }

		// Generate arguments
		g.output.write_string('(')
		if args_block.kind == .block {
			for j, arg in args_block.children {
				if j > 0 {
					g.output.write_string(', ')
				}
				// Generate argument as pattern
				g.in_pattern = true
				g.generate_node(arg)!
				g.in_pattern = false
			}
		} else {
			// Single argument
			g.in_pattern = true
			g.generate_node(args_block)!
			g.in_pattern = false
		}
		g.output.write_string(')')

		// Generate guard if present
		if guard.id != 0 {
			g.output.write_string(' when ')
			g.generate_node(guard)!
		}

		g.output.write_string(' ->\n            ')

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

	if pattern.kind == .block {
		for child in pattern.children {
			g.generate_pattern(child)!
		}
	} else {
		g.generate_pattern(pattern)!
	}

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

// Generate case clause specifically for lambda functions (multi-head)
fn (mut g ErlangGenerator) generate_case_clause_for_lambda(node ast.Node, arity int) ! {
	if node.children.len < 2 {
		return error('Case clause must have pattern and body')
	}

	pattern := node.children[0]
	body := node.children[1]
	guard := if node.children.len >= 3 { node.children[2] } else { ast.Node{} }

	// Generate pattern - for lambda, we need to handle tuple patterns for multi-arity
	if arity == 1 {
		// Single argument - generate pattern directly
		g.in_pattern = true
		g.generate_node(pattern)!
		g.in_pattern = false
	} else {
		// Multiple arguments - generate tuple pattern
		if pattern.kind == .block {
			g.output.write_string('{')
			for i, arg_pattern in pattern.children {
				if i > 0 {
					g.output.write_string(', ')
				}
				g.in_pattern = true
				g.generate_node(arg_pattern)!
				g.in_pattern = false
			}
			g.output.write_string('}')
		} else {
			// Single pattern for multiple args - wrap in tuple
			g.output.write_string('{')
			g.in_pattern = true
			g.generate_node(pattern)!
			g.in_pattern = false
			g.output.write_string('}')
		}
	}

	// Generate guard if present
	if guard.id != 0 {
		g.output.write_string(' when ')
		g.generate_node(guard)!
	}

	g.output.write_string(' ->\n        ')

	// Generate body
	g.generate_node(body)!
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
		.binary_literal {
			g.generate_binary_literal(node)!
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
	g.output.write_string('case ')
	g.generate_node(node.children[1])! // expression
	g.output.write_string(' of\n        ')
	g.generate_pattern(node.children[0])! // pattern
	g.output.write_string(' ->\n            ')
	g.generate_node(node.children[2])! // continuation
	g.output.write_string(';\n        ')
	if node.children.len == 4 && node.children[3].kind == .tuple_literal {
		rescue_expr := node.children[3]
		g.generate_node(rescue_expr.children[0])!
		g.output.write_string(' ->\n            ')
		g.generate_node(rescue_expr.children[1])!
	} else {
		g.output.write_string('Error ->\n            Error')
	}
	g.output.write_string('\n    end')
}

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

// Generate supervisor definitions (as proper OTP supervisor modules)
fn (mut g ErlangGenerator) generate_supervisor_def(node ast.Node) ! {
	if node.children.len != 1 {
		return error('Supervisor definition must have body')
	}

	// Generate standard supervisor callbacks
	g.output.write_string('-behaviour(supervisor).\n\n')
	g.output.write_string('-export([start_link/0, init/1]).\n\n')

	// Include .hrl file if module_name is set (indicating records exist)
	if g.module_name != '' {
		g.output.write_string('-include("${g.module_name}.hrl").\n\n')
	}

	// start_link/0
	g.output.write_string('start_link() ->\n')
	g.output.write_string('    supervisor:start_link({local, ?MODULE}, ?MODULE, []).\n\n')

	// init/1 - parse supervisor body for strategy and children
	g.output.write_string('init([]) ->\n')

	// Parse supervisor body for strategy and children
	body := node.children[0]
	mut strategy := 'one_for_one'
	mut children_list := []string{}

	// Extract strategy and children from body, and collect functions
	mut supervisor_functions := []ast.Node{}
	for child in body.children {
		if child.kind == .variable_binding {
			if child.value == 'strategy' && child.children.len > 0 {
				strategy_node := child.children[0]
				if strategy_node.kind == .atom {
					strategy = strategy_node.value
				}
			} else if child.value == 'children' && child.children.len > 0 {
				children_node := child.children[0]
				if children_node.kind == .list_literal {
					for child_ref in children_node.children {
						if child_ref.kind == .atom {
							child_name := child_ref.value
							children_list << '{${child_name}, {${child_name}, start_link, []}, permanent, 5000, worker, [${child_name}]}'
						}
					}
				}
			}
		} else if child.kind == .function || child.kind == .private_function {
			// Collect functions defined in supervisor
			supervisor_functions << child
		}
	}

	g.output.write_string('    Strategy = ${strategy},\n')
	g.output.write_string('    Children = [\n')
	for i, child_spec in children_list {
		g.output.write_string('        ${child_spec}')
		if i < children_list.len - 1 {
			g.output.write_string(',')
		}
		g.output.write_string('\n')
	}
	g.output.write_string('    ],\n')
	g.output.write_string('    {ok, {{Strategy, 5, 10}, Children}}.\n\n')
}

// Generate worker definitions (as proper OTP gen_server modules)
fn (mut g ErlangGenerator) generate_worker_def(node ast.Node) ! {
	if node.children.len != 1 {
		return error('Worker definition must have body')
	}

	// Generate standard gen_server callbacks
	g.output.write_string('-behaviour(gen_server).\n\n')

	// Collect user-defined public functions for export
	body := node.children[0]
	mut user_exports := []string{}

	for child in body.children {
		if child.kind == .function {
			func_name := child.value
			// Skip callbacks, export other public functions
			if func_name !in ['init', 'handle_call', 'handle_cast', 'handle_info', 'terminate',
				'code_change', 'start_link'] {
				// Calculate arity
				mut arity := 0
				if child.children.len > 0 {
					args_block := child.children[0]
					if args_block.kind == .block {
						arity = args_block.children.len
					}
				}
				user_exports << '${func_name}/${arity}'
			}
		}
	}

	// Export standard callbacks
	g.output.write_string('-export([start_link/0, start_link/1]).\n')
	g.output.write_string('-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).\n')

	// Export user-defined functions
	if user_exports.len > 0 {
		g.output.write_string('-export([${user_exports.join(', ')}]).\n')
	}
	g.output.write_string('\n')

	// Include .hrl file if module_name is set (indicating records exist)
	if g.module_name != '' {
		g.output.write_string('-include("${g.module_name}.hrl").\n\n')
	}

	// start_link functions
	g.output.write_string('start_link() ->\n')
	g.output.write_string('    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).\n\n')
	g.output.write_string('start_link(Args) ->\n')
	g.output.write_string('    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).\n\n')

	// Parse worker body for user-defined callbacks
	mut has_init := false
	mut has_handle_call := false
	mut has_handle_cast := false
	mut has_handle_info := false

	// Generate user-defined functions from body
	for child in body.children {
		if child.kind == .function {
			func_name := child.value
			match func_name {
				'init' {
					has_init = true
					g.generate_function(child)!
				}
				'handle_call' {
					has_handle_call = true
					g.generate_function(child)!
				}
				'handle_cast' {
					has_handle_cast = true
					g.generate_function(child)!
				}
				'handle_info' {
					has_handle_info = true
					g.generate_function(child)!
				}
				'start_link' {
					// Skip - we generate our own start_link
				}
				else {
					// Generate other functions as normal
					g.generate_function(child)!
				}
			}
		}
	}

	// Generate default callbacks if not provided
	if !has_init {
		g.output.write_string('init(Args) ->\n')
		g.output.write_string('    {ok, #{}}.\n\n')
	}
	if !has_handle_call {
		g.output.write_string('handle_call(_Req, _From, State) ->\n')
		g.output.write_string('    {reply, ok, State}.\n\n')
	}
	if !has_handle_cast {
		g.output.write_string('handle_cast(_Msg, State) ->\n')
		g.output.write_string('    {noreply, State}.\n\n')
	}
	if !has_handle_info {
		g.output.write_string('handle_info(_Info, State) ->\n')
		g.output.write_string('    {noreply, State}.\n\n')
	}

	// Always generate terminate and code_change
	g.output.write_string('terminate(_Reason, _State) ->\n')
	g.output.write_string('    ok.\n\n')
	g.output.write_string('code_change(_OldVsn, State, _Extra) ->\n')
	g.output.write_string('    {ok, State}.\n\n')
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
	// Extract base type name for lookup (remove generic parameters)
	mut base_type_name := node.value
	if node.value.contains('(') {
		base_type_name = node.value.split('(')[0]
	}

	// Check if we have a type table to get the actual type definition
	if g.type_table != unsafe { nil } {
		if custom_type := g.type_table.get_custom_type(base_type_name) {
			// Generate Erlang -type declaration using the full name with parameters
			// Add () if no parameters are present
			mut type_declaration := node.value
			if !type_declaration.contains('(') {
				type_declaration += '()'
			}
			g.output.write_string('-type ${type_declaration} :: ${type_to_erlang_spec(custom_type)}.\n')
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
	// Children come in pairs [key_atom, value_expr, ...]
	if node.children.len == 0 {
		// Ensure a blank line when the application block is empty
		g.output.write_string('\n')
		return
	}
	for i := 0; i < node.children.len; i += 2 {
		g.output.write_string('%%  ')
		// key
		key_node := node.children[i]
		key_str := g.generate_node_to_string(key_node) or { 'unknown' }
		g.output.write_string(key_str)
		g.output.write_string(': ')
		// value
		val_node := node.children[i + 1]
		val_str := g.generate_node_to_string(val_node) or { 'unknown' }
		g.output.write_string(val_str)
		g.output.write_string('\n')
	}
}

// Generate import statements (as comments)
fn (mut g ErlangGenerator) generate_import_statement(node ast.Node) ! {
	g.output.write_string('%% Import: ${node.value}\n')
}

// ============ Task 11: Advanced Features Generation ============

// Generate string interpolation (as iolist_to_binary with format)
fn (mut g ErlangGenerator) generate_string_interpolation(node ast.Node) ! {
	// Build format string and arguments
	mut format_parts := []string{}
	mut args := []string{}

	for segment in node.children {
		if segment.kind == .string {
			// String literals - add to format string
			format_parts << segment.value
		} else {
			// Variables - add ~p placeholder and argument
			format_parts << '~p'
			mut arg_code := g.generate_node_to_string(segment)!
			args << arg_code
		}
	}

	format_string := format_parts.join('')

	g.output.write_string('iolist_to_binary(io_lib:format("${format_string}", [')
	g.output.write_string(args.join(', '))
	g.output.write_string(']))')
}

// Generate anonymous functions
fn (mut g ErlangGenerator) generate_anonymous_function(node ast.Node) ! {
	if node.children.len == 0 {
		return error('Anonymous function must have at least a body')
	}

	// Check if this is a multi-head lambda (body is a block with case clauses)
	body := node.children[node.children.len - 1]
	if body.kind == .block && body.children.len > 0 && body.children[0].kind == .case_clause {
		// Multi-head lambda: fn do (p1) -> b1; (p2) -> b2 end
		// Generate as: fun(Args) -> case Args of Pattern1 -> Body1; Pattern2 -> Body2 end end

		// Determine arity based on the first clause pattern
		first_clause := body.children[0]
		if first_clause.children.len >= 1 {
			pattern := first_clause.children[0]
			arity := if pattern.kind == .block { pattern.children.len } else { 1 }

			// Generate function head with appropriate arity
			if arity == 1 {
				g.output.write_string('fun(Arg) -> ')
			} else {
				g.output.write_string('fun(')
				for i in 0 .. arity {
					if i > 0 {
						g.output.write_string(', ')
					}
					g.output.write_string('Arg${i + 1}')
				}
				g.output.write_string(') -> ')
			}

			// Generate case expression
			if arity == 1 {
				g.output.write_string('case Arg of\n')
			} else {
				g.output.write_string('case {')
				for i in 0 .. arity {
					if i > 0 {
						g.output.write_string(', ')
					}
					g.output.write_string('Arg${i + 1}')
				}
				g.output.write_string('} of\n')
			}

			// Generate case clauses
			for i, clause in body.children {
				if clause.kind == .case_clause {
					g.output.write_string('    ')
					g.generate_case_clause_for_lambda(clause, arity)!
					if i < body.children.len - 1 {
						g.output.write_string(';\n')
					} else {
						g.output.write_string('\n')
					}
				}
			}

			g.output.write_string('end')
		}
	} else {
		// Regular single-head lambda: fn(params) -> body end
		caller := node.children[node.children.len - 1]
		if caller.kind == .function_caller && node.kind != .lambda_expression {
			g.output.write_string('fun ?MODULE:')
			g.output.write_string(caller.value)
			g.output.write_string('/${caller.children.len}')
			return
		} else if caller.kind == .external_function_call {
			g.output.write_string('fun ')
			g.output.write_string(caller.value)
			g.output.write_string('/${caller.children.len}')
			return
		}

		g.output.write_string('fun(')

		// Parameters (all children except the last one)
		for i in 0 .. node.children.len - 1 {
			if node.children[i].children.len > 0 {
				g.generate_node(node.children[i].children[0])!
				if i < node.children.len - 2 {
					g.output.write_string(', ')
				}
			} else {
				g.generate_node(node.children[i])!
			}
		}

		g.output.write_string(') -> ')
		g.generate_node(body)!
	}

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
	if node.kind == .string_charlist {
		g.output.write_string('"${escaped}"')
	} else {
		g.output.write_string('<<"${escaped}"/utf8>>')
	}
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

fn (mut g ErlangGenerator) generate_external_function_call(node ast.Node) ! {
	// Parse module:function from the value field
	parts := node.value.split(':')
	if parts.len != 2 {
		return error('Invalid external function call format: ${node.value}')
	}

	module_name := parts[0]
	function_name := parts[1]

	// Generate Erlang code: Module:Function(Args)
	g.output.write_string('${module_name}:${function_name}(')

	for i, arg in node.children {
		if i > 0 {
			g.output.write_string(', ')
		}
		g.generate_node(arg)!
	}

	g.output.write_string(')')
}

// Convert an AST node representing a type expression into an ast.Type recursively
fn (g ErlangGenerator) field_type_node_to_type(node ast.Node) ast.Type {
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
			variant_types << g.field_type_node_to_type(child)
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
			element_types << g.field_type_node_to_type(child)
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
				element_types << g.field_type_node_to_type(child)
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
				param_types << g.field_type_node_to_type(child)
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

		// For other types (records, custom types), return as is
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

// Record dependency analysis and ordering
fn (g ErlangGenerator) analyze_record_dependencies(records []ast.Node) []ast.Node {
	if records.len <= 1 {
		return records
	}

	// Build dependency graph
	mut dependencies := map[string][]string{}
	mut record_map := map[string]ast.Node{}

	// Initialize record map and dependencies
	for record in records {
		record_name := record.value.to_lower()
		record_map[record_name] = record
		dependencies[record_name] = []
	}

	// Analyze dependencies by looking at field types
	for record in records {
		record_name := record.value.to_lower()

		for field in record.children {
			if field.kind != .record_field {
				continue
			}

			// Check field type for record references
			if field.children.len > 0 {
				field_type_node := field.children[0]
				field_type_name := g.extract_type_name_from_field_type(field_type_node)

				// If field type is a record type, add dependency
				if field_type_name != '' && field_type_name in record_map {
					if field_type_name != record_name { // Avoid self-reference
						dependencies[record_name] << field_type_name
					}
				}
			}
		}
	}

	// Topological sort using Kahn's algorithm
	return g.topological_sort_records(records, dependencies, record_map)
}

fn (g ErlangGenerator) extract_type_name_from_field_type(field_type_node ast.Node) string {
	match field_type_node.kind {
		.identifier {
			// Direct type reference like "BankAccount"
			// Check if it's a parameterized type like "list(BankAccount)"
			if field_type_node.children.len > 0 {
				// This is a parameterized type, extract the first parameter
				first_param := field_type_node.children[0]
				if first_param.kind == .identifier {
					return first_param.value.to_lower()
				}
			}
			// Direct type reference
			return field_type_node.value.to_lower()
		}
		else {}
	}
	return ''
}

fn (g ErlangGenerator) topological_sort_records(records []ast.Node, dependencies map[string][]string, record_map map[string]ast.Node) []ast.Node {
	mut sorted := []ast.Node{}
	mut in_degree := map[string]int{}

	// Calculate in-degree for each record
	for record_name, deps in dependencies {
		in_degree[record_name] = deps.len
	}

	// Find records with no dependencies (in-degree = 0)
	mut queue := []string{}
	for record_name, degree in in_degree {
		if degree == 0 {
			queue << record_name
		}
	}

	// Process queue
	for queue.len > 0 {
		current := queue[0]
		queue.delete(0)

		// Add to sorted list
		if current in record_map {
			sorted << record_map[current]
		}

		// Reduce in-degree for dependent records
		for record_name, deps in dependencies {
			if current in deps {
				in_degree[record_name]--
				if in_degree[record_name] == 0 {
					queue << record_name
				}
			}
		}
	}

	// If we couldn't sort all records, add remaining ones (circular dependencies)
	for record_name, _ in record_map {
		if record_name !in sorted.map(it.value.to_lower()) {
			if record_name in record_map {
				sorted << record_map[record_name]
			}
		}
	}

	return sorted
}

// Generate .hrl file with record definitions and custom types
fn (mut g ErlangGenerator) generate_hrl_file_with_types(records []ast.Node, custom_types []ast.Node) ! {
	// Add header comment
	g.hrl_output.write_string('%% ${g.module_name}.hrl - Record definitions\n\n')

	// Generate custom type definitions first
	for custom_type in custom_types {
		g.generate_type_def_hrl(custom_type)!
	}

	// Add blank line if we have both custom types and records
	if custom_types.len > 0 && records.len > 0 {
		g.hrl_output.write_string('\n')
	}

	// Sort records by dependencies (records with no dependencies first)
	sorted_records := g.analyze_record_dependencies(records)

	// Generate record definitions in dependency order
	for record in sorted_records {
		g.generate_record_definition_hrl(record)!
	}
}

// Generate .hrl file with record definitions (legacy method)
fn (mut g ErlangGenerator) generate_hrl_file(records []ast.Node) ! {
	g.generate_hrl_file_with_types(records, [])!
}

// Generate type definition for .hrl file
fn (mut g ErlangGenerator) generate_type_def_hrl(node ast.Node) ! {
	// Extract base type name for lookup (remove generic parameters)
	mut base_type_name := node.value
	if node.value.contains('(') {
		base_type_name = node.value.split('(')[0]
	}

	// Check if we have a type table to get the actual type definition
	if g.type_table != unsafe { nil } {
		if custom_type := g.type_table.get_custom_type(base_type_name) {
			// Generate Erlang -type declaration using the full name with parameters
			// Add () if no parameters are present
			mut type_declaration := node.value
			if !type_declaration.contains('(') {
				type_declaration += '()'
			}
			type_spec := type_to_erlang_spec(custom_type)
			g.hrl_output.write_string('-type ${type_declaration} :: ${type_spec}.\n')
			return
		}
	}

	// Fallback to comment if no type information available
	g.hrl_output.write_string('%% Type definition: ${node.value}\n')
}

// Generate record definition for .hrl file
fn (mut g ErlangGenerator) generate_record_definition_hrl(node ast.Node) ! {
	record_name := node.value.to_lower() // Convert to lowercase for Erlang convention

	// Generate record definition header
	g.hrl_output.write_string('-record(${record_name}, {')

	// Generate field definitions
	for i, field in node.children {
		if i > 0 {
			g.hrl_output.write_string(', ')
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
			// Use explicit type - convert the field_type_node to ast.Type properly
			field_type = g.field_type_node_to_type(field_type_node)
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
			g.hrl_output.write_string('${field_name} = ')
			g.generate_node_to_hrl(default_value)!
			g.hrl_output.write_string(' :: ${erlang_type}')
		} else {
			// Field without default value
			g.hrl_output.write_string('${field_name} = nil :: ${erlang_type}')
		}
	}

	g.hrl_output.write_string('}).\n\n')
}

// Helper function to generate nodes to .hrl output
fn (mut g ErlangGenerator) generate_node_to_hrl(node ast.Node) ! {
	match node.kind {
		.integer, .float, .string, .string_charlist, .boolean, .atom, .nil {
			g.generate_literal_to_hrl(node)!
		}
		.variable_ref {
			g.generate_variable_ref_to_hrl(node)!
		}
		.identifier {
			g.generate_identifier_to_hrl(node)!
		}
		.list_literal {
			g.generate_list_literal_to_hrl(node)!
		}
		.tuple_literal {
			g.generate_tuple_literal_to_hrl(node)!
		}
		.map_literal {
			g.generate_map_literal_to_hrl(node)!
		}
		else {
			return error('Unsupported node type for .hrl generation: ${node.kind}')
		}
	}
}

// Helper functions for .hrl generation
fn (mut g ErlangGenerator) generate_literal_to_hrl(node ast.Node) ! {
	match node.kind {
		.integer, .float, .boolean, .atom {
			g.hrl_output.write_string(node.value)
		}
		.string {
			escaped := g.escape_string(node.value)
			g.hrl_output.write_string('<<"${escaped}"/utf8>>')
		}
		.string_charlist {
			escaped := g.escape_string(node.value)
			g.hrl_output.write_string('"${escaped}"')
		}
		.nil {
			g.hrl_output.write_string('nil')
		}
		else {
			return error('Unknown literal type: ${node.kind}')
		}
	}
}

fn (mut g ErlangGenerator) generate_variable_ref_to_hrl(node ast.Node) ! {
	original_name := node.value
	unique_name := g.get_unique_var_name(original_name)
	g.hrl_output.write_string(unique_name)
}

fn (mut g ErlangGenerator) generate_identifier_to_hrl(node ast.Node) ! {
	original_name := node.value
	unique_name := g.get_unique_var_name(original_name)
	g.hrl_output.write_string(unique_name)
}

fn (mut g ErlangGenerator) generate_list_literal_to_hrl(node ast.Node) ! {
	if node.children.len == 0 {
		g.hrl_output.write_string('[]')
		return
	}

	g.hrl_output.write_string('[')
	for i, element in node.children {
		if i > 0 {
			g.hrl_output.write_string(', ')
		}
		g.generate_node_to_hrl(element)!
	}
	g.hrl_output.write_string(']')
}

fn (mut g ErlangGenerator) generate_tuple_literal_to_hrl(node ast.Node) ! {
	if node.children.len == 0 {
		g.hrl_output.write_string('{}')
		return
	}

	g.hrl_output.write_string('{')
	for i, element in node.children {
		if i > 0 {
			g.hrl_output.write_string(', ')
		}
		g.generate_node_to_hrl(element)!
	}
	g.hrl_output.write_string('}')
}

fn (mut g ErlangGenerator) generate_map_literal_to_hrl(node ast.Node) ! {
	if node.children.len == 0 {
		g.hrl_output.write_string('#{}')
		return
	}

	g.hrl_output.write_string('#{')
	for i := 0; i < node.children.len; i += 2 {
		if i > 0 {
			g.hrl_output.write_string(', ')
		}

		// Generate key
		key := node.children[i]
		g.generate_node_to_hrl(key)!

		// Use => for map creation in .hrl
		g.hrl_output.write_string(' => ')

		// Generate value
		value := node.children[i + 1]
		g.generate_node_to_hrl(value)!
	}
	g.hrl_output.write_string('}')
}
