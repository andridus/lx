module generator

import ast
import strings
import analysis

@[heap]
pub struct ErlangGenerator {
mut:
	output     strings.Builder
	errors     []string
	type_table &analysis.TypeTable = unsafe { nil }
}

pub fn new_generator() ErlangGenerator {
	return ErlangGenerator{}
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
		.integer, .float, .string, .boolean, .atom, .nil {
			g.generate_literal(node)!
		}
		else {
			return error('Unsupported node type: ${node.kind}')
		}
	}
}

fn (mut g ErlangGenerator) generate_module(node ast.Node) ! {
	mod_name := if node.value.len > 0 { node.value } else { 'main' }
	g.output.write_string('-module(' + mod_name + ').\n')

	mut exports := []string{}
	for child in node.children {
		if child.kind == .function {
			func_name := child.value
			exports << '${func_name}/0'
		}
	}

	if exports.len > 0 {
		g.output.write_string('-export([${exports.join(', ')}]).\n\n')
	}

	for i, child in node.children {
		g.generate_node(child)!
		if i < node.children.len - 1 {
			g.output.write_string('\n')
		}
	}
}

fn (mut g ErlangGenerator) generate_function(node ast.Node) ! {
	func_name := node.value

	mut spec_str := ''
	if g.type_table != unsafe { nil } {
		if typ := g.type_table.get_type(node.id) {
			spec_type := type_to_erlang_spec(typ)
			spec_str = '-spec ${func_name}() -> ${spec_type}.'
			g.output.write_string(spec_str + '\n')
		}
	}

	g.output.write_string('${func_name}() ->\n')
	g.output.write_string('    ')

	if node.children.len > 0 {
		g.generate_node(node.children[0])!
	} else {
		g.output.write_string('nil')
	}

	g.output.write_string('.\n')
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
	match t.name {
		'integer' {
			return 'integer()'
		}
		'float' {
			return 'float()'
		}
		'string' {
			return 'binary()'
		}
		'boolean' {
			return 'boolean()'
		}
		'atom' {
			return 'atom()'
		}
		'nil' {
			return 'nil'
		}
		'module' {
			return 'atom()'
		}
		'any' {
			return 'any()'
		}
		'term' {
			return 'term()'
		}
		'list' {
			if t.params.len == 1 {
				return '[' + type_to_erlang_spec(t.params[0]) + ']'
			} else {
				return 'list()'
			}
		}
		'tuple' {
			if t.params.len > 0 {
				elems := t.params.map(type_to_erlang_spec).join(', ')
				return '{' + elems + '}'
			} else {
				return 'tuple()'
			}
		}
		'map' {
			if t.params.len == 2 {
				return '#{' + type_to_erlang_spec(t.params[0]) + ' => ' +
					type_to_erlang_spec(t.params[1]) + '}'
			} else {
				return 'map()'
			}
		}
		else {
			return t.name + '()'
		}
	}
}
