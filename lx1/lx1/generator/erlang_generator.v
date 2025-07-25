module generator

import ast
import strings

pub struct ErlangGenerator {
mut:
    output strings.Builder
    errors []string
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

pub fn (g ErlangGenerator) get_errors() []string {
    return g.errors
}

fn (mut g ErlangGenerator) error(msg string) {
    g.errors << 'Generation error: ${msg}'
}

fn (mut g ErlangGenerator) generate_node(node ast.Node) ! {
    match node.kind {
        .module { g.generate_module(node)! }
        .function { g.generate_function(node)! }
        .integer, .float, .string, .boolean, .atom, .nil {
            g.generate_literal(node)!
        }
        else {
            return error('Unsupported node type: ${node.kind}')
        }
    }
}

fn (mut g ErlangGenerator) generate_module(node ast.Node) ! {
    // Generate module declaration
    g.output.write_string('-module(main).\n')

    // Generate exports
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

    // Generate functions
    for i, child in node.children {
        g.generate_node(child)!
        if i < node.children.len - 1 {
            g.output.write_string('\n')
        }
    }
}

fn (mut g ErlangGenerator) generate_function(node ast.Node) ! {
    func_name := node.value

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
    return s.replace('\\', '\\\\').replace('"', '\\"').replace('\n', '\\n').replace('\t', '\\t').replace('\r', '\\r')
}