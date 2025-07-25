module analysis

import ast

pub struct Analyzer {
mut:
    errors []string
}

pub fn new_analyzer() Analyzer {
    return Analyzer{}
}

pub fn (mut a Analyzer) analyze(node ast.Node) !ast.Node {
    return a.analyze_node(node)
}

pub fn (a Analyzer) get_errors() []string {
    return a.errors
}

fn (mut a Analyzer) error(msg string) {
    a.errors << 'Analysis error: ${msg}'
}

fn (mut a Analyzer) analyze_node(node ast.Node) !ast.Node {
    return match node.kind {
        .module { a.analyze_module(node) }
        .function { a.analyze_function(node) }
        .integer, .float, .string, .boolean, .atom, .nil {
            a.analyze_literal(node)
        }
        else {
            error('Unsupported node type: ${node.kind}')
        }
    }
}

fn (mut a Analyzer) analyze_module(node ast.Node) !ast.Node {
    mut analyzed_children := []ast.Node{}

    // Check for duplicate function names
    mut function_names := map[string]bool{}

    for child in node.children {
        if child.kind == .function {
            func_name := child.value
            if func_name in function_names {
                a.error('Duplicate function name: ${func_name}')
            }
            function_names[func_name] = true
        }

        analyzed_child := a.analyze_node(child)!
        analyzed_children << analyzed_child
    }

    return ast.Node{
        ...node,
        children: analyzed_children
    }
}

fn (mut a Analyzer) analyze_function(node ast.Node) !ast.Node {
    if node.children.len != 1 {
        return error('Function must have exactly one body expression')
    }

    body := a.analyze_node(node.children[0])!

    // Function type is function that returns the body type
    func_type := ast.Type{
        name: 'function',
        params: [body.type_]
    }

    return ast.Node{
        ...node,
        children: [body],
        type_: func_type
    }
}

fn (mut a Analyzer) analyze_literal(node ast.Node) !ast.Node {
    // Literals already have their types set by builders
    // Just validate them
    match node.kind {
        .integer {
            // Validate integer literal
            if node.value.len == 0 {
                return error('Integer value cannot be empty')
            }
        }
        .float {
            // Validate float literal
            if node.value.len == 0 {
                return error('Float value cannot be empty')
            }
        }
        .string {
            // String is always valid
        }
        .boolean {
            // Validate boolean literal
            if node.value != 'true' && node.value != 'false' {
                return error('Invalid boolean value: ${node.value}')
            }
        }
        .atom {
            // Validate atom literal
            if node.value.len == 0 {
                return error('Atom name cannot be empty')
            }
            // Check if atom name is valid (starts with letter)
            if !node.value[0].is_letter() {
                return error('Atom name must start with letter')
            }
        }
        .nil {
            // nil is always valid
        }
        else {
            return error('Unknown literal type: ${node.kind}')
        }
    }

    return node
}