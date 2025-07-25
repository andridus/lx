module ast

pub struct IDGenerator {
mut:
    next_id int
}

pub fn (mut gen IDGenerator) next() int {
    id := gen.next_id
    gen.next_id++
    return id
}

// Simple ID generator - just use incremental IDs for now
fn next_id() int {
    return 1 // For Task 1, we'll use simple IDs
}

// Builders para cada tipo de literal
pub fn new_integer(value int, pos Position) Node {
    return Node{
        id: next_id(),
        kind: .integer,
        value: value.str(),
        position: pos,
        type_: Type{name: 'integer'}
    }
}

pub fn new_float(value f64, pos Position) Node {
    return Node{
        id: next_id(),
        kind: .float,
        value: value.str(),
        position: pos,
        type_: Type{name: 'float'}
    }
}

pub fn new_string(value string, pos Position) Node {
    return Node{
        id: next_id(),
        kind: .string,
        value: value,
        position: pos,
        type_: Type{name: 'string'}
    }
}

pub fn new_boolean(value bool, pos Position) Node {
    return Node{
        id: next_id(),
        kind: .boolean,
        value: value.str(),
        position: pos,
        type_: Type{name: 'boolean'}
    }
}

pub fn new_atom(value string, pos Position) Node {
    return Node{
        id: next_id(),
        kind: .atom,
        value: value,
        position: pos,
        type_: Type{name: 'atom'}
    }
}

pub fn new_nil(pos Position) Node {
    return Node{
        id: next_id(),
        kind: .nil,
        value: 'nil',
        position: pos,
        type_: Type{name: 'nil'}
    }
}

pub fn new_function(name string, body Node, pos Position) Node {
    return Node{
        id: next_id(),
        kind: .function,
        value: name,
        children: [body],
        position: pos,
        type_: Type{name: 'function', params: [body.type_]}
    }
}

pub fn new_module(functions []Node, pos Position) Node {
    return Node{
        id: next_id(),
        kind: .module,
        children: functions,
        position: pos,
        type_: Type{name: 'module'}
    }
}

// Helper para criar Position
pub fn new_position(line int, column int, file string) Position {
    return Position{
        line: line,
        column: column,
        file: file
    }
}