module analysis

import ast

@[heap]
pub struct TypeTable {
pub mut:
	types   map[int]ast.Type
	next_id int = 1
}

pub fn new_type_table() TypeTable {
	return TypeTable{
		types:   map[int]ast.Type{}
		next_id: 1
	}
}

pub fn (mut tt TypeTable) assign_type(id int, typ ast.Type) {
	tt.types[id] = typ
}

pub fn (tt TypeTable) get_type(id int) ?ast.Type {
	return tt.types[id] or { none }
}

pub fn (mut tt TypeTable) generate_id() int {
	id := tt.next_id
	tt.next_id++
	return id
}

pub fn (tt TypeTable) debug_print() {
	println('=== TYPE TABLE ===')
	for id, typ in tt.types {
		println('  Node ${id}: ${typ.str()}')
	}
	println('==================')
}
