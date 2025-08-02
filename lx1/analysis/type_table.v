module analysis

import ast

pub struct RecordType {
pub:
	name           string
	fields         map[string]ast.Type // field_name -> field_type
	field_defaults map[string]ast.Node // field_name -> field_default
}

@[heap]
pub struct TypeTable {
pub mut:
	types        map[int]ast.Type
	next_id      int = 1
	record_types map[string]RecordType // record_name -> RecordType
}

pub fn new_type_table() TypeTable {
	return TypeTable{
		types:        map[int]ast.Type{}
		next_id:      1
		record_types: map[string]RecordType{}
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

pub fn (mut tt TypeTable) register_record_type(name string, fields map[string]ast.Type, field_defaults map[string]ast.Node) {
	tt.record_types[name] = RecordType{
		name:           name
		fields:         fields
		field_defaults: field_defaults
	}
}

pub fn (tt TypeTable) get_record_type(name string) ?RecordType {
	return tt.record_types[name] or { return none }
}

pub fn (tt TypeTable) get_field_type(record_name string, field_name string) ?ast.Type {
	record_type := tt.get_record_type(record_name) or { return none }
	return record_type.fields[field_name] or { return none }
}

pub fn (tt TypeTable) get_field_default(record_name string, field_name string) ?ast.Node {
	record_type := tt.get_record_type(record_name) or { return none }
	return record_type.field_defaults[field_name] or { return none }
}
