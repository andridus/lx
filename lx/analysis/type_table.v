module analysis

import ast

pub struct RecordType {
pub:
	name           string
	fields         map[string]ast.Type // field_name -> field_type
	field_defaults map[string]ast.Node // field_name -> field_default
}

pub struct FunctionType {
pub mut:
	return_type ast.Type
pub:
	name       string
	parameters []ast.Type
	heads      []FunctionHead
	public     bool = true
}

pub struct FunctionHead {
pub:
	node_id  int
	patterns []ast.Type
pub mut:
	return_type ast.Type
}

@[heap]
pub struct TypeTable {
pub mut:
	types          map[int]ast.Type
	next_id        int = 1
	record_types   map[string]RecordType   // record_name -> RecordType
	function_types map[string]FunctionType // function_name -> FunctionType
	custom_types   map[string]ast.Type     // type_name -> Type definition
	used_types     map[string]bool         // track which types have been used
	type_positions map[string]ast.Position // track where each type was defined
}

pub fn new_type_table() TypeTable {
	return TypeTable{
		types:          map[int]ast.Type{}
		next_id:        1
		record_types:   map[string]RecordType{}
		function_types: map[string]FunctionType{}
		custom_types:   map[string]ast.Type{}
		used_types:     map[string]bool{}
		type_positions: map[string]ast.Position{}
	}
}

pub fn (mut tt TypeTable) update_function_head(function_name string, node_id int, head FunctionHead) {
	if mut function := tt.function_types[function_name] {
		mut return_types := []ast.Type{}
		mut heads := []FunctionHead{}
		for mut head1 in function.heads {
			if head1.node_id == node_id {
				return_types << head.return_type
				heads << head
			} else {
				return_types << head1.return_type
				heads << head1
			}
		}
		mut return_type_ := function.return_type
		if return_types.len > 1 {
			return_type_ = ast.Type{
				name:   'union'
				params: return_types
			}
		}
		tt.function_types[function_name] = FunctionType{
			...function
			heads:       heads
			return_type: return_type_
		}
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

// Function type management
pub fn (mut tt TypeTable) register_function_type(name string, heads []FunctionHead, return_type ast.Type, public bool) {
	tt.function_types[name] = FunctionType{
		name:        name
		return_type: return_type
		heads:       heads
		public:      public
	}
}

pub fn (tt TypeTable) get_function_type(name string) ?FunctionType {
	return tt.function_types[name] or { return none }
}

// Custom type management
pub fn (mut tt TypeTable) register_custom_type(name string, type_def ast.Type) {
	tt.custom_types[name] = type_def
}

pub fn (tt TypeTable) get_custom_type(name string) ?ast.Type {
	return tt.custom_types[name] or { return none }
}

// Mark a type as used
pub fn (mut tt TypeTable) mark_type_used(name string) {
	tt.used_types[name] = true
}

// Register type position
pub fn (mut tt TypeTable) register_type_position(name string, pos ast.Position) {
	tt.type_positions[name] = pos
}

// Get type position
pub fn (tt TypeTable) get_type_position(name string) ?ast.Position {
	return tt.type_positions[name] or { none }
}

// Get all unused types
pub fn (tt TypeTable) get_unused_types() []string {
	mut unused := []string{}

	// Check custom types
	for type_name, _ in tt.custom_types {
		if !tt.used_types[type_name] {
			unused << type_name
		}
	}

	// Check record types
	for record_name, _ in tt.record_types {
		if !tt.used_types[record_name] {
			unused << record_name
		}
	}

	return unused
}
