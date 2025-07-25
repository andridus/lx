module ast

pub fn new_integer_with_id(id int, value int, pos Position) Node {
	return Node{
		id:       id
		kind:     .integer
		value:    value.str()
		position: pos
	}
}

pub fn new_float_with_id(id int, value f64, pos Position) Node {
	return Node{
		id:       id
		kind:     .float
		value:    value.str()
		position: pos
	}
}

pub fn new_string_with_id(id int, value string, pos Position) Node {
	return Node{
		id:       id
		kind:     .string
		value:    value
		position: pos
	}
}

pub fn new_boolean_with_id(id int, value bool, pos Position) Node {
	return Node{
		id:       id
		kind:     .boolean
		value:    value.str()
		position: pos
	}
}

pub fn new_atom_with_id(id int, value string, pos Position) Node {
	return Node{
		id:       id
		kind:     .atom
		value:    value
		position: pos
	}
}

pub fn new_nil_with_id(id int, pos Position) Node {
	return Node{
		id:       id
		kind:     .nil
		value:    'nil'
		position: pos
	}
}

pub fn new_function_with_id(id int, name string, body Node, pos Position) Node {
	return Node{
		id:       id
		kind:     .function
		value:    name
		children: [body]
		position: pos
	}
}

pub fn new_module_with_id(id int, name string, functions []Node, pos Position) Node {
	return Node{
		id:       id
		kind:     .module
		value:    name
		children: functions
		position: pos
	}
}

pub fn new_position(line int, column int, file string) Position {
	return Position{
		line:   line
		column: column
		file:   file
	}
}
