module ast

pub fn new_integer(id int, value int, pos Position) Node {
	return Node{
		id:       id
		kind:     .integer
		value:    value.str()
		position: pos
	}
}

pub fn new_float(id int, value f64, pos Position) Node {
	return Node{
		id:       id
		kind:     .float
		value:    value.str()
		position: pos
	}
}

pub fn new_string(id int, value string, pos Position) Node {
	return Node{
		id:       id
		kind:     .string
		value:    value
		position: pos
	}
}

pub fn new_boolean(id int, value bool, pos Position) Node {
	return Node{
		id:       id
		kind:     .boolean
		value:    value.str()
		position: pos
	}
}

pub fn new_atom(id int, value string, pos Position) Node {
	return Node{
		id:       id
		kind:     .atom
		value:    value
		position: pos
	}
}

pub fn new_nil(id int, pos Position) Node {
	return Node{
		id:       id
		kind:     .nil
		value:    'nil'
		position: pos
	}
}

pub fn new_function(id int, name string, body Node, pos Position) Node {
	return Node{
		id:       id
		kind:     .function
		value:    name
		children: [body]
		position: pos
	}
}

pub fn new_module(id int, name string, functions []Node, pos Position) Node {
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

pub fn new_variable_binding(id int, name string, value Node, pos Position) Node {
	return Node{
		id:       id
		kind:     .variable_binding
		value:    name
		children: [value]
		position: pos
	}
}

pub fn new_variable_ref(id int, name string, pos Position) Node {
	return Node{
		id:       id
		kind:     .variable_ref
		value:    name
		position: pos
	}
}

pub fn new_block(id int, expressions []Node, pos Position) Node {
	return Node{
		id:       id
		kind:     .block
		children: expressions
		position: pos
	}
}

pub fn new_function_caller(id int, function_name string, arguments []Node, pos Position) Node {
	return Node{
		id:       id
		kind:     .function_caller
		value:    function_name
		children: arguments
		position: pos
	}
}

pub fn new_parentheses(id int, expression Node, pos Position) Node {
	return Node{
		id:       id
		kind:     .parentheses
		children: [expression]
		position: pos
	}
}

pub fn new_directive_call(id int, directive_name string, arguments []Node, pos Position) Node {
	return Node{
		id:       id
		kind:     .directive_call
		value:    directive_name
		children: arguments
		position: pos
	}
}

pub fn new_list_literal(id int, elements []Node, pos Position) Node {
	return Node{
		id:       id
		kind:     .list_literal
		value:    '[]'
		children: elements
		position: pos
	}
}

pub fn new_list_cons(id int, head Node, tail Node, pos Position) Node {
	return Node{
		id:       id
		kind:     .list_cons
		value:    '[]'
		children: [head, tail]
		position: pos
	}
}
