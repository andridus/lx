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

pub fn new_tuple_literal(id int, elements []Node, pos Position) Node {
	return Node{
		id:       id
		kind:     .tuple_literal
		value:    '{}'
		children: elements
		position: pos
	}
}

pub fn new_map_literal(id int, entries []Node, pos Position) Node {
	return Node{
		id:       id
		kind:     .map_literal
		value:    '%{}'
		children: entries
		position: pos
	}
}

pub fn new_map_access(id int, map_expr Node, key_expr Node, pos Position) Node {
	return Node{
		id:       id
		kind:     .map_access
		value:    'map[key]'
		children: [map_expr, key_expr]
		position: pos
	}
}

// Record builders
pub fn new_record_field(id int, field_name string, field_type Node, default_value Node, pos Position) Node {
	return Node{
		id:       id
		kind:     .record_field
		value:    field_name
		children: [field_type, default_value]
		position: pos
	}
}

pub fn new_record_field_without_default(id int, field_name string, field_type Node, pos Position) Node {
	return Node{
		id:       id
		kind:     .record_field
		value:    field_name
		children: [field_type]
		position: pos
	}
}

pub fn new_record_definition(id int, name string, fields []Node, pos Position) Node {
	return Node{
		id:       id
		kind:     .record_definition
		value:    name
		children: fields
		position: pos
	}
}

pub fn new_record_literal(id int, name string, field_values []Node, pos Position) Node {
	return Node{
		id:       id
		kind:     .record_literal
		value:    name
		children: field_values
		position: pos
	}
}

pub fn new_record_access(id int, record_expr Node, field_name string, pos Position) Node {
	return Node{
		id:       id
		kind:     .record_access
		value:    field_name
		children: [record_expr]
		position: pos
	}
}

pub fn new_record_update(id int, record_name string, record_expr Node, field_name string, field_value Node, pos Position) Node {
	return Node{
		id:       id
		kind:     .record_update
		value:    record_name
		children: [record_expr, Node{
			id:       -1
			kind:     .identifier
			value:    field_name
			children: []
			position: pos
		}, field_value]
		position: pos
	}
}

// New builders for additional functionality

pub fn new_function_with_params(id int, name string, params []Node, body Node, pos Position) Node {
	// Create args block
	args_block := Node{
		id: -1
		kind: .block
		value: 'args'
		children: params
		position: pos
	}

	return Node{
		id:       id
		kind:     .function
		value:    name
		children: [args_block, body]
		position: pos
	}
}

pub fn new_function_parameter(id int, name string, pos Position) Node {
	return Node{
		id:       id
		kind:     .function_parameter
		value:    name
		position: pos
	}
}

pub fn new_lambda_expression(id int, params []Node, body Node, pos Position) Node {
	mut children := []Node{}
	children << params
	children << body
	return Node{
		id:       id
		kind:     .lambda_expression
		children: children
		position: pos
	}
}

pub fn new_case_expression(id int, expr Node, clauses []Node, pos Position) Node {
	mut children := []Node{}
	children << expr
	children << clauses
	return Node{
		id:       id
		kind:     .case_expression
		children: children
		position: pos
	}
}

pub fn new_case_clause(id int, pattern Node, body Node, pos Position) Node {
	return Node{
		id:       id
		kind:     .case_clause
		children: [pattern, body]
		position: pos
	}
}

pub fn new_pattern_match(id int, pattern Node, pos Position) Node {
	return Node{
		id:       id
		kind:     .pattern_match
		children: [pattern]
		position: pos
	}
}

pub fn new_pattern_binding(id int, pattern Node, expr Node, pos Position) Node {
	return Node{
		id:       id
		kind:     .pattern_binding
		children: [pattern, expr]
		position: pos
	}
}

pub fn new_type_alias(id int, name string, type_def Node, pos Position) Node {
	return Node{
		id:       id
		kind:     .type_alias
		value:    name
		children: [type_def]
		position: pos
	}
}

pub fn new_type_annotation(id int, type_node Node, pos Position) Node {
	return Node{
		id:       id
		kind:     .type_annotation
		children: [type_node]
		position: pos
	}
}

pub fn new_identifier(id int, name string, pos Position) Node {
	return Node{
		id:       id
		kind:     .identifier
		value:    name
		position: pos
	}
}
