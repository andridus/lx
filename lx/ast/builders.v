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

pub fn new_pattern_match_with_expr(id int, pattern Node, expr Node, pos Position) Node {
	return Node{
		id:       id
		kind:     .pattern_match
		children: [pattern, expr]
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

pub fn new_charlist(id int, value string, pos Position) Node {
	return Node{
		id:       id
		kind:     .string_charlist
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

pub fn new_external_function_call(id int, module_name string, function_name string, arguments []Node, pos Position) Node {
	return Node{
		id:       id
		kind:     .external_function_call
		value:    '${module_name}:${function_name}'
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
		id:       -1
		kind:     .block
		value:    'args'
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

pub fn new_case_clause_with_guard(id int, pattern Node, guard Node, body Node, pos Position) Node {
	return Node{
		id:       id
		kind:     .case_clause
		children: [pattern, body, guard]
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

// ============ Task 11: Control Flow Builders ============

pub fn new_if_expr(id int, condition Node, then_expr Node, else_expr ?Node, pos Position) Node {
	mut children := [condition, then_expr]
	if else_node := else_expr {
		children << else_node
	}
	return Node{
		id:       id
		kind:     .if_expr
		children: children
		position: pos
	}
}

pub fn new_with_expr(id int, pattern Node, expr Node, body Node, else_body ?Node, pos Position) Node {
	mut children := [pattern, expr, body]
	if else_node := else_body {
		children << else_node
	}
	return Node{
		id:       id
		kind:     .with_expr
		children: children
		position: pos
	}
}

pub fn new_with_expr_multi(id int, clauses []Node, body Node, else_body ?Node, pos Position) Node {
	mut children := []Node{}
	// Add all clauses first
	for clause in clauses {
		children << clause
	}
	// Add body
	children << body
	// Add else body if present
	if else_node := else_body {
		children << else_node
	}
	return Node{
		id:       id
		kind:     .with_expr
		children: children
		position: pos
	}
}

pub fn new_match_expr(id int, pattern Node, expr Node, rescue_body ?Node, pos Position) Node {
	mut children := [pattern, expr]
	if rescue_node := rescue_body {
		children << rescue_node
	}
	return Node{
		id:       id
		kind:     .match_expr
		children: children
		position: pos
	}
}

// ============ Task 11: Concurrency Builders ============

pub fn new_spawn_expr(id int, func_expr Node, pos Position) Node {
	return Node{
		id:       id
		kind:     .spawn_expr
		children: [func_expr]
		position: pos
	}
}

pub fn new_send_expr(id int, target Node, message Node, pos Position) Node {
	return Node{
		id:       id
		kind:     .send_expr
		children: [target, message]
		position: pos
	}
}

pub fn new_receive_expr(id int, clauses []Node, pos Position) Node {
	return Node{
		id:       id
		kind:     .receive_expr
		children: clauses
		position: pos
	}
}

pub fn new_supervisor_def(id int, name string, body Node, pos Position) Node {
	return Node{
		id:       id
		kind:     .supervisor_def
		value:    name
		children: [body]
		position: pos
	}
}

pub fn new_worker_def(id int, name string, body Node, pos Position) Node {
	return Node{
		id:       id
		kind:     .worker_def
		value:    name
		children: [body]
		position: pos
	}
}

// ============ Task 11: Binary Builders ============

pub fn new_binary_literal(id int, segments []Node, pos Position) Node {
	return Node{
		id:       id
		kind:     .binary_literal
		children: segments
		position: pos
	}
}

pub fn new_binary_pattern(id int, segments []Node, pos Position) Node {
	return Node{
		id:       id
		kind:     .binary_pattern
		children: segments
		position: pos
	}
}

pub fn new_binary_segment(id int, value Node, size ?Node, options []string, pos Position) Node {
	mut children := [value]
	if size_node := size {
		children << size_node
	}
	return Node{
		id:       id
		kind:     .binary_segment
		value:    options.join(',')
		children: children
		position: pos
	}
}

// ============ Task 11: Custom Types Builders ============

pub fn new_type_def(id int, name string, variants []Node, pos Position) Node {
	return Node{
		id:       id
		kind:     .type_def
		value:    name
		children: variants
		position: pos
	}
}

pub fn new_union_type(id int, variants []Node, pos Position) Node {
	return Node{
		id:       id
		kind:     .union_type
		children: variants
		position: pos
	}
}

pub fn new_generic_type(id int, name string, params []Node, pos Position) Node {
	return Node{
		id:       id
		kind:     .generic_type
		value:    name
		children: params
		position: pos
	}
}

pub fn new_opaque_type(id int, name string, base_type Node, pos Position) Node {
	return Node{
		id:       id
		kind:     .opaque_type
		value:    name
		children: [base_type]
		position: pos
	}
}

pub fn new_nominal_type(id int, name string, base_type Node, pos Position) Node {
	return Node{
		id:       id
		kind:     .nominal_type
		value:    name
		children: [base_type]
		position: pos
	}
}

// ============ Task 11: Module System Builders ============

pub fn new_deps_declaration(id int, deps []Node, pos Position) Node {
	return Node{
		id:       id
		kind:     .deps_declaration
		children: deps
		position: pos
	}
}

pub fn new_application_config(id int, config []Node, pos Position) Node {
	return Node{
		id:       id
		kind:     .application_config
		children: config
		position: pos
	}
}

pub fn new_import_statement(id int, module_name string, pos Position) Node {
	return Node{
		id:       id
		kind:     .import_statement
		value:    module_name
		position: pos
	}
}

// ============ Task 11: Advanced Features Builders ============

pub fn new_string_interpolation(id int, segments []Node, pos Position) Node {
	return Node{
		id:       id
		kind:     .string_interpolation
		children: segments
		position: pos
	}
}

pub fn new_anonymous_function(id int, params []Node, body Node, pos Position) Node {
	mut children := params.clone()
	children << body
	return Node{
		id:       id
		kind:     .anonymous_function
		children: children
		position: pos
	}
}

pub fn new_lambda_call(id int, lambda Node, args []Node, pos Position) Node {
	mut children := [lambda]
	children << args
	return Node{
		id:       id
		kind:     .lambda_call
		children: children
		position: pos
	}
}

pub fn new_directive(id int, name string, args []Node, pos Position) Node {
	return Node{
		id:       id
		kind:     .directive
		value:    name
		children: args
		position: pos
	}
}

pub fn new_test_block(id int, name string, body Node, pos Position) Node {
	return Node{
		id:       id
		kind:     .test_block
		value:    name
		children: [body]
		position: pos
	}
}

pub fn new_list_comprehension(id int, children []Node, pos Position) Node {
	return Node{
		id:       id
		kind:     .list_comprehension
		children: children
		position: pos
	}
}
