module analysis

import ast
import errors

pub struct HMInferencer {
mut:
	type_env     TypeEnv
	type_table   TypeTable
	constraints  []Constraint
	var_counter  int
	error_reporter errors.ErrorReporter
}

pub struct TypeScheme {
pub:
	quantified_vars []TypeVar
	body           ast.Type
}

pub struct Substitution {
mut:
	mappings map[string]ast.Type
}

pub fn new_hm_inferencer() HMInferencer {
	return HMInferencer{
		type_env: new_type_env('root')
		type_table: new_type_table()
		constraints: []
		var_counter: 0
		error_reporter: errors.new_error_reporter()
	}
}

pub fn (mut hmi HMInferencer) infer_expression(expr ast.Node) !ast.Type {
	return match expr.kind {
		.integer { type_integer() }
		.float { type_float() }
		.string { type_string() }
		.boolean { type_boolean() }
		.atom { type_atom() }
		.nil { type_nil() }
		.variable_ref { hmi.infer_variable_ref(expr) }
		.variable_binding { hmi.infer_binding(expr) }
		.function_caller { hmi.infer_function_call(expr) }
		.parentheses { hmi.infer_parentheses(expr) }
		.list_literal { hmi.infer_list_literal(expr) }
		.list_cons { hmi.infer_list_cons(expr) }
		.tuple_literal { hmi.infer_tuple_literal(expr) }
		.map_literal { hmi.infer_map_literal(expr) }
		.map_access { hmi.infer_map_access(expr) }
		.block { hmi.infer_block(expr) }
		else {
			hmi.error('Unsupported expression type for inference: ${expr.kind}', expr.position)
			return error('Unsupported expression type')
		}
	}
}

fn (mut hmi HMInferencer) infer_variable_ref(node ast.Node) !ast.Type {
	name := node.value
	if typ := hmi.type_env.lookup(name) {
		return hmi.instantiate(typ)
	}
	hmi.error('Undefined variable: ${name}', node.position)
	return error('Undefined variable')
}

fn (mut hmi HMInferencer) infer_binding(node ast.Node) !ast.Type {
	if node.children.len != 2 {
		hmi.error('Invalid binding: expected 2 children', node.position)
		return error('Invalid binding')
	}

	value_expr := node.children[0]
	value_type := hmi.infer_expression(value_expr)!

	// Store the type in the environment
	hmi.type_env.bind(node.value, TypeScheme{
		quantified_vars: []
		body: value_type
	})

	return value_type
}

fn (mut hmi HMInferencer) infer_function_call(node ast.Node) !ast.Type {
	if node.children.len < 1 {
		hmi.error('Invalid function call: no function name', node.position)
		return error('Invalid function call')
	}

	func_name := node.children[0].value
	args := node.children[1..]

	// Infer argument types
	mut arg_types := []ast.Type{}
	for arg in args {
		arg_type := hmi.infer_expression(arg)!
		arg_types << arg_type
	}

	// Check if it's a built-in function
	if builtin_type := hmi.get_builtin_function_type(func_name, arg_types) {
		return builtin_type
	}

	// For now, return a generic function type
	// This will be extended with proper function type inference
	return type_function(arg_types, type_integer())
}

fn (mut hmi HMInferencer) infer_parentheses(node ast.Node) !ast.Type {
	if node.children.len != 1 {
		hmi.error('Invalid parentheses: expected 1 child', node.position)
		return error('Invalid parentheses')
	}
	return hmi.infer_expression(node.children[0])
}

fn (mut hmi HMInferencer) infer_list_literal(node ast.Node) !ast.Type {
	if node.children.len == 0 {
		// Empty list - return generic list type
		return ast.Type{
			name: 'list'
			params: [ast.Type{name: 'any', params: []}]
		}
	}

	// Infer type of first element
	first_type := hmi.infer_expression(node.children[0])!

	// Check if all elements have the same type
	for i in 1..node.children.len {
		elem_type := hmi.infer_expression(node.children[i])!
		hmi.add_constraint(first_type, elem_type, node.position)
	}

	return ast.Type{
		name: 'list'
		params: [first_type]
	}
}

fn (mut hmi HMInferencer) infer_list_cons(node ast.Node) !ast.Type {
	if node.children.len != 2 {
		hmi.error('Invalid list cons: expected 2 children', node.position)
		return error('Invalid list cons')
	}

	head_type := hmi.infer_expression(node.children[0])!
	tail_type := hmi.infer_expression(node.children[1])!

	// Tail should be a list
	expected_tail_type := ast.Type{
		name: 'list'
		params: [head_type]
	}
	hmi.add_constraint(tail_type, expected_tail_type, node.position)

	return expected_tail_type
}

fn (mut hmi HMInferencer) infer_tuple_literal(node ast.Node) !ast.Type {
	mut elem_types := []ast.Type{}
	for child in node.children {
		elem_type := hmi.infer_expression(child)!
		elem_types << elem_type
	}

	return ast.Type{
		name: 'tuple'
		params: elem_types
	}
}

fn (mut hmi HMInferencer) infer_map_literal(node ast.Node) !ast.Type {
	if node.children.len == 0 {
		// Empty map
		return ast.Type{
			name: 'map'
			params: [
				ast.Type{name: 'any', params: []},
				ast.Type{name: 'any', params: []}
			]
		}
	}

	// For now, assume all keys and values have the same type
	// This will be improved with proper map type inference
	return ast.Type{
		name: 'map'
		params: [
			ast.Type{name: 'any', params: []},
			ast.Type{name: 'any', params: []}
		]
	}
}

fn (mut hmi HMInferencer) infer_map_access(node ast.Node) !ast.Type {
	if node.children.len != 2 {
		hmi.error('Invalid map access: expected 2 children', node.position)
		return error('Invalid map access')
	}

	map_type := hmi.infer_expression(node.children[0])!
	key_type := hmi.infer_expression(node.children[1])!

	// Map should have type map(K, V)
	if map_type.name != 'map' || map_type.params.len != 2 {
		hmi.error('Expected map type for map access', node.position)
		return error('Expected map type')
	}

	// Key should match the map's key type
	hmi.add_constraint(key_type, map_type.params[0], node.position)

	// Return the value type
	return map_type.params[1]
}

fn (mut hmi HMInferencer) infer_block(node ast.Node) !ast.Type {
	if node.children.len == 0 {
		return type_nil()
	}

	// Return type of last expression
	return hmi.infer_expression(node.children[node.children.len - 1])
}

fn (mut hmi HMInferencer) get_builtin_function_type(func_name string, arg_types []ast.Type) ?ast.Type {
	return match func_name {
		'+', '-', '*', '/' {
			if arg_types.len == 2 {
				hmi.add_constraint(arg_types[0], type_integer(), ast.Position{})
				hmi.add_constraint(arg_types[1], type_integer(), ast.Position{})
				type_integer()
			} else {
				none
			}
		}
		'==', '!=', '<', '<=', '>', '>=' {
			if arg_types.len == 2 {
				hmi.add_constraint(arg_types[0], arg_types[1], ast.Position{})
				type_boolean()
			} else {
				none
			}
		}
		'and', 'or' {
			if arg_types.len == 2 {
				hmi.add_constraint(arg_types[0], type_boolean(), ast.Position{})
				hmi.add_constraint(arg_types[1], type_boolean(), ast.Position{})
				type_boolean()
			} else {
				none
			}
		}
		'map_size' {
			if arg_types.len == 1 {
				hmi.add_constraint(arg_types[0], ast.Type{name: 'map', params: [ast.Type{name: 'any', params: []}, ast.Type{name: 'any', params: []}]}, ast.Position{})
				type_integer()
			} else {
				none
			}
		}
		'map_get' {
			if arg_types.len == 2 {
				ast.Type{name: 'any', params: []}
			} else {
				none
			}
		}
		'map_put' {
			if arg_types.len == 3 {
				ast.Type{name: 'map', params: [ast.Type{name: 'any', params: []}, ast.Type{name: 'any', params: []}]}
			} else {
				none
			}
		}
		'map_remove' {
			if arg_types.len == 2 {
				ast.Type{name: 'map', params: [ast.Type{name: 'any', params: []}, ast.Type{name: 'any', params: []}]}
			} else {
				none
			}
		}
		else { none }
	}
}

fn (mut hmi HMInferencer) add_constraint(left ast.Type, right ast.Type, pos ast.Position) {
	hmi.constraints << Constraint{
		left: left
		right: right
		position: pos
	}
}

fn (mut hmi HMInferencer) instantiate(type_scheme TypeScheme) ast.Type {
	// For now, just return the body type
	// This will be extended with proper instantiation
	return type_scheme.body
}

fn (mut hmi HMInferencer) error(msg string, pos ast.Position) {
	hmi.error_reporter.report(.analysis, msg, pos)
}

pub fn (hmi HMInferencer) get_errors() []errors.Err {
	return hmi.error_reporter.all()
}