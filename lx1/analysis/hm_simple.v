module analysis

import ast
import errors

// Simplified HM type system that works with existing code
pub struct HMTypeSystem {
mut:
	error_reporter errors.ErrorReporter
}

pub fn new_hm_type_system() HMTypeSystem {
	return HMTypeSystem{
		error_reporter: errors.new_error_reporter()
	}
}

// Simple type inference for polymorphic functions
pub fn infer_polymorphic_type(func_name string, arg_types []ast.Type) ast.Type {
	return match func_name {
		'identity' {
			if arg_types.len == 1 {
				// identity: (A) -> A
				type_function(arg_types, arg_types[0])
			} else {
				type_function(arg_types, type_integer())
			}
		}
		'map' {
			if arg_types.len == 2 {
				// map: ((A -> B), [A]) -> [B]
				func_type := arg_types[0]
				list_type := arg_types[1]
				if list_type.name == 'list' && list_type.params.len == 1 {
					// Create a fresh type variable for the result
					result_type := ast.Type{
						name: 'list'
						params: [ast.Type{name: 'T1', params: []}]
					}
					type_function(arg_types, result_type)
				} else {
					type_function(arg_types, type_integer())
				}
			} else {
				type_function(arg_types, type_integer())
			}
		}
		'head' {
			if arg_types.len == 1 {
				// head: ([A]) -> A
				list_type := arg_types[0]
				if list_type.name == 'list' && list_type.params.len == 1 {
					list_type.params[0]
				} else {
					type_integer()
				}
			} else {
				type_integer()
			}
		}
		'length' {
			if arg_types.len == 1 {
				// length: ([A]) -> integer
				type_integer()
			} else {
				type_integer()
			}
		}
		'compose' {
			if arg_types.len == 2 {
				// compose: ((B -> C), (A -> B)) -> (A -> C)
				func1 := arg_types[0]
				func2 := arg_types[1]
				if func1.name == 'function' && func2.name == 'function' {
					// Create a new function type
					param_type := ast.Type{name: 'T1', params: []}
					return_type := ast.Type{name: 'T2', params: []}
					type_function([param_type], return_type)
				} else {
					type_function(arg_types, type_integer())
				}
			} else {
				type_function(arg_types, type_integer())
			}
		}
		else {
			// Default: return a generic function type
			type_function(arg_types, type_integer())
		}
	}
}

// Check if a type is polymorphic (contains type variables)
pub fn is_polymorphic_type(typ ast.Type) bool {
	if typ.name.starts_with('T') && typ.params.len == 0 {
		return true
	}

	for param in typ.params {
		if is_polymorphic_type(param) {
			return true
		}
	}

	return false
}

// Get type variables from a type
pub fn get_type_variables(typ ast.Type) []string {
	mut vars := []string{}
	collect_type_variables(typ, mut vars)
	return vars
}

fn collect_type_variables(typ ast.Type, mut vars []string) {
	if typ.name.starts_with('T') && typ.params.len == 0 {
		if typ.name !in vars {
			vars << typ.name
		}
		return
	}

	for param in typ.params {
		collect_type_variables(param, mut vars)
	}
}

// Simple type unification
pub fn unify_types(t1 ast.Type, t2 ast.Type) bool {
	if t1.name == 'any' || t2.name == 'any' {
		return true
	}

	if t1.name.starts_with('T') && t1.params.len == 0 {
		return true // Type variable can unify with anything
	}

	if t2.name.starts_with('T') && t2.params.len == 0 {
		return true // Type variable can unify with anything
	}

	if t1.name != t2.name {
		return false
	}

	if t1.params.len != t2.params.len {
		return false
	}

	for i in 0..t1.params.len {
		if !unify_types(t1.params[i], t2.params[i]) {
			return false
		}
	}

	return true
}



// Generate Erlang spec for a polymorphic type
pub fn generate_polymorphic_spec(func_name string, typ ast.Type) string {
	if !is_polymorphic_type(typ) {
		return generate_simple_spec(typ)
	}

	// Convert polymorphic type to Erlang spec
	mut spec := '${func_name}('

	if typ.name == 'function' && typ.params.len > 1 {
		// Function type: (params...) -> return_type
		param_types := typ.params[..typ.params.len - 1]
		return_type := typ.params[typ.params.len - 1]

		for i, param in param_types {
			if i > 0 {
				spec += ', '
			}
			spec += convert_type_to_erlang(param)
		}

		spec += ') -> ${convert_type_to_erlang(return_type)}'
	} else {
		spec += convert_type_to_erlang(typ)
	}

	return spec
}

pub fn convert_type_to_erlang(typ ast.Type) string {
	return match typ.name {
		'integer' { 'integer()' }
		'float' { 'float()' }
		'string' { 'binary()' }
		'boolean' { 'boolean()' }
		'atom' { 'atom()' }
		'nil' { 'nil' }
		'list' {
			if typ.params.len == 1 {
				'[${convert_type_to_erlang(typ.params[0])}]'
			} else {
				'[any()]'
			}
		}
		'map' {
			if typ.params.len == 2 {
				'#{${convert_type_to_erlang(typ.params[0])} => ${convert_type_to_erlang(typ.params[1])}}'
			} else {
				'#{any() => any()}'
			}
		}
		'tuple' {
			if typ.params.len > 0 {
				mut tuple_spec := '{'
				for i, param in typ.params {
					if i > 0 {
						tuple_spec += ', '
					}
					tuple_spec += convert_type_to_erlang(param)
				}
				tuple_spec += '}'
				tuple_spec
			} else {
				'{}'
			}
		}
		'function' {
			if typ.params.len > 1 {
				param_types := typ.params[..typ.params.len - 1]
				return_type := typ.params[typ.params.len - 1]

				mut func_spec := '('
				for i, param in param_types {
					if i > 0 {
						func_spec += ', '
					}
					func_spec += convert_type_to_erlang(param)
				}
				func_spec += ') -> ${convert_type_to_erlang(return_type)}'
				func_spec
			} else {
				'(any()) -> any()'
			}
		}
		else {
			if typ.name.starts_with('T') {
				'any()' // Type variables become any() in Erlang
			} else {
				'any()'
			}
		}
	}
}

fn generate_simple_spec(typ ast.Type) string {
	return convert_type_to_erlang(typ)
}

pub fn (mut hms HMTypeSystem) error(msg string, pos ast.Position) {
	hms.error_reporter.report(.analysis, msg, pos)
}

pub fn (hms HMTypeSystem) get_errors() []errors.Err {
	return hms.error_reporter.all()
}