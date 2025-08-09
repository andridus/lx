module analysis

import ast

pub fn generalize(type_info ast.Type, env TypeEnv) TypeScheme {
	// Find free type variables in type_info that are not bound in env
	free_vars := find_free_variables(type_info, env)

	return TypeScheme{
		quantified_vars: free_vars
		body: type_info
	}
}

fn find_free_variables(typ ast.Type, env TypeEnv) []TypeVar {
	mut free_vars := []TypeVar{}

	// Get all type variables bound in the environment
	bound_vars := env.get_bound_variables()

	// Find free variables in the type
	collect_free_variables(typ, bound_vars, mut free_vars)

	return free_vars
}

fn collect_free_variables(typ ast.Type, bound_vars []string, mut free_vars []TypeVar) {
	// If it's a type variable
	if typ.name.starts_with('T') && typ.params.len == 0 {
		// Check if it's bound in the environment
		if typ.name !in bound_vars {
			// Check if we already have this variable
			for existing in free_vars {
				if existing.name == typ.name {
					return
				}
			}
			// Add to free variables
			free_vars << TypeVar{
				id: 0 // Will be set properly later
				name: typ.name
			}
		}
		return
	}

	// Recursively check parameters
	for param in typ.params {
		collect_free_variables(param, bound_vars, mut free_vars)
	}
}

pub fn is_generic_type(typ ast.Type) bool {
	// Check if the type contains any type variables
	return contains_type_variables(typ)
}
