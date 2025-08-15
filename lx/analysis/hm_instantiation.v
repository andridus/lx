module analysis

import ast

pub fn instantiate(type_scheme TypeScheme, var_counter int) (ast.Type, int) {
	// Create fresh type variables for each quantified variable
	mut substitution := Substitution{
		mappings: map[string]ast.Type{}
	}
	mut counter := var_counter

	for var in type_scheme.quantified_vars {
		fresh_var := new_type_var(counter)
		counter++
		substitution.mappings[var.name] = ast.Type{
			name:   fresh_var.name
			params: []
		}
	}

	// Apply substitution to the body
	return substitute_in_type(type_scheme.body, substitution), counter
}

pub fn instantiate_with_substitution(type_scheme TypeScheme, substitution Substitution) ast.Type {
	// Apply the given substitution to the type scheme
	return substitute_in_type_scheme(type_scheme, substitution).body
}

pub fn instantiate_partial(type_scheme TypeScheme, partial_substitution Substitution, var_counter int) (ast.Type, int) {
	// Create fresh variables for unsubstituted quantified variables
	mut full_substitution := Substitution{
		mappings: partial_substitution.mappings.clone()
	}
	mut counter := var_counter

	for var in type_scheme.quantified_vars {
		if var.name !in full_substitution.mappings {
			fresh_var := new_type_var(counter)
			counter++
			full_substitution.mappings[var.name] = ast.Type{
				name:   fresh_var.name
				params: []
			}
		}
	}

	return substitute_in_type(type_scheme.body, full_substitution), counter
}

pub fn instantiate_function_type(param_types []ast.Type, return_type ast.Type, var_counter int) (ast.Type, int) {
	// Create fresh type variables for function parameters and return type
	mut fresh_param_types := []ast.Type{}
	mut counter := var_counter

	for param in param_types {
		if is_generic_type(param) {
			fresh_var := new_type_var(counter)
			counter++
			fresh_param_types << ast.Type{
				name:   fresh_var.name
				params: []
			}
		} else {
			fresh_param_types << param
		}
	}

	mut fresh_return_type := return_type
	if is_generic_type(return_type) {
		fresh_var := new_type_var(counter)
		counter++
		fresh_return_type = ast.Type{
			name:   fresh_var.name
			params: []
		}
	}

	return type_function(fresh_param_types, fresh_return_type), counter
}

pub fn instantiate_list_type(element_type ast.Type, var_counter int) (ast.Type, int) {
	mut counter := var_counter
	if is_generic_type(element_type) {
		fresh_var := new_type_var(counter)
		counter++
		return ast.Type{
			name:   'list'
			params: [ast.Type{
				name:   fresh_var.name
				params: []
			}]
		}, counter
	}

	return ast.Type{
		name:   'list'
		params: [element_type]
	}, counter
}

pub fn instantiate_map_type(key_type ast.Type, value_type ast.Type, var_counter int) (ast.Type, int) {
	mut fresh_key_type := key_type
	mut fresh_value_type := value_type
	mut counter := var_counter

	if is_generic_type(key_type) {
		fresh_var := new_type_var(counter)
		counter++
		fresh_key_type = ast.Type{
			name:   fresh_var.name
			params: []
		}
	}

	if is_generic_type(value_type) {
		fresh_var := new_type_var(counter)
		counter++
		fresh_value_type = ast.Type{
			name:   fresh_var.name
			params: []
		}
	}

	return ast.Type{
		name:   'map'
		params: [fresh_key_type, fresh_value_type]
	}, counter
}

pub fn instantiate_tuple_type(element_types []ast.Type, var_counter int) (ast.Type, int) {
	mut fresh_element_types := []ast.Type{}
	mut counter := var_counter

	for elem_type in element_types {
		if is_generic_type(elem_type) {
			fresh_var := new_type_var(counter)
			counter++
			fresh_element_types << ast.Type{
				name:   fresh_var.name
				params: []
			}
		} else {
			fresh_element_types << elem_type
		}
	}

	return ast.Type{
		name:   'tuple'
		params: fresh_element_types
	}, counter
}

pub fn create_fresh_type_variables(count int, var_counter int) ([]ast.Type, int) {
	mut fresh_vars := []ast.Type{}
	mut counter := var_counter

	for _ in 0 .. count {
		fresh_var := new_type_var(counter)
		counter++
		fresh_vars << ast.Type{
			name:   fresh_var.name
			params: []
		}
	}

	return fresh_vars, counter
}

pub fn instantiate_with_constraints(type_scheme TypeScheme, constraints []Constraint, var_counter int) !(ast.Type, int) {
	// First, instantiate the type scheme
	instantiated_type, new_counter := instantiate(type_scheme, var_counter)

	// Then, apply constraints to refine the type
	// This is a simplified version - in a full implementation,
	// we would solve the constraints and apply the resulting substitution
	return instantiated_type, new_counter
}
