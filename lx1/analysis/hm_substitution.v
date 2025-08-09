module analysis

import ast

pub fn compose_substitutions(sub1 Substitution, sub2 Substitution) Substitution {
	mut result := Substitution{
		mappings: sub1.mappings.clone()
	}

	// Apply sub2 to all values in sub1
	for key, value in result.mappings {
		result.mappings[key] = substitute_in_type(value, sub2)
	}

	// Add mappings from sub2 that are not in sub1
	for key, value in sub2.mappings {
		if key !in result.mappings {
			result.mappings[key] = value
		}
	}

	return result
}

pub fn apply_substitution_to_constraints(constraints []Constraint, substitution Substitution) []Constraint {
	mut result := []Constraint{}

	for constraint in constraints {
		result << Constraint{
			left:     substitute_in_type(constraint.left, substitution)
			right:    substitute_in_type(constraint.right, substitution)
			position: constraint.position
		}
	}

	return result
}

pub fn apply_substitution_to_type_env(env TypeEnv, substitution Substitution) TypeEnv {
	mut new_env := TypeEnv{
		scope_name: env.scope_name
		bindings:   map[string]TypeScheme{}
	}

	for name, scheme in env.bindings {
		new_env.bindings[name] = substitute_in_type_scheme(scheme, substitution)
	}

	return new_env
}

pub fn is_identity_substitution(substitution Substitution) bool {
	// Check if the substitution maps each variable to itself
	for key, value in substitution.mappings {
		if value.name != key {
			return false
		}
	}
	return true
}

pub fn restrict_substitution(substitution Substitution, vars []string) Substitution {
	mut result := Substitution{
		mappings: map[string]ast.Type{}
	}

	for var in vars {
		if value := substitution.mappings[var] {
			result.mappings[var] = value
		}
	}

	return result
}

pub fn union_substitutions(sub1 Substitution, sub2 Substitution) !Substitution {
	mut result := Substitution{
		mappings: sub1.mappings.clone()
	}

	for key, value in sub2.mappings {
		if key in result.mappings {
			// Check if the mappings are compatible
			if result.mappings[key].name != value.name {
				return error('Incompatible substitutions for variable: ${key}')
			}
		} else {
			result.mappings[key] = value
		}
	}

	return result
}

pub fn substitute_in_type(typ ast.Type, substitution Substitution) ast.Type {
	// Check if this type is a type variable that needs substitution
	if typ.name.starts_with('T') && typ.params.len == 0 {
		if new_type := substitution.mappings[typ.name] {
			return new_type
		}
		// No substitution found, return original
		return typ
	}

	// Apply substitution to parameters recursively
	mut new_params := []ast.Type{}
	for param in typ.params {
		new_params << substitute_in_type(param, substitution)
	}

	return ast.Type{
		name:   typ.name
		params: new_params
	}
}

pub fn substitute_in_type_scheme(scheme TypeScheme, substitution Substitution) TypeScheme {
	// Create a new substitution that excludes quantified variables
	mut filtered_substitution := Substitution{
		mappings: map[string]ast.Type{}
	}

	for key, value in substitution.mappings {
		// Only include if the variable is not quantified
		mut is_quantified := false
		for var in scheme.quantified_vars {
			if var.name == key {
				is_quantified = true
				break
			}
		}

		if !is_quantified {
			filtered_substitution.mappings[key] = value
		}
	}

	new_body := substitute_in_type(scheme.body, filtered_substitution)

	return TypeScheme{
		quantified_vars: scheme.quantified_vars
		body:            new_body
	}
}

pub fn get_domain(substitution Substitution) []string {
	mut domain := []string{}
	for key in substitution.mappings.keys() {
		domain << key
	}
	return domain
}

pub fn get_range(substitution Substitution) []ast.Type {
	mut range := []ast.Type{}
	for value in substitution.mappings.values() {
		range << value
	}
	return range
}

pub fn is_ground_substitution(substitution Substitution) bool {
	// Check if all values in the substitution are ground types (no type variables)
	for value in substitution.mappings.values() {
		if contains_type_variables(value) {
			return false
		}
	}
	return true
}

fn contains_type_variables(typ ast.Type) bool {
	if typ.name.starts_with('T') && typ.params.len == 0 {
		return true
	}

	for param in typ.params {
		if contains_type_variables(param) {
			return true
		}
	}

	return false
}
