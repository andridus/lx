module analysis

import ast
import errors

pub struct Unifier {
mut:
	error_reporter errors.ErrorReporter
}

pub fn new_unifier() Unifier {
	return Unifier{
		error_reporter: errors.new_error_reporter()
	}
}

pub fn (mut u Unifier) unify(t1 ast.Type, t2 ast.Type, pos ast.Position) !Substitution {
	return u.unify_types(t1, t2, pos)
}

fn (mut u Unifier) unify_types(t1 ast.Type, t2 ast.Type, pos ast.Position) !Substitution {
	// Handle type variables
	if t1.name.starts_with('T') && t1.params.len == 0 {
		return u.unify_variable(t1.name, t2, pos)
	}

	if t2.name.starts_with('T') && t2.params.len == 0 {
		return u.unify_variable(t2.name, t1, pos)
	}

	// Handle ground types
	if t1.name == t2.name {
		if t1.params.len == 0 {
			// Both are ground types with no parameters
			return Substitution{
				mappings: map[string]ast.Type{}
			}
		}

		if t1.params.len == t2.params.len {
			// Unify parameters
			mut result := Substitution{
				mappings: map[string]ast.Type{}
			}
			for i in 0 .. t1.params.len {
				param_substitution := u.unify_types(t1.params[i], t2.params[i], pos)!
				result = compose_substitutions(result, param_substitution)
			}
			return result
		}
	}

	// Handle special cases for generic types
	if t1.name == 'list' && t2.name == 'list' {
		return u.unify_list_types(t1, t2, pos)
	}

	if t1.name == 'map' && t2.name == 'map' {
		return u.unify_map_types(t1, t2, pos)
	}

	if t1.name == 'tuple' && t2.name == 'tuple' {
		return u.unify_tuple_types(t1, t2, pos)
	}

	if t1.name == 'function' && t2.name == 'function' {
		return u.unify_function_types(t1, t2, pos)
	}

	// Types are incompatible
	u.error('Type mismatch: ${t1.str()} vs ${t2.str()}', pos)
	return error('Type mismatch')
}

fn (mut u Unifier) unify_variable(var_name string, typ ast.Type, pos ast.Position) !Substitution {
	// Check occurs check
	if u.occurs_in(var_name, typ) {
		u.error('Occurs check failed: ${var_name} occurs in ${typ.str()}', pos)
		return error('Occurs check failed')
	}

	// Create new substitution
	return Substitution{
		mappings: {
			var_name: typ
		}
	}
}

fn (mut u Unifier) occurs_in(var_name string, typ ast.Type) bool {
	if typ.name == var_name {
		return true
	}

	for param in typ.params {
		if u.occurs_in(var_name, param) {
			return true
		}
	}

	return false
}

fn (mut u Unifier) unify_list_types(t1 ast.Type, t2 ast.Type, pos ast.Position) !Substitution {
	if t1.params.len != 1 || t2.params.len != 1 {
		u.error('Invalid list type: expected 1 parameter', pos)
		return error('Invalid list type')
	}

	// Unify element types
	return u.unify_types(t1.params[0], t2.params[0], pos)
}

fn (mut u Unifier) unify_map_types(t1 ast.Type, t2 ast.Type, pos ast.Position) !Substitution {
	if t1.params.len != 2 || t2.params.len != 2 {
		u.error('Invalid map type: expected 2 parameters', pos)
		return error('Invalid map type')
	}

	// Unify key types
	key_substitution := u.unify_types(t1.params[0], t2.params[0], pos)!

	// Unify value types
	value_substitution := u.unify_types(t1.params[1], t2.params[1], pos)!

	// Compose substitutions
	return compose_substitutions(key_substitution, value_substitution)
}

fn (mut u Unifier) unify_tuple_types(t1 ast.Type, t2 ast.Type, pos ast.Position) !Substitution {
	if t1.params.len != t2.params.len {
		u.error('Tuple type mismatch: different number of elements', pos)
		return error('Tuple type mismatch')
	}

	mut result := Substitution{
		mappings: map[string]ast.Type{}
	}

	// Unify each element
	for i in 0 .. t1.params.len {
		element_substitution := u.unify_types(t1.params[i], t2.params[i], pos)!
		result = compose_substitutions(result, element_substitution)
	}

	return result
}

fn (mut u Unifier) unify_function_types(t1 ast.Type, t2 ast.Type, pos ast.Position) !Substitution {
	if t1.params.len < 1 || t2.params.len < 1 {
		u.error('Invalid function type: expected at least 1 parameter (return type)',
			pos)
		return error('Invalid function type')
	}

	// Function types have parameters + return type
	// For example: function(A, B, C) means (A, B) -> C

	if t1.params.len != t2.params.len {
		u.error('Function type mismatch: different number of parameters', pos)
		return error('Function type mismatch')
	}

	mut result := Substitution{
		mappings: map[string]ast.Type{}
	}

	// Unify all parameters (including return type)
	for i in 0 .. t1.params.len {
		param_substitution := u.unify_types(t1.params[i], t2.params[i], pos)!
		result = compose_substitutions(result, param_substitution)
	}

	return result
}

fn (mut u Unifier) error(msg string, pos ast.Position) {
	u.error_reporter.report(.analysis, msg, pos)
}

pub fn (u Unifier) get_errors() []errors.Err {
	return u.error_reporter.all()
}

// Convenience functions for common unification patterns
pub fn unify_with_any(typ ast.Type) bool {
	return typ.name == 'any'
}

pub fn unify_with_unknown(typ ast.Type) bool {
	return typ.name == 'unknown'
}

pub fn unify_ground_types(t1 ast.Type, t2 ast.Type) bool {
	// Check if both types are ground (no type variables)
	if contains_type_variables(t1) || contains_type_variables(t2) {
		return false
	}

	return t1.name == t2.name && t1.params.len == t2.params.len
}

// Unification with occurs check
pub fn unify_with_occurs_check(t1 ast.Type, t2 ast.Type) !Substitution {
	mut unifier := new_unifier()
	return unifier.unify(t1, t2, ast.Position{})
}

// Unification for polymorphic types
pub fn unify_polymorphic_types(t1 ast.Type, t2 ast.Type, quantified_vars []string) !Substitution {
	mut unifier := new_unifier()

	// Apply unification
	mut result := unifier.unify(t1, t2, ast.Position{})!

	// Remove substitutions for quantified variables
	for var_name in quantified_vars {
		result.mappings.delete(var_name)
	}

	return result
}
