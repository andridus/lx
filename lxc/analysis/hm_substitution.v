module analysis

// Substitution represents a substitution in the HM system
// Maps type variable IDs to their substituted types
pub struct Substitution {
pub mut:
	subst map[int]TypeInfo // TypeVar.id -> TypeInfo
}

// new_substitution creates an empty substitution
pub fn new_substitution() Substitution {
	return Substitution{
		subst: map[int]TypeInfo{}
	}
}

// singleton creates a substitution with a single mapping
pub fn singleton_substitution(var_id int, type_info TypeInfo) Substitution {
	mut s := new_substitution()
	s.subst[var_id] = type_info
	return s
}

// apply_to_type applies substitution to a type
pub fn (s Substitution) apply_to_type(t TypeInfo) TypeInfo {
	// For now, we'll work with the existing TypeInfo structure
	// In a full HM implementation, we'd need to handle type variables properly

	// If this is a type variable, substitute it
	if t.generic == 'typevar' {
		if value := t.value {
			// Parse the type variable ID from the value
			var_id := value.int()
			if substituted := s.subst[var_id] {
				return substituted
			}
		}
	}

	// For complex types, recursively apply substitution
	if t.generic == 'list' && t.values.len > 0 {
		element_type := s.apply_to_type(t.values[0])
		return typeinfo_list(element_type)
	}

	if t.generic == 'map' && t.values.len >= 2 {
		key_type := s.apply_to_type(t.values[0])
		value_type := s.apply_to_type(t.values[1])
		return typeinfo_map(key_type, value_type)
	}

	if t.generic == 'tuple' && t.values.len > 0 {
		element_types := t.values.map(s.apply_to_type(it))
		return typeinfo_tuple(element_types)
	}

	if t.generic == 'union' && t.values.len > 0 {
		union_types := t.values.map(s.apply_to_type(it))
		return typeinfo_union(union_types)
	}

	// For primitive types, return as-is
	return t
}

// apply_to_scheme applies substitution to a type scheme
pub fn (s Substitution) apply_to_scheme(scheme TypeScheme) TypeScheme {
	// Remove bound variables from substitution
	mut filtered_subst := map[int]TypeInfo{}
	for var_id, type_info in s.subst {
		mut is_bound := false
		for bound_var in scheme.vars {
			if bound_var.id == var_id {
				is_bound = true
				break
			}
		}
		if !is_bound {
			filtered_subst[var_id] = type_info
		}
	}

	filtered_s := Substitution{
		subst: filtered_subst
	}

	return TypeScheme{
		vars:      scheme.vars
		type_info: filtered_s.apply_to_type(scheme.type_info)
	}
}

// apply_to_env applies substitution to a type environment
pub fn (s Substitution) apply_to_env(env TypeEnv) TypeEnv {
	mut new_bindings := map[string]TypeScheme{}
	for name, scheme in env.bindings {
		new_bindings[name] = s.apply_to_scheme(scheme)
	}

	return TypeEnv{
		bindings: new_bindings
		parent:   env.parent
	}
}

// compose composes two substitutions (s1 ∘ s2)
pub fn (s1 Substitution) compose(s2 Substitution) Substitution {
	mut result := new_substitution()

	// Apply s1 to all types in s2
	for var_id, type_info in s2.subst {
		result.subst[var_id] = s1.apply_to_type(type_info)
	}

	// Add mappings from s1 that are not in s2
	for var_id, type_info in s1.subst {
		if var_id !in s2.subst {
			result.subst[var_id] = type_info
		}
	}

	return result
}

// identity returns the identity substitution
pub fn identity_substitution() Substitution {
	return new_substitution()
}

// str returns a string representation of the substitution
pub fn (s Substitution) str() string {
	mut mappings := []string{}
	for var_id, type_info in s.subst {
		mappings << '${var_id} -> ${type_info.str()}'
	}
	return '[${mappings.join(', ')}]'
}

// is_empty checks if the substitution is empty
pub fn (s Substitution) is_empty() bool {
	return s.subst.len == 0
}

// domain returns the domain of the substitution (all variable IDs)
pub fn (s Substitution) domain() []int {
	mut vars := []int{}
	for var_id, _ in s.subst {
		vars << var_id
	}
	return vars
}

// Helper function to create a type variable TypeInfo
pub fn typeinfo_typevar(var_id int, name string) TypeInfo {
	return TypeInfo{
		generic: 'typevar'
		value:   var_id.str()
		values:  []
	}
}
