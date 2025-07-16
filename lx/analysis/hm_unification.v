module analysis

// unify implements the unification algorithm for HM type inference
pub fn unify(t1 TypeInfo, t2 TypeInfo) !Substitution {
	// If types are identical, return identity substitution
	if types_equal(t1, t2) {
		return identity_substitution()
	}

	// Type variable cases
	if t1.generic == 'typevar' {
		return unify_variable(t1, t2)
	}

	if t2.generic == 'typevar' {
		return unify_variable(t2, t1)
	}

	// Unify complex types
	if t1.generic == 'list' && t2.generic == 'list' {
		return unify_lists(t1, t2)
	}

	if t1.generic == 'map' && t2.generic == 'map' {
		return unify_maps(t1, t2)
	}

	if t1.generic == 'tuple' && t2.generic == 'tuple' {
		return unify_tuples(t1, t2)
	}

	if t1.generic == 'union' || t2.generic == 'union' {
		return unify_unions(t1, t2)
	}

	// If we get here, types cannot be unified
	return error('Cannot unify ${t1.str()} with ${t2.str()}')
}

// unify_variable unifies a type variable with another type
fn unify_variable(var_type TypeInfo, other_type TypeInfo) !Substitution {
	if var_type.generic != 'typevar' {
		return error('Expected type variable, got ${var_type.str()}')
	}

	if value := var_type.value {
		var_id := value.int()

		// Occurs check: prevent infinite types
		if occurs_check(var_id, other_type) {
			return error('Occurs check failed: ${var_type.str()} occurs in ${other_type.str()}')
		}

		return singleton_substitution(var_id, other_type)
	}

	return error('Type variable has no ID')
}

// unify_lists unifies two list types
fn unify_lists(t1 TypeInfo, t2 TypeInfo) !Substitution {
	if t1.values.len == 0 || t2.values.len == 0 {
		return error('List types must have element types')
	}

	return unify(t1.values[0], t2.values[0])
}

// unify_maps unifies two map types
fn unify_maps(t1 TypeInfo, t2 TypeInfo) !Substitution {
	if t1.values.len < 2 || t2.values.len < 2 {
		return error('Map types must have key and value types')
	}

	key_subst := unify(t1.values[0], t2.values[0])!
	value_subst := unify(key_subst.apply_to_type(t1.values[1]), key_subst.apply_to_type(t2.values[1]))!

	return value_subst.compose(key_subst)
}

// unify_tuples unifies two tuple types
fn unify_tuples(t1 TypeInfo, t2 TypeInfo) !Substitution {
	if t1.values.len != t2.values.len {
		return error('Tuple types must have same arity: ${t1.values.len} vs ${t2.values.len}')
	}

	mut subst := identity_substitution()
	for i in 0 .. t1.values.len {
		element_subst := unify(subst.apply_to_type(t1.values[i]), subst.apply_to_type(t2.values[i]))!
		subst = element_subst.compose(subst)
	}

	return subst
}

// unify_unions unifies union types
fn unify_unions(t1 TypeInfo, t2 TypeInfo) !Substitution {
	// For now, we'll use a simple approach for union types
	// In a full implementation, this would be more sophisticated

	if t1.generic == 'union' && t2.generic != 'union' {
		// Try to unify t2 with any type in the union
		for union_type in t1.values {
			if subst := unify(union_type, t2) {
				return subst
			}
		}
		return error('Cannot unify ${t2.str()} with union ${t1.str()}')
	}

	if t2.generic == 'union' && t1.generic != 'union' {
		// Try to unify t1 with any type in the union
		for union_type in t2.values {
			if subst := unify(t1, union_type) {
				return subst
			}
		}
		return error('Cannot unify ${t1.str()} with union ${t2.str()}')
	}

	// Both are unions - more complex case
	if t1.generic == 'union' && t2.generic == 'union' {
		// For simplicity, just check if they're equal
		if types_equal(t1, t2) {
			return identity_substitution()
		}
		return error('Cannot unify unions ${t1.str()} and ${t2.str()}')
	}

	return error('Unexpected union unification case')
}

// occurs_check implements the occurs check to prevent infinite types
fn occurs_check(var_id int, t TypeInfo) bool {
	if t.generic == 'typevar' {
		if value := t.value {
			return value.int() == var_id
		}
	}

	// Recursively check in complex types
	for sub_type in t.values {
		if occurs_check(var_id, sub_type) {
			return true
		}
	}

	return false
}

// types_equal checks if two types are structurally equal
fn types_equal(t1 TypeInfo, t2 TypeInfo) bool {
	if t1.generic != t2.generic {
		return false
	}

	if t1.value != t2.value {
		return false
	}

	if t1.values.len != t2.values.len {
		return false
	}

	for i in 0 .. t1.values.len {
		if !types_equal(t1.values[i], t2.values[i]) {
			return false
		}
	}

	return true
}

// most_general_unifier finds the most general unifier of two types
pub fn most_general_unifier(t1 TypeInfo, t2 TypeInfo) !Substitution {
	return unify(t1, t2)
}

// unify_many unifies a list of type pairs
pub fn unify_many(pairs [][]TypeInfo) !Substitution {
	mut subst := identity_substitution()

	for pair in pairs {
		if pair.len != 2 {
			return error('Each pair must have exactly 2 types')
		}

		pair_subst := unify(subst.apply_to_type(pair[0]), subst.apply_to_type(pair[1]))!
		subst = pair_subst.compose(subst)
	}

	return subst
}
