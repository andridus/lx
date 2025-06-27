module typechecker

// Substitution represents a mapping from type variables to type expressions
pub struct Substitution {
pub mut:
	mappings map[string]TypeExpr
}

// new_substitution creates a new empty substitution
pub fn new_substitution() Substitution {
	return Substitution{
		mappings: map[string]TypeExpr{}
	}
}

// add adds a mapping from a type variable to a type expression
pub fn (mut sub Substitution) add(var_id string, type_expr TypeExpr) {
	sub.mappings[var_id] = type_expr
}

// get retrieves the type expression for a type variable
pub fn (sub &Substitution) get(var_id string) ?TypeExpr {
	return sub.mappings[var_id] or { none }
}

// has checks if a type variable has a mapping
pub fn (sub &Substitution) has(var_id string) bool {
	return var_id in sub.mappings
}

// apply applies the substitution to a type expression
pub fn (sub &Substitution) apply(type_expr TypeExpr) TypeExpr {
	return match type_expr {
		TypeVar {
			if mapped_type := sub.get(type_expr.id) {
				// Recursively apply substitution to avoid cycles
				sub.apply(mapped_type)
			} else {
				type_expr
			}
		}
		TypeConstructor {
			TypeConstructor{
				name:       type_expr.name
				parameters: type_expr.parameters.map(sub.apply(it))
			}
		}
		FunctionType {
			FunctionType{
				parameters:  type_expr.parameters.map(sub.apply(it))
				return_type: sub.apply(type_expr.return_type)
			}
		}
		RecordType {
			mut new_fields := map[string]TypeExpr{}
			for field_name, field_type in type_expr.fields {
				new_fields[field_name] = sub.apply(field_type)
			}
			RecordType{
				name:   type_expr.name
				fields: new_fields
			}
		}
		MapType {
			MapType{
				key_type:   sub.apply(type_expr.key_type)
				value_type: sub.apply(type_expr.value_type)
			}
		}
		TupleType {
			TupleType{
				element_types: type_expr.element_types.map(sub.apply(it))
			}
		}
		ListType {
			ListType{
				element_type: sub.apply(type_expr.element_type)
			}
		}
		BinaryType {
			type_expr
		}
	}
}

// compose composes this substitution with another substitution
pub fn (sub &Substitution) compose(other &Substitution) Substitution {
	mut result := new_substitution()

	// Add all mappings from the other substitution
	for var_id, type_expr in other.mappings {
		result.add(var_id, sub.apply(type_expr))
	}

	// Add mappings from this substitution that are not in the other
	for var_id, type_expr in sub.mappings {
		if !other.has(var_id) {
			result.add(var_id, type_expr)
		}
	}

	return result
}

// domain returns the set of type variables in the domain of the substitution
pub fn (sub &Substitution) domain() []string {
	return sub.mappings.keys()
}

// range returns the set of type variables in the range of the substitution
pub fn (sub &Substitution) range() []string {
	mut range_vars := []string{}
	for type_expr in sub.mappings.values() {
		range_vars << type_expr.get_type_vars()
	}
	return range_vars
}

// is_idempotent checks if the substitution is idempotent
pub fn (sub &Substitution) is_idempotent() bool {
	domain_vars := sub.domain()
	range_vars := sub.range()

	// Check if any variable in the domain appears in the range
	for domain_var in domain_vars {
		for range_var in range_vars {
			if domain_var == range_var {
				return false
			}
		}
	}

	return true
}

// make_idempotent makes the substitution idempotent by applying it to itself
pub fn (mut sub Substitution) make_idempotent() {
	for var_id, type_expr in sub.mappings {
		sub.mappings[var_id] = sub.apply(type_expr)
	}
}

// str returns a string representation of the substitution
pub fn (sub &Substitution) str() string {
	if sub.mappings.len == 0 {
		return '{}'
	}

	mut mappings := []string{}
	for var_id, type_expr in sub.mappings {
		mappings << '${var_id} -> ${type_expr.str()}'
	}

	return '{${mappings.join(', ')}}'
}

// empty checks if the substitution is empty
pub fn (sub &Substitution) empty() bool {
	return sub.mappings.len == 0
}

// size returns the number of mappings in the substitution
pub fn (sub &Substitution) size() int {
	return sub.mappings.len
}

// remove removes a mapping for a type variable
pub fn (mut sub Substitution) remove(var_id string) {
	sub.mappings.delete(var_id)
}

// clear removes all mappings
pub fn (mut sub Substitution) clear() {
	sub.mappings.clear()
}

// copy creates a copy of the substitution
pub fn (sub &Substitution) copy() Substitution {
	mut new_sub := new_substitution()
	for var_id, type_expr in sub.mappings {
		new_sub.add(var_id, type_expr)
	}
	return new_sub
}

// equals checks if two substitutions are equal
pub fn (sub &Substitution) equals(other &Substitution) bool {
	if sub.size() != other.size() {
		return false
	}

	for var_id, type_expr in sub.mappings {
		if other_type := other.get(var_id) {
			if type_expr.str() != other_type.str() {
				return false
			}
		} else {
			return false
		}
	}

	return true
}

// merge merges this substitution with another substitution
pub fn (sub &Substitution) merge(other &Substitution) ?Substitution {
	mut result := sub.copy()

	for var_id, type_expr in other.mappings {
		if existing_type := result.get(var_id) {
			// Check if the types are compatible
			if existing_type.str() != type_expr.str() {
				return none
			}
		} else {
			result.add(var_id, type_expr)
		}
	}

	return result
}

// restrict restricts the substitution to a set of type variables
pub fn (sub &Substitution) restrict(vars []string) Substitution {
	mut result := new_substitution()

	for var_id, type_expr in sub.mappings {
		if vars.contains(var_id) {
			result.add(var_id, type_expr)
		}
	}

	return result
}

// extend extends the substitution with another substitution
pub fn (mut sub Substitution) extend(other &Substitution) {
	for var_id, type_expr in other.mappings {
		sub.add(var_id, type_expr)
	}
}
