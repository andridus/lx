module typechecker

import ast { Position }

// UnificationError represents an error during type unification
pub struct UnificationError {
pub:
	message  string
	position Position
	left     TypeExpr
	right    TypeExpr
}

// str returns a string representation of UnificationError
pub fn (ue UnificationError) str() string {
	return 'Unification error at ${ue.position.str()}: ${ue.message}\n  Left: ${ue.left.str()}\n  Right: ${ue.right.str()}'
}

// UnificationResult represents the result of a unification operation
pub struct UnificationResult {
pub:
	substitution Substitution
	error        ?UnificationError
}

// success creates a successful unification result
pub fn success(substitution Substitution) UnificationResult {
	return UnificationResult{
		substitution: substitution
		error:        none
	}
}

// failure creates a failed unification result
pub fn failure(error UnificationError) UnificationResult {
	return UnificationResult{
		substitution: new_substitution()
		error:        error
	}
}

// Unifier provides type unification functionality
pub struct Unifier {
mut:
	var_counter int
}

// new_unifier creates a new unifier
pub fn new_unifier() Unifier {
	return Unifier{
		var_counter: 0
	}
}

// fresh_var creates a fresh type variable
pub fn (mut un Unifier) fresh_var() TypeVar {
	un.var_counter++
	return TypeVar{
		id:   'T${un.var_counter}'
		name: ''
	}
}

// fresh_var_named creates a fresh type variable with a name
pub fn (mut un Unifier) fresh_var_named(name string) TypeVar {
	un.var_counter++
	return TypeVar{
		id:   'T${un.var_counter}'
		name: name
	}
}

// occurs_check checks if a type variable occurs in a type expression
pub fn (un &Unifier) occurs_check(var_id string, type_expr TypeExpr) bool {
	return type_expr.contains_type_var(var_id)
}

// unify unifies two type expressions
pub fn (un &Unifier) unify(left TypeExpr, right TypeExpr, position Position) UnificationResult {
	match left {
		TypeVar {
			return un.unify_var(left, right, position)
		}
		TypeConstructor {
			return un.unify_constructor(left, right, position)
		}
		FunctionType {
			return un.unify_function(left, right, position)
		}
		RecordType {
			return un.unify_record(left, right, position)
		}
		MapType {
			return un.unify_map(left, right, position)
		}
		TupleType {
			return un.unify_tuple(left, right, position)
		}
		ListType {
			return un.unify_list(left, right, position)
		}
		BinaryType {
			return un.unify_binary(left, right, position)
		}
	}
}

// unify_var handles unification when the left side is a type variable
pub fn (un &Unifier) unify_var(left TypeVar, right TypeExpr, position Position) UnificationResult {
	// If left and right are the same variable, no substitution needed
	if right is TypeVar && left.id == right.id {
		return success(new_substitution())
	}

	// Check if the variable occurs in the right side (occurs check)
	if un.occurs_check(left.id, right) {
		return failure(UnificationError{
			message:  'Occurs check failed: type variable ${left.id} occurs in ${right.str()}'
			position: position
			left:     left
			right:    right
		})
	}

	// Create substitution: left -> right
	mut substitution := new_substitution()
	substitution.add(left.id, right)

	return success(substitution)
}

// unify_constructor handles unification of type constructors
pub fn (un &Unifier) unify_constructor(left TypeConstructor, right TypeExpr, position Position) UnificationResult {
	match right {
		TypeVar {
			// Swap and unify
			return un.unify_var(right, left, position)
		}
		TypeConstructor {
			// Check if constructors are the same
			if left.name != right.name {
				return failure(UnificationError{
					message:  'Cannot unify different type constructors: ${left.name} and ${right.name}'
					position: position
					left:     left
					right:    right
				})
			}

			// Check if parameter counts match
			if left.parameters.len != right.parameters.len {
				return failure(UnificationError{
					message:  'Type constructor ${left.name} has different parameter counts: ${left.parameters.len} vs ${right.parameters.len}'
					position: position
					left:     left
					right:    right
				})
			}

			// Unify parameters
			mut substitution := new_substitution()
			for i, left_param in left.parameters {
				right_param := right.parameters[i]
				result := un.unify(left_param, right_param, position)

				if result.error != none {
					return result
				}

				// Compose substitutions
				substitution = substitution.compose(&result.substitution)
			}

			return success(substitution)
		}
		else {
			return failure(UnificationError{
				message:  'Cannot unify type constructor with ${right.str()}'
				position: position
				left:     left
				right:    right
			})
		}
	}
}

// unify_function handles unification of function types
pub fn (un &Unifier) unify_function(left FunctionType, right TypeExpr, position Position) UnificationResult {
	match right {
		TypeVar {
			// Swap and unify
			return un.unify_var(right, left, position)
		}
		FunctionType {
			// Check if parameter counts match
			if left.parameters.len != right.parameters.len {
				return failure(UnificationError{
					message:  'Function types have different parameter counts: ${left.parameters.len} vs ${right.parameters.len}'
					position: position
					left:     left
					right:    right
				})
			}

			// Unify parameters
			mut substitution := new_substitution()
			for i, left_param in left.parameters {
				right_param := right.parameters[i]
				result := un.unify(left_param, right_param, position)

				if result.error != none {
					return result
				}

				// Compose substitutions
				substitution = substitution.compose(&result.substitution)
			}

			// Unify return types
			result := un.unify(left.return_type, right.return_type, position)
			if result.error != none {
				return result
			}

			// Compose substitutions
			substitution = substitution.compose(&result.substitution)

			return success(substitution)
		}
		else {
			return failure(UnificationError{
				message:  'Cannot unify function type with ${right.str()}'
				position: position
				left:     left
				right:    right
			})
		}
	}
}

// unify_record handles unification of record types
pub fn (un &Unifier) unify_record(left RecordType, right TypeExpr, position Position) UnificationResult {
	match right {
		TypeVar {
			// Swap and unify
			return un.unify_var(right, left, position)
		}
		RecordType {
			// Check if record names match
			if left.name != right.name {
				return failure(UnificationError{
					message:  'Cannot unify different record types: ${left.name} and ${right.name}'
					position: position
					left:     left
					right:    right
				})
			}

			// Check if field sets match
			left_fields := left.fields.keys()
			right_fields := right.fields.keys()

			if left_fields.len != right_fields.len {
				return failure(UnificationError{
					message:  'Record types have different field counts: ${left_fields.len} vs ${right_fields.len}'
					position: position
					left:     left
					right:    right
				})
			}

			// Check if all fields are present
			for field_name in left_fields {
				if !right_fields.contains(field_name) {
					return failure(UnificationError{
						message:  'Record type ${right.name} missing field: ${field_name}'
						position: position
						left:     left
						right:    right
					})
				}
			}

			// Unify field types
			mut substitution := new_substitution()
			for field_name, left_field_type in left.fields {
				right_field_type := right.fields[field_name] or {
					return failure(UnificationError{
						message:  'Field ${field_name} not found in right record type'
						position: position
						left:     left
						right:    right
					})
				}
				result := un.unify(left_field_type, right_field_type, position)

				if result.error != none {
					return result
				}

				// Compose substitutions
				substitution = substitution.compose(&result.substitution)
			}

			return success(substitution)
		}
		else {
			return failure(UnificationError{
				message:  'Cannot unify record type with ${right.str()}'
				position: position
				left:     left
				right:    right
			})
		}
	}
}

// unify_map handles unification of map types
pub fn (un &Unifier) unify_map(left MapType, right TypeExpr, position Position) UnificationResult {
	match right {
		TypeVar {
			// Swap and unify
			return un.unify_var(right, left, position)
		}
		MapType {
			// Unify key types
			key_result := un.unify(left.key_type, right.key_type, position)
			if key_result.error != none {
				return key_result
			}

			// Unify value types
			value_result := un.unify(left.value_type, right.value_type, position)
			if value_result.error != none {
				return value_result
			}

			// Compose substitutions
			substitution := key_result.substitution.compose(&value_result.substitution)

			return success(substitution)
		}
		else {
			return failure(UnificationError{
				message:  'Cannot unify map type with ${right.str()}'
				position: position
				left:     left
				right:    right
			})
		}
	}
}

// unify_tuple handles unification of tuple types
pub fn (un &Unifier) unify_tuple(left TupleType, right TypeExpr, position Position) UnificationResult {
	match right {
		TypeVar {
			// Swap and unify
			return un.unify_var(right, left, position)
		}
		TupleType {
			// Check if element counts match
			if left.element_types.len != right.element_types.len {
				return failure(UnificationError{
					message:  'Tuple types have different element counts: ${left.element_types.len} vs ${right.element_types.len}'
					position: position
					left:     left
					right:    right
				})
			}

			// Unify element types
			mut substitution := new_substitution()
			for i, left_element in left.element_types {
				right_element := right.element_types[i]
				result := un.unify(left_element, right_element, position)

				if result.error != none {
					return result
				}

				// Compose substitutions
				substitution = substitution.compose(&result.substitution)
			}

			return success(substitution)
		}
		else {
			return failure(UnificationError{
				message:  'Cannot unify tuple type with ${right.str()}'
				position: position
				left:     left
				right:    right
			})
		}
	}
}

// unify_list handles unification of list types
pub fn (un &Unifier) unify_list(left ListType, right TypeExpr, position Position) UnificationResult {
	match right {
		TypeVar {
			// Swap and unify
			return un.unify_var(right, left, position)
		}
		ListType {
			// Unify element types
			return un.unify(left.element_type, right.element_type, position)
		}
		else {
			return failure(UnificationError{
				message:  'Cannot unify list type with ${right.str()}'
				position: position
				left:     left
				right:    right
			})
		}
	}
}

// unify_binary handles unification of binary types
pub fn (un &Unifier) unify_binary(left BinaryType, right TypeExpr, position Position) UnificationResult {
	match right {
		TypeVar {
			// Swap and unify
			return un.unify_var(right, left, position)
		}
		BinaryType {
			// Binary types are compatible if they have the same unit size or one is variable
			if left.unit_size == 0 || right.unit_size == 0 || left.unit_size == right.unit_size {
				return success(new_substitution())
			} else {
				return failure(UnificationError{
					message:  'Cannot unify binary types with different unit sizes: ${left.unit_size} vs ${right.unit_size}'
					position: position
					left:     left
					right:    right
				})
			}
		}
		else {
			return failure(UnificationError{
				message:  'Cannot unify binary type with ${right.str()}'
				position: position
				left:     left
				right:    right
			})
		}
	}
}

// unify_many unifies multiple type expressions
pub fn (un &Unifier) unify_many(types_ []TypeExpr, position Position) UnificationResult {
	if types_.len < 2 {
		return success(new_substitution())
	}

	mut types := types_.clone()
	mut substitution := new_substitution()

	for i := 0; i < types.len - 1; i++ {
		// Sempre aplique a substituição acumulada antes de unificar
		types[i] = substitution.apply(types[i])
		types[i + 1] = substitution.apply(types[i + 1])

		result := un.unify(types[i], types[i + 1], position)
		if result.error != none {
			return result
		}

		// Atualize a substituição acumulada
		substitution = substitution.compose(&result.substitution)

		// Aplique a substituição acumulada em todos os tipos restantes
		for j := i + 2; j < types.len; j++ {
			types[j] = substitution.apply(types[j])
		}
	}

	return success(substitution)
}
