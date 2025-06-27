module main

import typechecker

fn test_substitution_creation() {
	// Test substitution creation
	sub := typechecker.new_substitution()
	assert sub.empty() == true
	assert sub.domain() == []
	assert sub.range() == []
}

fn test_substitution_add_get() {
	// Test adding and getting from substitution
	mut sub := typechecker.new_substitution()
	tv := typechecker.TypeVar{
		id:   'T1'
		name: ''
	}

	sub.add('T1', tv)
	assert sub.has('T1') == true
	if type_expr := sub.get('T1') {
		assert type_expr.str() == 'T1'
	} else {
		assert false, 'Should find mapping for T1'
	}
	assert sub.has('T2') == false
}

fn test_substitution_apply_basic() {
	// Test basic substitution application
	mut sub := typechecker.new_substitution()
	tv := typechecker.TypeVar{
		id:   'T1'
		name: 'a'
	}
	sub.add('T1', typechecker.integer_type)

	// Apply substitution to type variable
	result := sub.apply(tv)
	assert result.str() == typechecker.integer_type.str()

	// Apply substitution to type constructor (should be unchanged)
	result2 := sub.apply(typechecker.string_type)
	assert result2.str() == typechecker.string_type.str()
}

fn test_substitution_apply_complex() {
	// Test complex substitution application
	mut sub := typechecker.new_substitution()
	tv1 := typechecker.TypeVar{
		id:   'T1'
		name: 'a'
	}
	tv2 := typechecker.TypeVar{
		id:   'T2'
		name: 'b'
	}

	sub.add('T1', typechecker.integer_type)
	sub.add('T2', typechecker.string_type)

	// Apply to function type
	fn_type := typechecker.make_function_type([tv1], tv2)
	result := sub.apply(fn_type)
	expected := typechecker.make_function_type([typechecker.integer_type], typechecker.string_type)
	assert result.str() == expected.str()
}

fn test_substitution_apply_recursive() {
	// Test recursive substitution application
	mut sub := typechecker.new_substitution()
	tv1 := typechecker.TypeVar{
		id:   'T1'
		name: 'a'
	}
	tv2 := typechecker.TypeVar{
		id:   'T2'
		name: 'b'
	}

	// T1 -> T2, T2 -> integer
	sub.add('T1', tv2)
	sub.add('T2', typechecker.integer_type)

	result := sub.apply(tv1)
	assert result.str() == typechecker.integer_type.str()
}

fn test_substitution_compose() {
	// Test substitution composition
	mut sub1 := typechecker.new_substitution()
	mut sub2 := typechecker.new_substitution()

	tv1 := typechecker.TypeVar{
		id:   'T1'
		name: 'a'
	}
	tv2 := typechecker.TypeVar{
		id:   'T2'
		name: 'b'
	}

	sub1.add('T1', tv2)
	sub2.add('T2', typechecker.integer_type)

	composed := sub1.compose(&sub2)
	assert composed.has('T1') == true
	assert composed.has('T2') == true

	result := composed.apply(tv1)
	assert result.str() == typechecker.integer_type.str()
}

fn test_substitution_compose_overlap() {
	// Test substitution composition with overlapping domains
	mut sub1 := typechecker.new_substitution()
	mut sub2 := typechecker.new_substitution()

	tv1 := typechecker.TypeVar{
		id:   'T1'
		name: 'a'
	}
	tv2 := typechecker.TypeVar{
		id:   'T2'
		name: 'b'
	}

	sub1.add('T1', tv2)
	sub2.add('T1', typechecker.integer_type) // Overlapping domain
	sub2.add('T2', typechecker.string_type)

	composed := sub1.compose(&sub2)
	// T1 should be mapped to integer (from sub2)
	result := composed.apply(tv1)
	assert result.str() == typechecker.integer_type.str()
}

fn test_substitution_domain_range() {
	// Test domain and range operations
	mut sub := typechecker.new_substitution()
	tv1 := typechecker.TypeVar{
		id:   'T1'
		name: 'a'
	}
	tv2 := typechecker.TypeVar{
		id:   'T2'
		name: 'b'
	}

	sub.add('T1', typechecker.integer_type)
	sub.add('T2', typechecker.string_type)

	domain := sub.domain()
	assert domain.len == 2
	assert 'T1' in domain
	assert 'T2' in domain

	range := sub.range()
	assert range.len == 0 // No type variables in range
}

fn test_substitution_idempotent() {
	// Test that applying a substitution twice is the same as applying it once
	mut sub := typechecker.new_substitution()
	tv := typechecker.TypeVar{
		id:   'T1'
		name: 'a'
	}

	sub.add('T1', typechecker.integer_type)

	result1 := sub.apply(tv)
	result2 := sub.apply(result1)
	assert result1.str() == result2.str()
}

fn test_substitution_make_idempotent() {
	// Test making a substitution idempotent
	mut sub := typechecker.new_substitution()
	tv1 := typechecker.TypeVar{
		id:   'T1'
		name: 'a'
	}
	tv2 := typechecker.TypeVar{
		id:   'T2'
		name: 'b'
	}

	// T1 -> T2, T2 -> integer
	sub.add('T1', tv2)
	sub.add('T2', typechecker.integer_type)

	sub.make_idempotent()
	assert sub.has('T1') == true
	assert sub.has('T2') == true

	result := sub.apply(tv1)
	assert result.str() == typechecker.integer_type.str()
}

fn test_substitution_copy() {
	// Test substitution copying
	mut sub1 := typechecker.new_substitution()
	tv := typechecker.TypeVar{
		id:   'T1'
		name: 'a'
	}
	sub1.add('T1', typechecker.integer_type)

	sub2 := sub1.copy()
	assert sub2.has('T1') == true
	if type_expr := sub2.get('T1') {
		assert type_expr.str() == typechecker.integer_type.str()
	} else {
		assert false, 'Should find mapping for T1'
	}

	// Modifying copy should not affect original
	mut sub2_mut := sub2
	sub2_mut.add('T2', typechecker.string_type)
	assert sub1.has('T2') == false
}

fn test_substitution_equals() {
	// Test substitution equality
	mut sub1 := typechecker.new_substitution()
	mut sub2 := typechecker.new_substitution()
	mut sub3 := typechecker.new_substitution()

	tv := typechecker.TypeVar{
		id:   'T1'
		name: 'a'
	}
	sub1.add('T1', typechecker.integer_type)
	sub2.add('T1', typechecker.integer_type)
	sub3.add('T1', typechecker.string_type)

	assert sub1.equals(&sub2) == true
	assert sub1.equals(&sub3) == false

	empty_sub := typechecker.new_substitution()
	assert sub1.equals(&empty_sub) == false
}

fn test_substitution_merge() {
	// Test substitution merging
	mut sub1 := typechecker.new_substitution()
	mut sub2 := typechecker.new_substitution()

	tv1 := typechecker.TypeVar{
		id:   'T1'
		name: 'a'
	}
	tv2 := typechecker.TypeVar{
		id:   'T2'
		name: 'b'
	}

	sub1.add('T1', typechecker.integer_type)
	sub2.add('T2', typechecker.string_type)

	if merged := sub1.merge(&sub2) {
		assert merged.has('T1') == true
		assert merged.has('T2') == true
		if type_expr := merged.get('T1') {
			assert type_expr.str() == typechecker.integer_type.str()
		}
		if type_expr := merged.get('T2') {
			assert type_expr.str() == typechecker.string_type.str()
		}
	} else {
		assert false, 'Should be able to merge compatible substitutions'
	}
}

fn test_substitution_merge_conflict() {
	// Test substitution merging with conflicts
	mut sub1 := typechecker.new_substitution()
	mut sub2 := typechecker.new_substitution()

	tv := typechecker.TypeVar{
		id:   'T1'
		name: 'a'
	}

	sub1.add('T1', typechecker.integer_type)
	sub2.add('T1', typechecker.string_type) // Conflict

	if merged := sub1.merge(&sub2) {
		assert false, 'Should not be able to merge conflicting substitutions'
	} else {
		assert true, 'Correctly failed to merge conflicting substitutions'
	}
}

fn test_substitution_restrict() {
	// Test substitution restriction
	mut sub := typechecker.new_substitution()
	tv1 := typechecker.TypeVar{
		id:   'T1'
		name: 'a'
	}
	tv2 := typechecker.TypeVar{
		id:   'T2'
		name: 'b'
	}

	sub.add('T1', typechecker.integer_type)
	sub.add('T2', typechecker.string_type)

	restricted := sub.restrict(['T1'])
	assert restricted.has('T1') == true
	assert restricted.has('T2') == false
}

fn test_substitution_extend() {
	// Test substitution extension
	mut sub := typechecker.new_substitution()
	mut other := typechecker.new_substitution()

	sub.add('T1', typechecker.integer_type)
	other.add('T2', typechecker.string_type)

	sub.extend(&other)
	assert sub.has('T1') == true
	assert sub.has('T2') == true
	if type_expr := sub.get('T1') {
		assert type_expr.str() == typechecker.integer_type.str()
	}
	if type_expr := sub.get('T2') {
		assert type_expr.str() == typechecker.string_type.str()
	}
}

fn test_substitution_remove_clear() {
	// Test substitution removal and clearing
	mut sub := typechecker.new_substitution()
	tv1 := typechecker.TypeVar{
		id:   'T1'
		name: 'a'
	}
	tv2 := typechecker.TypeVar{
		id:   'T2'
		name: 'b'
	}

	sub.add('T1', typechecker.integer_type)
	sub.add('T2', typechecker.string_type)

	// Remove specific binding
	sub.remove('T1')
	assert sub.has('T1') == false
	assert sub.has('T2') == true

	// Clear all bindings
	sub.clear()
	assert sub.has('T2') == false
	assert sub.empty() == true
}

fn main() {
	test_substitution_creation()
	test_substitution_add_get()
	test_substitution_apply_basic()
	test_substitution_apply_complex()
	test_substitution_apply_recursive()
	test_substitution_compose()
	test_substitution_compose_overlap()
	test_substitution_domain_range()
	test_substitution_idempotent()
	test_substitution_make_idempotent()
	test_substitution_copy()
	test_substitution_equals()
	test_substitution_merge()
	test_substitution_merge_conflict()
	test_substitution_restrict()
	test_substitution_extend()
	test_substitution_remove_clear()
}
