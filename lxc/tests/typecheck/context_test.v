module main

import typechecker
import ast { Position }

fn test_type_binding() {
	// Test type binding creation
	binding := typechecker.TypeBinding{
		name:      'x'
		type_expr: typechecker.integer_type
		position:  Position{
			line:   1
			column: 1
		}
	}
	assert binding.str() == 'x: integer'
}

fn test_type_context_creation() {
	// Test context creation
	ctx := typechecker.new_context()
	assert ctx.level == 0
	assert ctx.bindings.len == 0
	assert ctx.parent == none
}

fn test_type_context_binding() {
	// Test adding bindings to context
	mut ctx := typechecker.new_context()
	pos := Position{
		line:   1
		column: 1
	}

	ctx.bind('x', typechecker.integer_type, pos)
	ctx.bind('y', typechecker.string_type, pos)

	assert ctx.bindings.len == 2
	assert ctx.has_binding('x') == true
	assert ctx.has_binding('y') == true
	assert ctx.has_binding('z') == false
}

fn test_type_context_lookup() {
	// Test looking up bindings
	mut ctx := typechecker.new_context()
	pos := Position{
		line:   1
		column: 1
	}

	ctx.bind('x', typechecker.integer_type, pos)

	if binding := ctx.lookup('x') {
		assert binding.name == 'x'
		assert binding.type_expr.str() == typechecker.integer_type.str()
	} else {
		assert false, 'Should find binding for x'
	}

	if binding := ctx.lookup('y') {
		assert false, 'Should not find binding for y'
	} else {
		assert true, 'Correctly not found binding for y'
	}
}

fn test_type_context_lookup_local() {
	// Test local lookup
	mut ctx := typechecker.new_context()
	pos := Position{
		line:   1
		column: 1
	}

	ctx.bind('x', typechecker.integer_type, pos)

	if binding := ctx.lookup_local('x') {
		assert binding.name == 'x'
		assert binding.type_expr.str() == typechecker.integer_type.str()
	} else {
		assert false, 'Should find local binding for x'
	}

	if binding := ctx.lookup_local('y') {
		assert false, 'Should not find local binding for y'
	} else {
		assert true, 'Correctly not found local binding for y'
	}
}

fn test_type_context_child() {
	// Test child context creation
	parent := typechecker.new_context()
	child := parent.new_child_context()

	assert child.parent != none
	assert child.level == 1
	assert child.bindings.len == 0
}

fn test_type_context_inheritance() {
	// Test binding inheritance from parent context
	mut parent := typechecker.new_context()
	pos := Position{
		line:   1
		column: 1
	}

	parent.bind('x', typechecker.integer_type, pos)

	child := parent.new_child_context()

	// Child should inherit parent's binding
	if binding := child.lookup('x') {
		assert binding.name == 'x'
		assert binding.type_expr.str() == typechecker.integer_type.str()
	} else {
		assert false, 'Child should inherit parent binding'
	}
}

fn test_type_context_shadowing() {
	// Test binding shadowing
	mut parent := typechecker.new_context()
	mut child := parent.new_child_context()
	pos := Position{
		line:   1
		column: 1
	}

	// Parent has x: integer
	parent.bind('x', typechecker.integer_type, pos)

	// Child has x: string (shadows parent)
	child.bind('x', typechecker.string_type, pos)

	// Child should see its own binding
	if binding := child.lookup('x') {
		assert binding.name == 'x'
		assert binding.type_expr.str() == typechecker.string_type.str()
	} else {
		assert false, 'Child should see its own binding'
	}

	// Parent should still see its own binding
	if binding := parent.lookup('x') {
		assert binding.name == 'x'
		assert binding.type_expr.str() == typechecker.integer_type.str()
	} else {
		assert false, 'Parent should see its own binding'
	}
}

fn test_type_context_removal() {
	// Test removing bindings
	mut ctx := typechecker.new_context()
	pos := Position{
		line:   1
		column: 1
	}

	ctx.bind('x', typechecker.integer_type, pos)
	ctx.bind('y', typechecker.string_type, pos)

	assert ctx.bindings.len == 2

	ctx.remove_binding('x')
	assert ctx.bindings.len == 1
	assert ctx.has_binding('x') == false
	assert ctx.has_binding('y') == true
}

fn test_type_context_get_bindings() {
	// Test getting all bindings
	mut ctx := typechecker.new_context()
	pos := Position{
		line:   1
		column: 1
	}

	ctx.bind('x', typechecker.integer_type, pos)
	ctx.bind('y', typechecker.string_type, pos)

	bindings := ctx.get_all_bindings()
	assert bindings.len == 2

	names := bindings.map(it.name)
	assert 'x' in names
	assert 'y' in names
}

fn test_type_context_recursive_bindings() {
	// Test getting recursive bindings
	mut parent := typechecker.new_context()
	mut child := parent.new_child_context()
	pos := Position{
		line:   1
		column: 1
	}

	parent.bind('x', typechecker.integer_type, pos)
	child.bind('y', typechecker.string_type, pos)

	parent_bindings := parent.get_all_bindings_recursive()
	assert parent_bindings.len == 1

	child_bindings := child.get_all_bindings_recursive()
	assert child_bindings.len == 2

	child_names := child_bindings.map(it.name)
	assert 'x' in child_names
	assert 'y' in child_names
}

fn test_type_scope_creation() {
	// Test scope creation
	scope := typechecker.new_scope()
	assert scope.context.level == 0
	assert scope.level == 0
}

fn test_type_scope_enter_exit() {
	// Test entering and exiting scopes
	mut scope := typechecker.new_scope()
	pos := Position{
		line:   1
		column: 1
	}

	// Add binding in current scope
	scope.bind('x', typechecker.integer_type, pos)
	assert scope.has_binding('x') == true

	// Enter new scope
	scope.enter_scope()

	// Add binding in new scope
	scope.bind('y', typechecker.string_type, pos)
	assert scope.has_binding('y') == true
	assert scope.has_binding('x') == true // Should inherit from parent

	// Exit scope
	scope.exit_scope()
	assert scope.has_binding('x') == true
	assert scope.has_binding('y') == false // Should be gone
}

fn test_type_environment_creation() {
	// Test environment creation
	env := typechecker.new_environment()
	assert env.builtin_types.len > 0
	assert env.module_types.len == 0
	assert env.record_types.len == 0
}

fn test_type_environment_lookup() {
	// Test environment lookup
	env := typechecker.new_environment()

	// Built-in types should be available
	assert env.has_type('integer') == true
	assert env.has_type('string') == true
	assert env.has_type('boolean') == true

	// Custom types should not be available initially
	assert env.has_type('Custom') == false
}

fn test_type_environment_registration() {
	// Test environment registration
	mut env := typechecker.new_environment()

	// Add some custom types
	custom_type := typechecker.TypeConstructor{
		name:       'Custom'
		parameters: []
	}
	env.register_module_type('Custom', custom_type)

	assert env.has_type('Custom') == true
}

fn test_type_environment_record_types() {
	// Test record type registration
	mut env := typechecker.new_environment()

	person_record := typechecker.RecordType{
		name:   'Person'
		fields: {
			'name': typechecker.string_type
			'age':  typechecker.integer_type
		}
	}
	env.register_record_type('Person', person_record)

	assert env.has_type('Person') == true
}

fn test_type_environment_string_representation() {
	// Test environment string representation
	env := typechecker.new_environment()

	env_str := env.str()
	assert env_str.contains('Type Environment')
	assert env_str.contains('integer')
	assert env_str.contains('string')
	assert env_str.contains('boolean')
}

fn main() {
	test_type_binding()
	test_type_context_creation()
	test_type_context_binding()
	test_type_context_lookup()
	test_type_context_lookup_local()
	test_type_context_child()
	test_type_context_inheritance()
	test_type_context_shadowing()
	test_type_context_removal()
	test_type_context_get_bindings()
	test_type_context_recursive_bindings()
	test_type_scope_creation()
	test_type_scope_enter_exit()
	test_type_environment_creation()
	test_type_environment_lookup()
	test_type_environment_registration()
	test_type_environment_record_types()
	test_type_environment_string_representation()
}
