module tests

import parser

fn test_supervisor_new_syntax_without_equals() {
	code := '
	supervisor test_sup do
		strategy :one_for_one
		children [:worker1, :worker2]
	end
	'

	mut p := parser.new_parser(code, 'test.lx', parser.new_directives_table())

	result := p.parse() or { panic('Failed to parse supervisor with new syntax: ${err}') }

	// Should be a module containing a supervisor
	assert result.kind == .module
	assert result.children.len > 0

	// Find the supervisor in the children
	mut supervisor_found := false
	for child in result.children {
		if child.kind == .supervisor_def {
			supervisor_found = true
			assert child.value == 'test_sup'

			// Check supervisor body
			body := child.children[0]
			assert body.kind == .block

			// Should have strategy and children as variable_binding nodes
			mut strategy_found := false
			mut children_found := false

			for expr in body.children {
				if expr.kind == .variable_binding {
					if expr.value == 'strategy' {
						strategy_found = true
						assert expr.children.len == 1
						assert expr.children[0].kind == .atom
						assert expr.children[0].value == 'one_for_one'
					} else if expr.value == 'children' {
						children_found = true
						assert expr.children.len == 1
						assert expr.children[0].kind == .list_literal
						assert expr.children[0].children.len == 2
					}
				}
			}

			assert strategy_found, 'Strategy binding not found'
			assert children_found, 'Children binding not found'
			break
		}
	}

	assert supervisor_found, 'Supervisor definition not found'
}

fn test_supervisor_old_syntax_with_equals_is_rejected() {
	code := '
	supervisor test_sup do
		strategy = :one_for_one
		children = [:worker1, :worker2]
	end
	'

	mut p := parser.new_parser(code, 'test.lx', parser.new_directives_table())

	p.parse() or {
		// Should fail with a parse error about using old syntax
		error_msg := err.msg()
		assert error_msg.contains('Supervisor directives should not use "=". Use "strategy :value"')
		return
	}

	// If we reach here, the parse didn't fail as expected
	assert false, 'Expected old syntax to be rejected, but parsing succeeded'
}

fn test_supervisor_mixed_syntax_rejects_equals() {
	code := '
	supervisor test_sup do
		strategy :one_for_one
		children = [:worker1]
	end
	'

	mut p := parser.new_parser(code, 'test.lx', parser.new_directives_table())

	p.parse() or {
		// Should fail when encountering the '=' in children line
		error_msg := err.msg()
		assert error_msg.contains('Supervisor directives should not use "=". Use "children :value"')
		return
	}

	// If we reach here, the parse didn't fail as expected
	assert false, 'Expected mixed syntax to be rejected, but parsing succeeded'
}

fn test_supervisor_context_only_affects_strategy_and_children() {
	code := '
	supervisor test_sup do
		strategy :one_for_one
		children [:worker1]
		other_var = "test"
	end
	'

	mut p := parser.new_parser(code, 'test.lx', parser.new_directives_table())

	result := p.parse() or { panic('Failed to parse mixed supervisor syntax: ${err}') }

	// Should be a module containing a supervisor
	assert result.kind == .module
	assert result.children.len > 0

	// Find the supervisor in the children
	mut supervisor_found := false
	for child in result.children {
		if child.kind == .supervisor_def {
			supervisor_found = true
			body := child.children[0]
			assert body.kind == .block

			// All three should be variable_binding nodes
			mut bindings_found := 0
			for expr in body.children {
				if expr.kind == .variable_binding {
					bindings_found++
					assert expr.value in ['strategy', 'children', 'other_var']
				}
			}

			assert bindings_found == 3, 'Expected 3 variable bindings, found ${bindings_found}'
			break
		}
	}

	assert supervisor_found, 'Supervisor definition not found'
}
