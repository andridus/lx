module analysis

import ast
import errors

pub fn solve_constraints(constraints []Constraint) !Substitution {
	mut solver := ConstraintSolver{
		constraints: constraints
		error_reporter: errors.new_error_reporter()
	}

	return solver.solve()
}

pub struct ConstraintSolver {
mut:
	constraints   []Constraint
	substitution  Substitution
	error_reporter errors.ErrorReporter
}

pub fn new_constraint_solver(constraints []Constraint) ConstraintSolver {
	return ConstraintSolver{
		constraints: constraints
		substitution: Substitution{
			mappings: map[string]ast.Type{}
		}
		error_reporter: errors.new_error_reporter()
	}
}

pub fn (mut solver ConstraintSolver) solve() !Substitution {
	// Apply occurs check and solve constraints
	for constraint in solver.constraints {
		solver.solve_constraint(constraint)!
	}

	return solver.substitution
}

fn (mut solver ConstraintSolver) solve_constraint(constraint Constraint) ! {
	left := constraint.left
	right := constraint.right

	// Apply current substitution to both sides
	left_substituted := substitute_in_type(left, solver.substitution)
	right_substituted := substitute_in_type(right, solver.substitution)

	// Try to unify the types
	new_substitution := solver.unify_types(left_substituted, right_substituted, constraint.position)!

	// Compose with current substitution
	solver.substitution = compose_substitutions(solver.substitution, new_substitution)
}

fn (mut solver ConstraintSolver) unify_types(t1 ast.Type, t2 ast.Type, pos ast.Position) !Substitution {
	// Handle type variables
	if t1.name.starts_with('T') && t1.params.len == 0 {
		return solver.unify_variable(t1.name, t2, pos)
	}

	if t2.name.starts_with('T') && t2.params.len == 0 {
		return solver.unify_variable(t2.name, t1, pos)
	}

	// Handle ground types
	if t1.name == t2.name {
		if t1.params.len == 0 {
			// Both are ground types with no parameters
			return Substitution{mappings: map[string]ast.Type{}}
		}

		if t1.params.len == t2.params.len {
			// Unify parameters
			mut result := Substitution{mappings: map[string]ast.Type{}}
			for i in 0..t1.params.len {
				param_substitution := solver.unify_types(t1.params[i], t2.params[i], pos)!
				result = compose_substitutions(result, param_substitution)
			}
			return result
		}
	}

	// Types are incompatible
	solver.error('Type mismatch: ${t1.str()} vs ${t2.str()}', pos)
	return error('Type mismatch')
}

fn (mut solver ConstraintSolver) unify_variable(var_name string, typ ast.Type, pos ast.Position) !Substitution {
	// Check if variable is already bound
	if existing := solver.substitution.mappings[var_name] {
		return solver.unify_types(existing, typ, pos)
	}

	// Check occurs check
	if occurs_in(var_name, typ) {
		solver.error('Occurs check failed: ${var_name} occurs in ${typ.str()}', pos)
		return error('Occurs check failed')
	}

	// Create new substitution
	return Substitution{
		mappings: {
			var_name: typ
		}
	}
}

fn occurs_in(var_name string, typ ast.Type) bool {
	if typ.name == var_name {
		return true
	}

	for param in typ.params {
		if occurs_in(var_name, param) {
			return true
		}
	}

	return false
}

fn (mut solver ConstraintSolver) error(msg string, pos ast.Position) {
	solver.error_reporter.report(.analysis, msg, pos)
}

pub fn collect_constraints_from_expression(expr ast.Node) []Constraint {
	mut collector := ConstraintCollector{
		constraints: []
	}
	collector.collect(expr)
	return collector.constraints
}

pub struct ConstraintCollector {
mut:
	constraints []Constraint
}

fn (mut collector ConstraintCollector) collect(node ast.Node) {
	match node.kind {
		.function_caller {
			collector.collect_function_call(node)
		}
		.list_cons {
			collector.collect_list_cons(node)
		}
		.map_access {
			collector.collect_map_access(node)
		}
		.block {
			collector.collect_block(node)
		}
		else {
			// Recursively collect from children
			for child in node.children {
				collector.collect(child)
			}
		}
	}
}

fn (mut collector ConstraintCollector) collect_function_call(node ast.Node) {
	if node.children.len < 2 {
		return
	}

	func_name := node.children[0].value
	args := node.children[1..]

	// Collect constraints for arguments
	for arg in args {
		collector.collect(arg)
	}

	// Add constraints for built-in functions
	match func_name {
		'+', '-', '*', '/' {
			if args.len == 2 {
				collector.add_constraint(args[0], type_integer(), node.position)
				collector.add_constraint(args[1], type_integer(), node.position)
			}
		}
		'==', '!=', '<', '<=', '>', '>=' {
			if args.len == 2 {
				collector.add_constraint_nodes(args[0], args[1], node.position)
			}
		}
		'and', 'or' {
			if args.len == 2 {
				collector.add_constraint(args[0], type_boolean(), node.position)
				collector.add_constraint(args[1], type_boolean(), node.position)
			}
		}
		else {}
	}
}

fn (mut collector ConstraintCollector) collect_list_cons(node ast.Node) {
	if node.children.len != 2 {
		return
	}

	head := node.children[0]
	tail := node.children[1]

	// Collect constraints for head and tail
	collector.collect(head)
	collector.collect(tail)

	// Add constraint: tail should be a list of head's type
	head_type := infer_type(head)
	_ := infer_type(tail)
	_ := ast.Type{
		name: 'list'
		params: [head_type]
	}
	collector.add_constraint_nodes(tail, node.children[1], node.position)
}

fn (mut collector ConstraintCollector) collect_map_access(node ast.Node) {
	if node.children.len != 2 {
		return
	}

	map_expr := node.children[0]
	key_expr := node.children[1]

	// Collect constraints for map and key
	collector.collect(map_expr)
	collector.collect(key_expr)

	// Add constraint: map should have type map(K, V) where K matches key type
	map_type := infer_type(map_expr)
	_ := infer_type(key_expr)

	if map_type.name == 'map' && map_type.params.len == 2 {
		collector.add_constraint_nodes(key_expr, node.children[0], node.position)
	}
}

fn (mut collector ConstraintCollector) collect_block(node ast.Node) {
	// Collect constraints from all expressions in the block
	for child in node.children {
		collector.collect(child)
	}
}

fn (mut collector ConstraintCollector) add_constraint(left ast.Node, right ast.Type, pos ast.Position) {
	left_type := infer_type(left)
	collector.constraints << Constraint{
		left: left_type
		right: right
		position: pos
	}
}

fn (mut collector ConstraintCollector) add_constraint_nodes(left ast.Node, right ast.Node, pos ast.Position) {
	left_type := infer_type(left)
	right_type := infer_type(right)
	collector.constraints << Constraint{
		left: left_type
		right: right_type
		position: pos
	}
}

// Simplified type inference for constraint collection
fn infer_type(node ast.Node) ast.Type {
	return match node.kind {
		.integer { type_integer() }
		.float { type_float() }
		.string { type_string() }
		.boolean { type_boolean() }
		.atom { type_atom() }
		.nil { type_nil() }
		.variable_ref { ast.Type{name: 'T1', params: []} } // Placeholder
		.list_literal {
			if node.children.len == 0 {
				ast.Type{name: 'list', params: [ast.Type{name: 'any', params: []}]}
			} else {
				ast.Type{name: 'list', params: [infer_type(node.children[0])]}
			}
		}
		.tuple_literal {
			mut params := []ast.Type{}
			for child in node.children {
				params << infer_type(child)
			}
			ast.Type{name: 'tuple', params: params}
		}
		.map_literal {
			ast.Type{name: 'map', params: [ast.Type{name: 'any', params: []}, ast.Type{name: 'any', params: []}]}
		}
		else { ast.Type{name: 'any', params: []} }
	}
}