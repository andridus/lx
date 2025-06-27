module typechecker

import ast { Position }

// TypeBinding represents a type binding for a variable
pub struct TypeBinding {
pub:
	name      string
	type_expr TypeExpr
	position  Position
}

// str returns a string representation of TypeBinding
pub fn (tb TypeBinding) str() string {
	return '${tb.name}: ${tb.type_expr.str()}'
}

// TypeContext represents the type environment and scope
pub struct TypeContext {
pub mut:
	bindings map[string]TypeBinding
	parent   ?&TypeContext
	level    int // Scope level for shadowing
} @[heap]

// new_context creates a new type context
pub fn new_context() &TypeContext {
	return &TypeContext{
		bindings: map[string]TypeBinding{}
		parent: none
		level: 0
	}
}

// new_child_context creates a new child context
pub fn (ctx &TypeContext) new_child_context() &TypeContext {
	return unsafe {
		&TypeContext{
			bindings: map[string]TypeBinding{}
			parent: ctx
			level: ctx.level + 1
		}
	}
}

// bind adds a type binding to the context
pub fn (mut ctx TypeContext) bind(name string, type_expr TypeExpr, position Position) {
	ctx.bindings[name] = TypeBinding{
		name: name
		type_expr: type_expr
		position: position
	}
}

// lookup finds a type binding in the current context or parent contexts
pub fn (ctx &TypeContext) lookup(name string) ?TypeBinding {
	// Check current context first
	if binding := ctx.bindings[name] {
		return binding
	}

	// Check parent contexts
	if parent := ctx.parent {
		return parent.lookup(name)
	}

	return none
}

// lookup_local finds a type binding only in the current context
pub fn (ctx &TypeContext) lookup_local(name string) ?TypeBinding {
	return ctx.bindings[name] or { none }
}

// has_binding checks if a variable is bound in the current context
pub fn (ctx &TypeContext) has_binding(name string) bool {
	return name in ctx.bindings
}

// has_binding_recursive checks if a variable is bound in any context
pub fn (ctx &TypeContext) has_binding_recursive(name string) bool {
	if ctx.has_binding(name) {
		return true
	}

	if parent := ctx.parent {
		return parent.has_binding_recursive(name)
	}

	return false
}

// remove_binding removes a binding from the current context
pub fn (mut ctx TypeContext) remove_binding(name string) {
	ctx.bindings.delete(name)
}

// get_all_bindings returns all bindings in the current context
pub fn (ctx &TypeContext) get_all_bindings() []TypeBinding {
	return ctx.bindings.values()
}

// get_all_bindings_recursive returns all bindings in all contexts
pub fn (ctx &TypeContext) get_all_bindings_recursive() []TypeBinding {
	mut all_bindings := ctx.get_all_bindings()

	if parent := ctx.parent {
		all_bindings << parent.get_all_bindings_recursive()
	}

	return all_bindings
}

// str returns a string representation of the context
pub fn (ctx &TypeContext) str() string {
	mut result := 'Context (level ${ctx.level}):\n'

	for _, binding in ctx.bindings {
		result += '  ${binding.str()}\n'
	}

	if parent := ctx.parent {
		result += parent.str()
	}

	return result
}

// TypeScope represents a lexical scope for type checking
pub struct TypeScope {
pub mut:
	context &TypeContext
	level   int
}

// new_scope creates a new type scope
pub fn new_scope() &TypeScope {
	return &TypeScope{
		context: new_context()
		level: 0
	}
}

// enter_scope enters a new nested scope
pub fn (mut scope TypeScope) enter_scope() {
	scope.context = scope.context.new_child_context()
	scope.level++
}

// exit_scope exits the current scope and returns to the parent
pub fn (mut scope TypeScope) exit_scope() {
	if parent := scope.context.parent {
		scope.context = parent
		scope.level--
	}
}

// bind adds a type binding to the current scope
pub fn (mut scope TypeScope) bind(name string, type_expr TypeExpr, position Position) {
	scope.context.bind(name, type_expr, position)
}

// lookup finds a type binding in the current scope
pub fn (scope &TypeScope) lookup(name string) ?TypeBinding {
	return scope.context.lookup(name)
}

// has_binding checks if a variable is bound in the current scope
pub fn (scope &TypeScope) has_binding(name string) bool {
	return scope.context.has_binding_recursive(name)
}

// get_context returns the current type context
pub fn (scope &TypeScope) get_context() &TypeContext {
	return scope.context
}

// str returns a string representation of the scope
pub fn (scope &TypeScope) str() string {
	return 'Scope (level ${scope.level}):\n${scope.context.str()}'
}

// TypeEnvironment represents the global type environment
pub struct TypeEnvironment {
pub mut:
	builtin_types map[string]TypeExpr
	module_types  map[string]TypeExpr
	record_types  map[string]RecordType
}

// new_environment creates a new type environment
pub fn new_environment() &TypeEnvironment {
	mut env := &TypeEnvironment{
		builtin_types: map[string]TypeExpr{}
		module_types: map[string]TypeExpr{}
		record_types: map[string]RecordType{}
	}

	// Initialize built-in types
	env.builtin_types['integer'] = integer_type
	env.builtin_types['float'] = float_type
	env.builtin_types['string'] = string_type
	env.builtin_types['boolean'] = boolean_type
	env.builtin_types['atom'] = atom_type
	env.builtin_types['nil'] = nil_type
	env.builtin_types['any'] = any_type
	env.builtin_types['unknown'] = unknown_type

	return env
}

// register_builtin_type registers a built-in type
pub fn (mut env TypeEnvironment) register_builtin_type(name string, type_expr TypeExpr) {
	env.builtin_types[name] = type_expr
}

// register_module_type registers a module type
pub fn (mut env TypeEnvironment) register_module_type(name string, type_expr TypeExpr) {
	env.module_types[name] = type_expr
}

// register_record_type registers a record type
pub fn (mut env TypeEnvironment) register_record_type(name string, record_type RecordType) {
	env.record_types[name] = record_type
}

// lookup_type looks up a type by name
pub fn (env &TypeEnvironment) lookup_type(name string) ?TypeExpr {
	// Check built-in types first
	if type_expr := env.builtin_types[name] {
		return type_expr
	}

	// Check module types
	if type_expr := env.module_types[name] {
		return type_expr
	}

	// Check record types
	if record_type := env.record_types[name] {
		return record_type
	}

	return none
}

// lookup_record_type looks up a record type by name
pub fn (env &TypeEnvironment) lookup_record_type(name string) ?RecordType {
	return env.record_types[name] or { none }
}

// has_type checks if a type exists
pub fn (env &TypeEnvironment) has_type(name string) bool {
	return (name in env.builtin_types) ||
		   (name in env.module_types) ||
		   (name in env.record_types)
}

// str returns a string representation of the environment
pub fn (env &TypeEnvironment) str() string {
	mut result := 'Type Environment:\n'

	result += 'Built-in types:\n'
	for name, type_expr in env.builtin_types {
		result += '  ${name}: ${type_expr.str()}\n'
	}

	result += 'Module types:\n'
	for name, type_expr in env.module_types {
		result += '  ${name}: ${type_expr.str()}\n'
	}

	result += 'Record types:\n'
	for name, record_type in env.record_types {
		result += '  ${name}: ${record_type.str()}\n'
	}

	return result
}