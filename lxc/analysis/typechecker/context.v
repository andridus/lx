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
	bindings          map[string]TypeBinding
	type_aliases      map[string]TypeExpr
	record_types      map[string]string       // Maps record access expressions to their types
	function_contexts map[string]&TypeContext // Maps function IDs to their type contexts
	parent            ?&TypeContext
	level             int // Scope level for shadowing
}

// new_context creates a new type context
@[heap]
pub fn new_context() &TypeContext {
	return &TypeContext{
		bindings:          map[string]TypeBinding{}
		type_aliases:      map[string]TypeExpr{}
		record_types:      map[string]string{}
		function_contexts: map[string]&TypeContext{}
		parent:            none
		level:             0
	}
}

// new_child_context creates a new child context
pub fn (ctx &TypeContext) new_child_context() &TypeContext {
	return unsafe {
		&TypeContext{
			bindings:          map[string]TypeBinding{}
			type_aliases:      map[string]TypeExpr{}
			function_contexts: map[string]&TypeContext{}
			parent:            ctx
			level:             ctx.level + 1
		}
	}
}

// bind adds a type binding to the context
pub fn (mut ctx TypeContext) bind(name string, type_expr TypeExpr, position Position) {
	ctx.bindings[name] = TypeBinding{
		name:      name
		type_expr: type_expr
		position:  position
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

pub fn (mut ctx TypeContext) store_record_type(expr ast.RecordAccessExpr, record_type string) {
	key := '${expr.record.str()}.${expr.field}'
	ctx.record_types[key] = record_type
}

pub fn (mut ctx TypeContext) store_record_type_for_expr(expr ast.RecordLiteralExpr, record_type string) {
	key := '${expr.name}'
	ctx.record_types[key] = record_type
}

pub fn (mut ctx TypeContext) store_record_type_for_update_expr(expr ast.RecordUpdateExpr, record_type string) {
	key := '${expr.record_name}'
	ctx.record_types[key] = record_type
}

// store_record_field_type stores the type of a specific field in a record
pub fn (mut ctx TypeContext) store_record_field_type(record_name string, field_name string, field_type string) {
	key := '${record_name}.${field_name}'
	ctx.record_types[key] = field_type
}

// get_record_field_type gets the type of a specific field in a record
pub fn (ctx &TypeContext) get_record_field_type(record_name string, field_name string) ?string {
	key := '${record_name}.${field_name}'
	if field_type := ctx.record_types[key] {
		return field_type
	}

	if parent := ctx.parent {
		return parent.get_record_field_type(record_name, field_name)
	}

	return none
}

// get_record_type_from_expr gets the record type from an expression
pub fn (ctx &TypeContext) get_record_type_from_expr(expr ast.Expr) ?string {
	if expr is ast.VariableExpr {
		var_expr := expr as ast.VariableExpr
		if record_type := ctx.record_types[var_expr.name] {
			return record_type
		}
	}

	if parent := ctx.parent {
		return parent.get_record_type_from_expr(expr)
	}

	return none
}

pub fn (ctx &TypeContext) get_record_type(expr ast.RecordAccessExpr) ?string {
	if expr.record is ast.VariableExpr {
		var_expr := expr.record as ast.VariableExpr
		if record_type := ctx.record_types[var_expr.name] {
			return record_type
		}

		if parent := ctx.parent {
			return parent.get_record_type(expr)
		}
	}

	// Try to get record type from record literal expressions
	if expr.record is ast.RecordLiteralExpr {
		record_expr := expr.record as ast.RecordLiteralExpr
		if record_type := ctx.record_types[record_expr.name] {
			return record_type
		}
	}

	key := '${expr.record.str()}.${expr.field}'

	if record_type := ctx.record_types[key] {
		return record_type
	}

	if parent := ctx.parent {
		return parent.get_record_type(expr)
	}

	return none
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
		level:   0
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
		module_types:  map[string]TypeExpr{}
		record_types:  map[string]RecordType{}
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
	return name in env.builtin_types || name in env.module_types || name in env.record_types
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

// new_type_context creates a new type context for compilation
pub fn new_type_context() &TypeContext {
	return new_context()
}

// register_type_alias registers a type alias in the context
pub fn (mut ctx TypeContext) register_type_alias(name string, type_expr TypeExpr) {
	ctx.type_aliases[name] = type_expr
}

// lookup_type_alias finds a type alias in the current context or parent contexts
pub fn (ctx &TypeContext) lookup_type_alias(name string) ?TypeExpr {
	// Check current context first
	if type_expr := ctx.type_aliases[name] {
		return type_expr
	}

	// Check parent contexts
	if parent := ctx.parent {
		return parent.lookup_type_alias(name)
	}

	return none
}

// has_type_alias checks if a type alias exists in the current context
pub fn (ctx &TypeContext) has_type_alias(name string) bool {
	return name in ctx.type_aliases
}

// has_type_alias_recursive checks if a type alias exists in any context
pub fn (ctx &TypeContext) has_type_alias_recursive(name string) bool {
	if ctx.has_type_alias(name) {
		return true
	}

	if parent := ctx.parent {
		return parent.has_type_alias_recursive(name)
	}

	return false
}

// get_all_type_aliases returns all type aliases in the current context
pub fn (ctx &TypeContext) get_all_type_aliases() map[string]TypeExpr {
	return ctx.type_aliases.clone()
}

// get_all_type_aliases_recursive returns all type aliases in all contexts
pub fn (ctx &TypeContext) get_all_type_aliases_recursive() map[string]TypeExpr {
	mut all_aliases := ctx.get_all_type_aliases()

	if parent := ctx.parent {
		parent_aliases := parent.get_all_type_aliases_recursive()
		for name, type_expr in parent_aliases {
			// Child context aliases take precedence
			if name !in all_aliases {
				all_aliases[name] = type_expr
			}
		}
	}

	return all_aliases
}

// store_expression_type_for_var armazena o tipo de qualquer expressão para uma variável
pub fn (mut ctx TypeContext) store_expression_type_for_var(var_name string, type_expr TypeExpr) {
	ctx.record_types[var_name] = type_expr.str()
}

// store_record_type_for_var associa o tipo do record ao nome da variável
pub fn (mut ctx TypeContext) store_record_type_for_var(var_name string, record_type string) {
	ctx.record_types[var_name] = record_type
}

// store_function_context stores the type context for a specific function
pub fn (mut ctx TypeContext) store_function_context(function_id string, function_context &TypeContext) {
	ctx.function_contexts[function_id] = unsafe { function_context }
}

// get_function_context retrieves the type context for a specific function
pub fn (ctx &TypeContext) get_function_context(function_id string) ?&TypeContext {
	if function_context := ctx.function_contexts[function_id] {
		return function_context
	}

	// Check parent contexts
	if parent := ctx.parent {
		return parent.get_function_context(function_id)
	}

	return none
}

// store_function_var_type armazena o tipo de uma variável de função específica
pub fn (mut ctx TypeContext) store_function_var_type(function_id string, var_name string, type_str string) {
	key := '${function_id}:${var_name}'
	ctx.record_types[key] = type_str
}

// get_function_var_type recupera o tipo de uma variável de função específica
pub fn (ctx &TypeContext) get_function_var_type(function_id string, var_name string) ?string {
	key := '${function_id}:${var_name}'

	// Check current context first
	if record_type := ctx.record_types[key] {
		return record_type
	}

	// Check parent contexts
	if parent := ctx.parent {
		return parent.get_function_var_type(function_id, var_name)
	}

	return none
}
