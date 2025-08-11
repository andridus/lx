module analysis

@[heap]
pub struct TypeEnv {
	scope_name string
mut:
	bindings map[string]TypeScheme
	parent   ?&TypeEnv
	children []&TypeEnv
}

pub fn new_type_env(scope_name string) TypeEnv {
	return TypeEnv{
		scope_name: scope_name
		bindings:   map[string]TypeScheme{}
		parent:     none
		children:   []
	}
}

pub fn (mut env TypeEnv) bind(name string, scheme TypeScheme) {
	env.bindings[name] = scheme
}

pub fn (env TypeEnv) lookup(name string) ?TypeScheme {
	// Check current scope first
	if scheme := env.bindings[name] {
		return scheme
	}

	// Check parent scope
	if parent := env.parent {
		return parent.lookup(name)
	}

	return none
}

pub fn (env TypeEnv) lookup_local(name string) ?TypeScheme {
	// Only check current scope
	return env.bindings[name] or { none }
}

pub fn (mut env TypeEnv) add_child(mut child TypeEnv) {
	child.parent = env
	env.children << &child
}

pub fn (env TypeEnv) get_bound_variables() []string {
	mut vars := []string{}

	// Get variables from current scope
	for name in env.bindings.keys() {
		vars << name
	}

	// Get variables from parent scope
	if parent := env.parent {
		parent_vars := parent.get_bound_variables()
		vars << parent_vars
	}

	return vars
}

pub fn (env TypeEnv) get_local_bound_variables() []string {
	mut vars := []string{}
	for name in env.bindings.keys() {
		vars << name
	}
	return vars
}

pub fn (env TypeEnv) is_bound(name string) bool {
	return name in env.bindings
}

pub fn (env TypeEnv) is_bound_in_scope(name string) bool {
	// Check if variable is bound in current scope (not inherited)
	return name in env.bindings
}

pub fn (env TypeEnv) get_scope_chain() []string {
	mut chain := []string{}

	// Add current scope
	chain << env.scope_name

	// Add parent scopes
	if parent := env.parent {
		parent_chain := parent.get_scope_chain()
		chain << parent_chain
	}

	return chain
}

pub fn (env TypeEnv) get_all_bindings() map[string]TypeScheme {
	mut all_bindings := env.bindings.clone()

	// Add parent bindings
	if parent := env.parent {
		parent_bindings := parent.get_all_bindings()
		for name, scheme in parent_bindings {
			if name !in all_bindings {
				all_bindings[name] = scheme
			}
		}
	}

	return all_bindings
}

pub fn (mut env TypeEnv) shadow(name string, scheme TypeScheme) {
	// Create a shadow binding (overrides parent binding)
	env.bindings[name] = scheme
}

pub fn (mut env TypeEnv) remove(name string) {
	env.bindings.delete(name)
}

pub fn (mut env TypeEnv) clear() {
	env.bindings.clear()
}

pub fn (env TypeEnv) size() int {
	return env.bindings.len
}

pub fn (env TypeEnv) is_empty() bool {
	return env.bindings.len == 0
}

pub fn (env TypeEnv) clone() TypeEnv {
	return TypeEnv{
		scope_name: env.scope_name
		bindings:   env.bindings.clone()
		parent:     env.parent
		children:   env.children.clone()
	}
}

pub fn (env TypeEnv) apply_substitution(substitution Substitution) TypeEnv {
	mut new_env := TypeEnv{
		scope_name: env.scope_name
		bindings:   map[string]TypeScheme{}
		parent:     env.parent
		children:   env.children.clone()
	}

	// Apply substitution to all bindings
	for name, scheme in env.bindings {
		new_env.bindings[name] = substitute_in_type_scheme(scheme, substitution)
	}

	return new_env
}

pub fn (env TypeEnv) str() string {
	mut result := 'TypeEnv(${env.scope_name}): {'

	mut bindings_str := []string{}
	for name, scheme in env.bindings {
		bindings_str << '${name}: ${scheme.str()}'
	}

	result += bindings_str.join(', ')
	result += '}'

	if parent := env.parent {
		result += ' -> ${parent.str()}'
	}

	return result
}

pub fn (scheme TypeScheme) str() string {
	if scheme.quantified_vars.len == 0 {
		return scheme.body.str()
	}

	vars_str := scheme.quantified_vars.map(it.name).join(', ')
	return 'forall ${vars_str}. ${scheme.body.str()}'
}
