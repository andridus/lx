module analysis

// HMTypeVar represents a type variable in the HM system
pub struct HMTypeVar {
pub:
	id   int
	name string
}

// TypeScheme represents a type scheme for polymorphism (∀ variables. type)
pub struct TypeScheme {
pub:
	vars      []HMTypeVar
	type_info TypeInfo
}

// TypeEnv represents a type environment for variable bindings
pub struct TypeEnv {
pub mut:
	bindings map[string]TypeScheme
	parent   ?&TypeEnv
}

// new_type_env creates a new empty type environment
pub fn new_type_env() TypeEnv {
	return TypeEnv{
		bindings: map[string]TypeScheme{}
		parent:   none
	}
}

// extend creates a new type environment with a parent
pub fn (env TypeEnv) extend() TypeEnv {
	return TypeEnv{
		bindings: map[string]TypeScheme{}
		parent:   &env
	}
}

// lookup searches for a variable in the environment
pub fn (env TypeEnv) lookup(name string) ?TypeScheme {
	if scheme := env.bindings[name] {
		return scheme
	}
	if parent := env.parent {
		return parent.lookup(name)
	}
	return none
}

// bind adds a variable binding to the environment
pub fn (mut env TypeEnv) bind(name string, scheme TypeScheme) {
	env.bindings[name] = scheme
}

// str returns a string representation of HMTypeVar
pub fn (tv HMTypeVar) str() string {
	return 'HMTypeVar(${tv.id}, ${tv.name})'
}

// str returns a string representation of TypeScheme
pub fn (ts TypeScheme) str() string {
	if ts.vars.len == 0 {
		return ts.type_info.str()
	}
	var_names := ts.vars.map(it.name).join(', ')
	return '∀${var_names}. ${ts.type_info.str()}'
}

// str returns a string representation of TypeEnv
pub fn (env TypeEnv) str() string {
	mut bindings := []string{}
	for name, scheme in env.bindings {
		bindings << '${name}: ${scheme.str()}'
	}
	return '{${bindings.join(', ')}}'
}

// print_debug prints debug information for the type environment
pub fn (env TypeEnv) print_debug() {
	println('Type Environment:')
	if env.bindings.len == 0 {
		println('  (empty)')
	} else {
		for name, scheme in env.bindings {
			println('  ${name}: ${scheme.str()}')
		}
	}

	if parent := env.parent {
		println('  Parent environment:')
		parent.print_debug()
	}
}

// monotype creates a monomorphic type scheme (no type variables)
pub fn monotype(type_info TypeInfo) TypeScheme {
	return TypeScheme{
		vars:      []
		type_info: type_info
	}
}

// polytype creates a polymorphic type scheme with type variables
pub fn polytype(vars []HMTypeVar, type_info TypeInfo) TypeScheme {
	return TypeScheme{
		vars:      vars
		type_info: type_info
	}
}
