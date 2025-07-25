module analysis

import ast

@[heap]
pub struct TypeEnv {
mut:
	bindings map[string]ast.Type
	parent   &TypeEnv = unsafe { nil }
}

pub fn new_type_env() TypeEnv {
	return TypeEnv{
		bindings: map[string]ast.Type{}
		parent:   unsafe { nil }
	}
}

pub fn (env &TypeEnv) lookup(name string) ?ast.Type {
	if name in env.bindings {
		return env.bindings[name]
	}
	if env.parent != unsafe { nil } {
		return env.parent.lookup(name)
	}
	return none
}

pub fn (mut env TypeEnv) bind(name string, typ ast.Type) {
	env.bindings[name] = typ
}

pub fn (env &TypeEnv) extend() &TypeEnv {
	return &TypeEnv{
		bindings: map[string]ast.Type{}
		parent:   env
	}
}
