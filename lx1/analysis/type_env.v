module analysis

import ast

pub struct TypeEnv {
	scope_name string
mut:
	bindings map[string]ast.Type
	parent   int
}

pub fn new_type_env(scope_name string) TypeEnv {
	return TypeEnv{
		scope_name: scope_name
		bindings:   map[string]ast.Type{}
	}
}
