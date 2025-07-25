module analysis

import ast

pub fn unify(t1 ast.Type, t2 ast.Type) bool {
	if t1.name != t2.name {
		return false
	}
	if t1.params.len != t2.params.len {
		return false
	}
	for i in 0 .. t1.params.len {
		if !unify(t1.params[i], t2.params[i]) {
			return false
		}
	}
	return true
}
