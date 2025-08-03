module analysis

import ast

pub fn unify(t1 ast.Type, t2 ast.Type) bool {
	if t1.name == 'any' || t2.name == 'any' {
		return true
	}

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

pub fn unify_with_records(t1 ast.Type, t2 ast.Type, type_table &TypeTable) bool {
	// Se ambos são records, verificar se são o mesmo record
	if t1.name == t2.name {
		if _ := type_table.get_record_type(t1.name) {
			return true
		}
	}

	// Para outros tipos, usar unificação padrão
	return unify(t1, t2)
}

pub fn unify_with_functions(t1 ast.Type, t2 ast.Type, type_table &TypeTable) bool {
	if t1.name == 'function' && t2.name == 'function' {
		// For now, just check if they have the same number of parameters
		// This can be extended later for more sophisticated function type checking
		return t1.params.len == t2.params.len
	}

	return unify(t1, t2)
}
