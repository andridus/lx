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
