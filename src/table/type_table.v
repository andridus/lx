module table

import ast

pub struct TypeTable {
mut:
	idxs  map[string]u32
	types []ast.Literal
}

pub fn TypeTable.init() !&TypeTable {
	mut vt := TypeTable{}
	vt.insert('nil', .l_nil)!
	vt.insert('integer', .l_integer)!
	vt.insert('float', .l_float)!
	vt.insert('boolean', .l_boolean)!
	vt.insert('atom', .l_atom)!
	vt.insert('string', .l_string)!
	vt.insert('list', .l_list)!
	vt.insert('tuple', .l_tuple)!
	vt.insert('char', .l_char)!
	vt.insert('function', .l_function)!
	vt.insert('any', .l_any)!
	return &vt
}

pub fn (mut vt TypeTable) insert(name string, lit ast.Literal) !u32 {
	if idx := vt.idxs[name] {
		return idx
	} else {
		vt.types << lit
		idx := u32(vt.types.len - 1)
		vt.idxs[name] = idx
		return idx
	}
}

pub fn (vt TypeTable) lookup_by_name(s string) ?(u32, ast.Literal) {
	if idx := vt.idxs[s] {
		return idx, vt.types[idx]
	}
	return none
}
