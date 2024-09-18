module table

import ast

pub struct VarTable {
mut:
	idents []string
	kinds  []ast.Literal
	idxs   map[string]u32
}

pub fn VarTable.init() !&VarTable {
	mut vt := VarTable{}
	return &vt
}

pub fn make_ident(aix u32) u32 {
	val := (aix << 6) + ((0x0 << 4) | ((0x2 << 2) | 0x3))
	return u32(val)
}

pub fn (mut vt VarTable) insert(name string, lit ast.Literal) !u32 {
	if idx := vt.idxs[name] {
		return make_ident(idx)
	} else {
		vt.idents << name
		vt.kinds << lit
		idx := u32(vt.idents.len - 1)
		vt.idxs[name] = idx
		return make_ident(idx)
	}
}

// pub fn (mut vt VarTable) from(str string) !u32 {
// 	return vt.insert(str)
// }

pub fn (vt VarTable) ref(s string) !u32 {
	if idx := vt.idxs[s] {
		return u32(idx)
	}
	return error('undefined ident ${s}')
}

pub fn (vt VarTable) lookup_by_name(s string) ?(u32, ast.Literal) {
	if idx := vt.idxs[s] {
		return idx, vt.kinds[idx]
	}
	return none
}

pub fn (mut vt VarTable) lookup_by_idx(idx u32) ?(string, ast.Literal) {
	if str := vt.idents[idx] {
		return str, vt.kinds[idx]
	}
	return none
}

pub fn (vt VarTable) eq(idx u32, idx2 u32) bool {
	if idx < vt.idents.len && idx2 < vt.idents.len {
		ident1 := vt.idents[idx]
		ident2 := vt.idents[idx2]
		return ident1 == ident2
	}
	return false
}
