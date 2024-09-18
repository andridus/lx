module table

import ast

pub struct FunTable {
mut:
	idents []string
	nodes  map[int]ast.Node
	idxs   map[string]u32
}

pub fn FunTable.init() !&FunTable {
	mut ft := FunTable{}
	return &ft
}

pub fn make_fun_ident(aix u32) u32 {
	val := (aix << 6) + ((0x0 << 4) | ((0x2 << 2) | 0x3))
	return u32(val)
}

pub fn (mut ft FunTable) insert(name string, node ast.Node) !u32 {
	if idx := ft.idxs[name] {
		return make_ident(idx)
	} else {
		ft.idents << name
		idx := u32(ft.idents.len - 1)
		ft.idxs[name] = idx
		ft.nodes[idx] = node
		return make_ident(idx)
	}
}

pub fn (mut ft FunTable) from(str string, node ast.Node) !u32 {
	return ft.insert(str, node)
}

pub fn (ft FunTable) ref(s string) !u32 {
	if idx := ft.idxs[s] {
		return u32(idx)
	}
	return error('undefined ident ${s}')
}

pub fn (ft FunTable) lookup_by_name(s string) ?ast.Node {
	if idx := ft.idxs[s] {
		return ft.nodes[idx]
	}
	return none
}

pub fn (mut ft FunTable) string_by_idx(idx u32) ?ast.Node {
	if node := ft.nodes[idx] {
		return node
	}
	return none
}

pub fn (ft FunTable) eq(idx u32, idx2 u32) bool {
	if idx < ft.idents.len && idx2 < ft.idents.len {
		ident1 := ft.idents[idx]
		ident2 := ft.idents[idx2]
		return ident1 == ident2
	}
	return false
}
