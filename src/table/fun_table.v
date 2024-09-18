module table

import ast

pub struct FunTable {
mut:
	idxs  map[string]u32
	names []string
	rets  []ast.Literal
	args  [][]ast.Literal
}

pub fn FunTable.init() !&FunTable {
	mut ft := FunTable{}
	return &ft
}

pub fn (mut ft FunTable) insert(name string, ret ast.Literal, args []ast.Literal) !u32 {
	arity := args.len
	fun_name := '${name}/${arity}'
	if _idx := ft.idxs[name] {
		return error('the function `${fun_name}` already defined')
	} else {
		ft.names << name
		idx := u32(ft.names.len - 1)
		ft.idxs[name] = idx
		ft.rets << ret
		ft.args << args
		return idx
	}
}

pub fn (ft FunTable) ref(s string) !u32 {
	if idx := ft.idxs[s] {
		return idx
	}
	return error('undefined ident ${s}')
}

pub fn (ft FunTable) lookup_by_name(s string) ?(ast.Literal, []ast.Literal) {
	if idx := ft.idxs[s] {
		return ft.rets[idx], ft.args[idx]
	}
	return none
}

pub fn (mut ft FunTable) string_by_idx(idx u32) ?(ast.Literal, []ast.Literal) {
	if ret := ft.rets[idx] {
		return ret, ft.args[idx]
	}
	return none
}
