module table

import ast

@[heap]
pub struct ContextVarTable {
mut:
	idents []string
	kinds  []ast.Literal
	idxs   map[string]u32
}

pub struct VarTable {
mut:
	contexts map[u32]&ContextVarTable
}

pub fn VarTable.init() !&VarTable {
	mut vt := VarTable{}
	return &vt
}

pub fn make_ident(aix u32) u32 {
	val := (aix << 6) + ((0x0 << 4) | ((0x2 << 2) | 0x3))
	return u32(val)
}

fn (mut vt VarTable) get_or_create_context(context u32) &ContextVarTable {
	return vt.get_context(context) or {
		ctx := &ContextVarTable{}
		vt.contexts[context] = ctx
		ctx
	}
}

fn (vt VarTable) get_context(context u32) !&ContextVarTable {
	if ctx := vt.contexts[context] {
		return ctx
	}
	return error('context not found')
}

pub fn (mut vt VarTable) insert(context u32, name string, lit ast.Literal) !u32 {
	mut ctx := vt.get_or_create_context(context)
	if idx := ctx.idxs[name] {
		return make_ident(idx)
	} else {
		ctx.idents << name
		ctx.kinds << lit
		idx := u32(ctx.idents.len - 1)
		ctx.idxs[name] = idx
		return make_ident(idx)
	}
}

pub fn (vt VarTable) ref(context u32, s string) !u32 {
	ctx := vt.get_context(context)!
	if idx := ctx.idxs[s] {
		return u32(idx)
	}
	return error('undefined ident ${s}')
}

pub fn (vt VarTable) lookup_by_name(context u32, s string) ?(u32, ast.Literal) {
	ctx := vt.get_context(context) or { return none }
	if idx := ctx.idxs[s] {
		return idx, ctx.kinds[idx]
	}
	return none
}

pub fn (mut vt VarTable) lookup_by_idx(context u32, idx u32) ?(string, ast.Literal) {
	ctx := vt.get_context(context) or { return none }
	if str := ctx.idents[idx] {
		return str, ctx.kinds[idx]
	}
	return none
}
