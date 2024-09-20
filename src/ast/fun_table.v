module ast

pub struct FunTable {
mut:
	idxs  map[string]u32
	names []string
	rets  [][]Literal
	args  [][][]Literal
}

pub fn FunTable.init() !&FunTable {
	mut ft := FunTable{}
	return &ft
}

pub fn (mut ft FunTable) insert(name string, ret Literal, args []Literal) !u32 {
	arity := args.len
	fun_name := '${name}/${arity}'
	mut idx0 := -1
	if idx := ft.idxs[fun_name] {
		for a in ft.args[idx] {
			mut err := true
			for i in 0 .. args.len {
				if args[i] != a[i] {
					err = false
					break
				} else {
					continue
				}
			}
			if err {
				return error('the function `${fun_name}` with same signature already defined')
			}
		}
		idx0 = idx
	} else {
		ft.names << fun_name
		idx0 = u32(ft.names.len - 1)
	}
	ft.idxs[fun_name] = u32(idx0)

	if mut a := ft.args[idx0] {
		mut r := ft.rets[idx0]
		a << args
		r << ret
		ft.args[idx0] = a
		ft.rets[idx0] = r
	} else {
		ft.args << [args]
		ft.rets << [ret]
	}
	return u32(idx0)
}

pub fn (ft FunTable) ref(s string) !u32 {
	if idx := ft.idxs[s] {
		return idx
	}
	return error('undefined ident ${s}')
}

pub fn (ft FunTable) lookup_by_name(s string) ?([]Literal, [][]Literal) {
	if idx := ft.idxs[s] {
		return ft.rets[idx], ft.args[idx]
	}
	return none
}

pub fn (ft FunTable) by_idx(idx u32) ?([]Literal, [][]Literal) {
	if ret := ft.rets[idx] {
		return ret, ft.args[idx]
	}
	return none
}
