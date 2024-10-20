module ast

pub struct FunCaller {
	path string
	mod  string
	line u32
	pos  u32
}

pub struct FunctionLabel {
pub:
	name      string
	mod       string
	pos_start u32
	pos_end   u32
	filepath  string
	args      []string
	returns   string
	is_private bool
}

pub struct FunctionCallerLabel {
pub:
	name       string
	mod        string
	parent_fun string
	pos_start  u32
	pos_end    u32
	filepath   string
	args       []string
}

pub struct FunTable {
mut:
	labels                       []FunctionLabel
	caller_labels								 []FunctionCallerLabel
	idxs                         map[string]u32
	names                        []string
	rets                         [][]Literal
	args                         [][][]Literal
	type_reevaluate_instructions map[u32]map[u32][][]int
	callers                      map[u32]map[u32]FunCaller
}

pub fn FunTable.init() !&FunTable {
	mut ft := FunTable{}
	return &ft
}

pub fn (mut ft FunTable) set_labels(functions []FunctionLabel) {
	ft.labels = functions
}
pub fn (mut ft FunTable) set_caller_labels(functions_caller []FunctionCallerLabel) {
	ft.caller_labels = functions_caller
}
pub fn (mut ft FunTable) add_type_reevaluate_instructions(idx u32, args_idx u32, instructions [][]int) {
	ft.type_reevaluate_instructions[idx][args_idx] = instructions
}

pub fn find_args_idx(args0 []Literal, args1 [][]Literal, match_any bool) ?u32 {
	mut idx := -1
	if args1.len == 1 {
		if args1[0] == [] {
			return 0
		}
	}
	for a in args1 {
		idx++
		mut err := true
		for i in 0 .. args0.len {
			if args0[i] != a[i] {
				if match_any && a[i] == .l_any {
					continue
				} else {
					err = false
					break
				}
			} else {
				continue
			}
		}
		if err {
			return u32(idx)
		} else {
			return none
		}
	}
	return none
}

pub fn (mut ft FunTable) reevaluate_with_args(fun_idx u32, args_idx u32, args []Literal, new_args []Literal) Literal {
	if instructions := ft.type_reevaluate_instructions[fun_idx][args_idx] {
		mut ret := Literal.l_any
		mut i := -1
		mut args0 := map[int]Literal{}
		for i0, n in args {
			if n == .l_any {
				args0[i] = new_args[i0]
			}
		}
		for ins in instructions {
			for i0 in ins {
				if i0 < 0 {
					ret = args0[i0]
				}
			}
		}
		return ret
	}
	return .l_any
}

pub fn (mut ft FunTable) insert_caller(fun_idx u32, args_idx u32, mod string, path string, line u32, pos u32) {
	ft.callers[fun_idx][args_idx] = FunCaller{
		path: path
		mod:  mod
		pos:  pos
		line: line
	}
}

pub fn (mut ft FunTable) insert(name string, ret Literal, args []Literal, reevaluate [][]int) !(u32, u32) {
	arity := args.len
	fun_name := '${name}/${arity}'
	mut idx0 := -1
	if idx := ft.idxs[fun_name] {
		if _ := find_args_idx(args, ft.args[idx], false) {
			return error('the function `${fun_name}` with same signature already defined')
		}
		idx0 = idx
	} else {
		ft.names << fun_name
		idx0 = u32(ft.names.len - 1)
	}
	ft.idxs[fun_name] = u32(idx0)

	mut args_idx := u32(0)
	if mut a := ft.args[idx0] {
		mut r := ft.rets[idx0]
		args_idx = u32(a.len)
		a << args
		r << ret
		ft.args[idx0] = a
		ft.rets[idx0] = r
	} else {
		ft.args << [args]
		ft.rets << [ret]
	}
	if reevaluate.len > 0 {
		ft.type_reevaluate_instructions[u32(idx0)][args_idx] = reevaluate
	}
	return u32(idx0), args_idx
}

pub fn (ft FunTable) ref(s string) !u32 {
	if idx := ft.idxs[s] {
		return idx
	}
	return error('undefined ident ${s}')
}

pub fn (ft FunTable) lookup_by_name(s string) ?(u32, []Literal, [][]Literal) {
	if idx := ft.idxs[s] {
		return idx, ft.rets[idx], ft.args[idx]
	}
	return none
}

pub fn (ft FunTable) by_idx(idx u32) ?([]Literal, [][]Literal) {
	if ret := ft.rets[idx] {
		return ret, ft.args[idx]
	}
	return none
}
