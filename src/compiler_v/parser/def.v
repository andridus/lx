// Copyright (c) 2023 Helder de Sousa. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module parser

import compiler_v.ast
import compiler_v.table
import compiler_v.types
import compiler_v.token
import compiler_v.docs
import compiler_v.color

pub fn (mut p Parser) call_expr() ?ast.Node {
	if p.tok.kind == .modl {
		return p.call_from_module_node(.modl)!
	}
	return none
}

pub fn (mut p Parser) call_from_module_node(kind token.Kind) !ast.Node {
	mut meta := p.meta()
	mut is_external := false
	mut is_ffi := false
	mut is_local := false
	// mut is_unknown := false
	// mut tok := p.tok
	mut arg_nodes := []ast.Node{}
	mut arity_num := 0
	mut arity_args := []string{}
	mut arity_name := ''
	mut fun_name := token.Token{}
	mut module_ref := [p.tok]
	mut return_ti := types.void_ti
	// Initial lexer position for stmt
	p.error_pos_in = p.tok.pos - p.tok.lit.len

	if kind == .ident {
		if p.peek_tok.kind == .dot {
			// should be a var, check
			p.error_pos_out = p.tok.pos
			p.log_d('ERROR', '`${p.tok.lit}` is not a var', '', '', p.tok.lit)
			exit(1)
		} else {
			fun_name = p.tok
			is_local = true
		}
	}
	// Gets the name of module or local function
	p.check(kind)
	mut icount := 0
	for p.tok.kind == .dot {
		p.check(.dot)
		if p.tok.kind == .ident {
			if icount > 0 {
				module_ref << fun_name
				icount = 0
			}
			fun_name = p.tok
			icount++
		} else if p.tok.kind == .modl {
			module_ref << p.tok
		} else {
			p.error_pos_inline = p.lexer.pos_inline
			p.error('The token `${p.tok.str()}` is not a Module. \n Module starts with a capital letter.')
			exit(1)
		}
		p.next_token()
	}
	mut module_name := module_name0(module_ref)
	if module_name.starts_with('FFI.') {
		is_ffi = true
	}
	// Check if is a local function
	if is_local {
		module_name = p.current_module
	}
	// Check if is aliased
	aliased_name := p.program.modules[p.current_module].aliases[module_name]
	if aliased_name.len > 0 {
		module_name = aliased_name
	}
	// get module path
	// module_path := module_name.to_lower()
	// If module placed with anything
	if fun_name.kind == .ignore {
		p.warn('Module ${module_name} is orphan')
	}
	/// Check the args of function
	p.check(.lpar)

	if f := p.program.table.find_fn(fun_name.lit, module_name) {
		for p.tok.kind != .rpar {
			e := p.expr_node(0)
			arity_num++
			arity_args << e.meta.ti.name
			arg_nodes << e
			if p.tok.kind != .rpar {
				p.check(.comma)
			}
		}
		arity_name = '${arity_num}_${arity_args.join('_')}'
		if arity_name == '0_' {
			arity_name = '0'
		}
		finded := f.idx_arity_by_args[arity_name]
		mut valid_arity := false

		if finded > 0 {
			a := f.arities[finded]
			if a.is_valid {
				valid_arity = true
			}
			return_ti = a.return_ti
		}

		if valid_arity == false {
			mut args_ := []string{}
			for _, a in f.arities {
				if a.is_valid {
					arg_len := a.args.len
					if arg_len > 0 {
						mut args0 := []string{}
						for a1 in a.args {
							args0 << a1.ti.str()
						}

						args_ << color.fg(color.white, 0, '${fun_name.lit}(${args0.join(', ')})')
					} else {
						args_ << color.fg(color.white, 0, '${fun_name.lit}()')
					}
				}
			}
			p.error_pos_out = p.tok.pos
			fun_name0 := color.fg(color.white, 0, '${fun_name.lit}(${arity_args.join(', ')})')
			p.log_d('ERROR', 'The function ${fun_name0} ${color.fg(color.red, 0, 'not exists, check one of')} ${args_.join(' | ')}',
				docs.function_args_desc, docs.function_args_url, fun_name.lit)
		}
		if p.tok.kind == .comma {
			p.error('too many arguments in call to `${fun_name}`')
		}
	} else {
		// if is_c_module == false && is_v_module == false {
		// is_unknown = true
		p.error_pos_out = p.tok.pos
		if is_local {
			// // should be a local function, check
			p.log_d('ERROR', 'The `${fun_name}` is undefined local function', docs.local_function_desc,
				docs.local_function_url, p.tok.lit)
		} else {
			if is_external {
				// call from external module (perhaps alias?)
				p.log_d('WARN', 'unknown function `${fun_name.lit}` from module `${module_name}`',
					'', '', fun_name.lit)
			} else if is_ffi {
			} else {
				p.log_d('WARN', 'unknown function `${fun_name.lit}`', '', '', fun_name.lit)
			}
		}
		for p.tok.kind != .rpar {
			e := p.expr_node(0)
			arity_num++
			arity_args << e.meta.ti.name
			arg_nodes << e
			if p.tok.kind != .rpar {
				p.check(.comma)
			}
		}
	}
	p.check(.rpar)

	if p.tok.kind == .typedef {
		p.check(.typedef)
		return_ti = p.parse_ti()
	}

	meta.put_ti(return_ti)
	fun_node := p.node(meta, '.', [
		p.node(meta, '__aliases__', [p.node_atomic(module_name)]),
		p.node_atomic(fun_name.lit),
	])
	mut arities := []string{}
	for an in arg_nodes {
		arities << an.meta.ti.str()
	}
	return p.node_function_caller(meta, p.node_left(fun_node), arg_nodes, ast.FunctionCaller{
		name: fun_name.lit
		return_ti: return_ti
		module_name: module_name
		args: arg_nodes
		arity: arities
	})
}

fn module_name0(tokens []token.Token) string {
	mut name := []string{}
	for t in tokens {
		name << t.lit
	}
	return name.join('.')
}

fn (mut p Parser) def_decl() ast.Node {
	mut meta := p.meta()
	pos_in := p.tok.pos
	mut pos_out := p.tok.pos
	mut ignore_var := false
	is_private := p.tok.kind == .key_defp

	p.program.table.clear_vars()
	if is_private {
		p.check(.key_defp)
	} else {
		p.check(.key_def)
	}
	name := p.check_name()
	p.check(.lpar)

	// GET Args
	mut args := []table.Var{}
	mut ast_args := []ast.Node{}

	for p.tok.kind != .rpar {
		mut is_nil := false
		is_nil = p.tok.kind == .key_nil
		mut arg_name0 := 'anonymous'
		if p.tok.kind == .underscore {
			ignore_var = true
			p.next_token()
			if p.tok.kind == .ident {
				arg_name0 = '_' + p.check_name()
			}
		} else {
			arg_name0 = p.check_name()
		}
		mut arg_names := [arg_name0]
		for p.tok.kind == .comma {
			p.check(.comma)
			arg_names << p.check_name()
		}
		// parse type of ARG
		mut ti := types.void_ti
		if is_nil {
			ti = types.nil_ti
		} else if p.tok.kind == .typedef {
			p.check(.typedef)

			ti = p.parse_ti()
		}
		for arg_name in arg_names {
			typ0 := p.program.table.find_type(ti.str()) or { types.type_from_ti(ti) }
			mut meta0 := p.meta()
			arg := table.Var{
				name: arg_name
				ti: ti
				type_: typ0
				is_ignored: ignore_var
				expr: p.node_atom(mut meta0, arg_name)
			}
			args << arg
			p.program.table.register_var(arg)
			// ast_args << ast.Arg{
			// 	ti: ti
			// 	name: arg_name
			// }
			// if ti.kind == .variadic && p.tok.kind == .comma {
			// 	p.error('cannot use ...(variadic) with non-final parameter $arg_name')
			// }
		}
		if p.tok.kind != .rpar {
			p.check(.comma)
		}
	}

	p.check(.rpar)
	// Return type
	mut ti := types.void_ti
	mut from_type := false
	if p.tok.kind == .typedef {
		p.check(.typedef)
		ti = p.parse_ti()
		p.return_ti = ti
		from_type = true
	}
	node_block := p.parse_block()
	// Try get type from body inference
	if from_type == false {
		if node_block.kind is ast.List {
			n0 := node_block.nodes[0]
			if n0.kind is ast.Tuple {
				value := n0.nodes[0].left.str()
				if value == ':do' {
					ti = n0.nodes[1].meta.ti
				}
			}
		}
	}

	mut final_args := []table.Var{}
	mut args_overfn := '${args.len}'
	for a in args {
		var0 := p.program.table.find_var(a.name, p.context) or { a }
		mut meta0 := p.meta()

		// NOTE: check this. it`s necessary check if var (on table) and arg has equals ti, if a.ti != .void_ ?
		if a.ti.kind == .void_ {
			meta0.put_ti(var0.ti)
		} else {
			meta0.put_ti(a.ti)
		}
		var := p.node_var(meta0, a.name, [])
		final_args << table.Var{
			...a
			ti: var0.ti
		}
		ast_args << var
		args_overfn += '_${var.meta.ti.name}'
	}
	pos_out = p.tok.pos
	p.program.table.register_or_update_fn(args_overfn, final_args, ti, table.Fn{
		name: name
		is_external: false
		is_valid: true
		module_path: p.module_path
		module_name: p.module_name
		def_pos_in: pos_in
		def_pos_out: pos_out
	})
	meta.put_ti(types.new_sum_ti([ti.kind]))
	return p.node_function(meta, [
		p.node(meta, name, ast_args),
		node_block,
	], ast.Function{
		name: name
		module_name: p.module_name
		arity: args_overfn
		args: ast_args
		is_main: name == 'main'
		return_ti: meta.ti
		is_private: is_private
	})
}
