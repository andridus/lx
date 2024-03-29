// Copyright (c) 2023 Helder de Sousa. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module parser

import compiler_v.ast
import compiler_v.table
import compiler_v.types
// fn (mut p Parser) expr_stmt() ast.Node {
// 	node := p.expr(0)
// 	return node
// }

pub fn (mut p Parser) parse_block() ast.Node {
	mut meta := p.meta()

	if p.inside_ifcase > 0 && p.tok.kind != .key_do {
	} else {
		p.check(.key_do)
	}

	mut stmts := []ast.Node{}

	if p.tok.kind != .key_end {
		for {
			mut stmt := p.stmt()
			stmt.mark_with_is_main_expr()

			// if is last statement
			if p.tok.kind in [.eof, .key_end] {
				stmt.mark_with_last_expr()
				meta.put_ti(stmt.meta.ti)
			}
			p.check_node(mut stmt)
			stmts << stmt

			// p.warn('after stmt(): tok=$p.tok.str()')
			if p.tok.kind in [.eof, .key_end] {
				break
			}
			if p.inside_ifcase > 0 && p.tok.kind == .key_else {
				break
			}
		}
	}

	if p.tok.kind == .key_end {
		p.check(.key_end)
	}

	if stmts.len == 1 {
		return p.node_list(meta, [p.node_tuple(meta, [p.node_atomic('do'), stmts[0]])])
	} else {
		return p.node_list(p.meta(), [
			p.node_tuple(p.meta(), [
				p.node_atomic('do'),
				p.node(meta, '__block__', stmts),
			]),
		])
	}
}

fn (mut p Parser) parse_nil_literal() ast.Node {
	node := p.node_nil()
	p.next_token()
	return node
}

fn (mut p Parser) parse_number_literal() ast.Node {
	mut meta := p.meta()
	mut node := p.node_default()
	if p.tok.kind == .float {
		node = unsafe { p.node_float(mut meta, p.tok.value.fval) }
	} else {
		node = unsafe { p.node_integer(mut meta, p.tok.value.ival) }
	}
	p.next_token()
	return node
}

fn (mut p Parser) infix_expr(left ast.Node) ast.Node {
	mut meta := p.meta()
	op := p.tok.lit
	op_precedence := ast.precedence(op)
	p.next_token()
	next_precedence := ast.precedence(p.tok.lit)
	right := p.expr_node(next_precedence)
	node := p.parse_operations(mut meta, left, op, op_precedence, right, p.inside_parens > 0)
	return node
}

fn (mut p Parser) not_expr() ast.Node {
	mut meta := p.meta()
	p.check(.bang)
	node := p.expr_node(0)
	meta.put_ti(types.bool_ti)
	return p.node_bang(meta, node)
}

fn (mut p Parser) underscore_expr() ast.Node {
	meta := p.meta()
	p.check(.underscore)
	// ti := types.void_ti
	mut name := ''
	if p.tok.kind == .ident {
		name = p.tok.lit
		p.check(.ident)
	}

	return p.node_underscore(meta, name)
}

fn (mut p Parser) parse_boolean() ast.Node {
	mut meta := p.meta()
	meta.put_ti(types.bool_ti)
	mut node := p.node_default()
	if p.tok.kind == .key_true {
		node = p.node_boolean(meta, 'true')
	} else if p.tok.kind == .key_false {
		node = p.node_boolean(meta, 'false')
	}
	p.next_token()
	return node
}

fn (mut p Parser) atom_expr() ast.Node {
	mut meta := p.meta()
	if p.peek_tok.kind == .dot {
		node0 := p.call_from_module_node(.atom) or {
			println(err.msg())
			exit(1)
		}
		return node0
	} else {
		node := p.node_atom(mut meta, p.tok.lit)
		p.program.table.find_or_new_atom(p.tok.lit)
		p.check(.atom)
		return node
	}
}

// fn (mut p Parser) if_expr() (ast.Expr, types.TypeIdent) {
// 	mut else_stmts := ast.Node{}
// 	p.check(.key_if)
// 	p.inside_ifcase++
// 	cond, ti := p.expr(0)
// 	// stmts := p.parse_block()
// 	if p.tok.kind == .key_else {
// 		p.check(.key_else)
// 		else_stmts = p.parse_block()
// 	}
// 	p.inside_ifcase--
// 	node := ast.IfExpr{
// 		cond: cond
// 		// stmts: stmts
// 		// else_stmts: else_stmts
// 		ti: ti
// 		is_used: p.in_var_expr
// 	}
// 	return node, node.ti
// }

fn (mut p Parser) string_expr() ast.Node {
	mut meta := p.meta()

	mut node := p.node_string(mut meta, p.tok.lit)
	if p.peek_tok.kind != .hash {
		if p.lexer.inside_interpolation {
			p.next_token()
			// the first interpolate expression
			mut node0 := p.expr_node(0)
			node0 = p.node_caller_to_string(meta, node0)
			// the rest of string with maybe other interpolations
			node1 := p.expr_node(0)
			node2 := p.node_string_concat(mut meta, node0, node1)
			node = p.node_string_concat(mut meta, node, node2)
		} else {
			p.next_token()
		}

		return node
	}

	for p.tok.kind == .str {
		p.next_token()
		if p.tok.kind != .hash {
			continue
		}
		p.check(.hash)
		p.expr_node(0)
	}
	return node
}

fn (mut p Parser) string_concat_expr() ast.Node {
	mut meta := p.meta()
	mut left := p.node_default()
	mut right := p.node_default()
	if p.tok.kind == .str {
		left = p.string_expr()
	} else if p.tok.kind == .ident {
		left = p.ident_expr()
	}
	if left.meta.ti.kind != .string_ {
		println('${left} is not a string type')
		exit(1)
	}
	p.check(.string_concat)

	if p.peek_tok.kind == .string_concat {
		right = p.string_concat_expr()
	} else {
		right = p.expr_node(0)
		if right.meta.ti.kind != .string_ {
			println('${right} is not a string type')
			exit(1)
		}
	}

	meta.put_ti(left.meta.ti)
	return p.node_string_concat(mut meta, left, right)
}

// fn (mut p Parser) charlist_expr() (ast.Expr, types.TypeIdent) {
// 	mut node := ast.CharlistLiteral{
// 		val: p.tok.lit.bytes()
// 	}
// 	if p.peek_tok.kind != .hash {
// 		p.next_token()
// 		return node, types.charlist_ti
// 	}
// 	for p.tok.kind == .str {
// 		p.next_token()
// 		if p.tok.kind != .hash {
// 			continue
// 		}
// 		p.check(.hash)
// 		p.expr(0)
// 	}
// 	return node, types.string_ti
// }

// fn (mut p Parser) tuple_expr() (ast.Expr, types.TypeIdent) {
// 	p.check(.lcbr)
// 	node0, _ := p.expr(0)
// 	mut values := [node0]
// 	for p.tok.kind == .comma {
// 		p.check(.comma)
// 		node1, _ := p.expr(0)
// 		values << node1
// 	}
// 	p.check(.rcbr)

// 	node := ast.TupleLiteral{
// 		values: values
// 	}
// 	return node, node.ti
// }

// fn (mut p Parser) keyword_list_expr() (ast.Expr, types.TypeIdent) {
// 	mut node := ast.Expr(ast.EmptyExpr{})
// 	mut keyword_list := ast.KeywordList{}
// 	breakpoint := [token.Kind.rpar, .rsbr, .eof]
// 	for p.tok.kind !in breakpoint {
// 		keyword := p.tok.lit
// 		mut atom := false

// 		if p.tok.kind == .key_keyword {
// 			p.check(.key_keyword)
// 			atom = true
// 		} else if p.tok.kind == .str {
// 			p.check(.str)
// 			p.next_token()
// 		} else {
// 			println('${p.tok.kind} not a keyword')
// 			exit(1)
// 		}
// 		value := p.tok.lit
// 		typ := types.ti_from_token(p.tok)

// 		keyword_list.put(keyword, value, typ, atom)
// 		p.next_token()
// 		if p.tok.kind != .comma && p.tok.kind in breakpoint {
// 		} else {
// 			p.check(.comma)
// 		}
// 	}
// 	node = keyword_list
// 	return node, types.void_ti
// }

fn (mut p Parser) block_expr(is_top_stmt bool) ast.Node {
	p.check(.key_do)
	meta := p.meta()
	mut stmts := []ast.Node{}
	for p.peek_tok.kind != .eof {
		if p.tok.kind == .key_end {
			break
		}
		stmts << p.stmt()
	}
	p.check(.key_end)
	if stmts.len == 0 {
		return p.node_list(meta, []ast.Node{})
	} else if stmts.len == 1 {
		return p.node_list(meta, [
			p.node_tuple(meta, [
				p.node_atomic('do'),
				stmts[0],
			]),
		])
	} else {
		return p.node_list(p.meta(), [
			p.node_tuple(meta, [
				p.node_atomic('do'),
				p.node(meta, '__block__', stmts),
			]),
		])
	}
}

fn (mut p Parser) module_decl() ast.Node {
	meta := p.meta()
	p.tok_inline = p.tok.line_nr
	p.check(.key_defmodule)
	mut module_path_name := [p.tok.lit]
	p.check(.modl)

	for p.tok.kind == .dot {
		p.check(.dot)
		module_path_name << p.tok.lit
		p.check(.modl)
	}
	p.module_name = module_path_name.join('.')

	block := p.block_expr(false)
	return p.node(meta, 'defmodule', [
		p.node(meta, '__aliases__', [
			p.node_atomic(p.module_name),
		]),
		block,
	])
}
