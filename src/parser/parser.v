module parser

import ast
import lexer
import token

const ending_kinds = [token.Kind.eof, .newline, ._comma]

struct Options {
	build_path string = '_build'
}

struct Parser {
	filename string
	options  &Options = unsafe { nil }
mut:
	lexer              &lexer.Lexer
	stmts              []ast.Node
	current_token      token.Token
	next_token         token.Token
	brackets_delimiter []token.Kind
	in_keyword_list    bool
}

pub fn parse_stmts(data []u8) ![]ast.Node {
	mut l := lexer.Lexer.init(data)!
	mut p := Parser{
		lexer: &l
	}
	p.call_next_token()!
	p.call_next_token()!
	for p.current_token.kind != .eof {
		stmt := p.stmt()!
		if !stmt.is_comment() { // ignore comment
			p.stmts << stmt
		}
		for {
			if p.current_token.kind == .newline {
				p.call_next_token()!
			} else {
				break
			}
		}
	}
	return p.stmts
}

pub fn parse_stmt(data []u8) !ast.Node {
	mut l := lexer.Lexer.init(data)!
	mut p := Parser{
		lexer: &l
	}
	p.current_token = p.lexer.read_next_token()!
	p.next_token = p.lexer.read_next_token()!
	return p.stmt()!
}

fn (mut p Parser) call_next_token() ! {
	p.current_token = p.next_token
	p.next_token = p.lexer.read_next_token() or {
		println(p.gen_token_error())
		exit(1)
	}
}

fn (mut p Parser) expect_keyword(keyword0 string) !token.Token {
	keyword := p.expect(._keyword_atom)!
	if keyword.value == keyword0 {
		return keyword
	} else {
		return error(p.lexer.show_error_custom_error('expect keyword atom `${keyword0}` but received `${keyword.value}`'))
	}
}

fn (mut p Parser) expect(kind token.Kind) !token.Token {
	curr := p.current_token
	if p.current_token.kind == kind {
		p.ignore_next_newline()
		if p.next_token.kind != .eof {
			p.call_next_token()!
		}
		return curr
	} else {
		return error(p.lexer.show_error_custom_error(p.gen_expect_error(kind)))
	}
}

// fn (mut p Parser) expect_next(kind token.Kind) ! {
// 	p.ignore_next_newline()
// 	if p.next_token.kind == kind {
// 		if p.next_token.kind != .eof {
// 			p.call_next_token()!
// 		}
// 	} else {
// 		error(p.lexer.show_error_custom_error(p.gen_expect_error(kind)))
// 	}
// }

fn (mut p Parser) call_match_and_next_token(kind token.Kind) ! {
	if p.current_token.kind == kind {
		p.call_next_token()!
	} else {
		error(p.lexer.show_error_custom_error(p.gen_expect_error(kind)))
	}
}

fn (mut p Parser) stmt() !ast.Node {
	curr := p.current_token
	match curr.kind() {
		._defmodule {
			return p.parse_module()
		}
		.eof {
			return error('eof')
		}
		else {
			return p.expr()
		}
	}
}

fn (mut p Parser) expr() !ast.Node {
	curr := p.current_token
	node := match curr.kind() {
		._int {
			ast.new_node_2(curr.value(), ast.Integer{})
		}
		._float, ._float_e {
			ast.new_node_2(curr.value(), ast.Float{})
		}
		._string {
			ast.new_node_2(curr.value(), ast.String{})
		}
		._aliases {
			p.parse_aliases()!
		}
		._charlist {
			ast.new_node_2(curr.value(), ast.Charlist{})
		}
		._lsbr {
			nodes, kind := p.parse_until(._rsbr)!
			ast.new_node_3('[]', ast.List.new(kind), nodes)
		}
		._lcbr {
			nodes, kind := p.parse_until(._rcbr)!
			if kind is ast.Mixed {
				k := kind as ast.Mixed
				ast.new_node_3('{}', ast.Tuple.new(k.kinds()), nodes)
			} else {
				ast.new_node_3('{}', ast.Tuple.new([kind]), nodes)
			}
		}
		._true {
			ast.new_node(ast.Boolean.new(true))
		}
		._false {
			ast.new_node(ast.Boolean.new(false))
		}
		._atom {
			ast.new_atom_node(curr.value())
		}
		._keyword_atom {
			p.parse_keyword_item()!
		}
		._comment {
			ast.new_node_2(curr.value(), ast.Comment{})
		}
		else {
			return error(p.lexer.show_error_custom_error('not parsed kind `${curr.kind()}`'))
		}
	}
	return p.maybe_apply_precendence(node)!
}

fn (mut p Parser) parse_keyword_item() !ast.Node {
	p.in_keyword_list = true
	key := p.expect(._keyword_atom)!
	value := p.expr()!
	p.in_keyword_list = false
	return ast.new_keyword_node(key.value, value)
}

fn (mut p Parser) parse_aliases() !ast.Node {
	mut aliases := []string{}
	for {
		aliases << p.current_token.value()
		if p.next_token.kind == ._dot {
			p.ignore_next_newline()
			p.call_next_token()!
			p.call_next_token() or { break }
		} else {
			break
		}
	}
	return ast.new_aliases_node(aliases)
}

fn (mut p Parser) ignore_next_newline() {
	for {
		if p.next_token.kind == .newline {
			p.call_next_token() or { break }
		} else {
			break
		}
	}
}

fn (mut p Parser) find_keyword() ?ast.Node {
	return none
}

fn (mut p Parser) maybe_apply_precendence(node ast.Node) !ast.Node {
	if prec := p.next_token.precedence() {
		p.call_next_token()!
		left := node
		function_token := p.current_token
		p.call_next_token()!
		right := p.expr()!

		// p.call_next_token()!
		function_kind := ast.Function{
			precedence: prec.get_precedence()
			position:   .infix
		}
		match prec.get_assoc() {
			.left {
				return p.insert_node_deep_left(function_token.value(), function_kind,
					left, right)
			}
			else {
				return ast.new_node_3(function_token.value(), function_kind, [left, right])
			}
		}
	} else {
		p.check_end_token()!
		return node
	}
}

fn (mut p Parser) insert_node_deep_left(name string, function ast.Function, left ast.Node, right ast.Node) !ast.Node {
	if right.kind is ast.Function {
		function0 := right.kind as ast.Function
		if function0.precedence > 0 && function0.precedence <= function.precedence
			&& right.nodes.len == 2 {
			left0 := right.nodes[0]
			right0 := right.nodes[1]
			node_left := p.insert_node_deep_left(name, function, left, left0)!
			name0 := right.left.to_str()
			return ast.new_node_3(name0, function0, [node_left, right0])
		}
	}
	return ast.new_node_3(name, function, [left, right])
}

fn (mut p Parser) check_end_token() !bool {
	if p.current_token.kind() in parser.ending_kinds {
		p.call_next_token()!
		p.call_next_token()!
		return true
	}
	if p.next_token.kind() in parser.ending_kinds {
		if !p.in_keyword_list {
			p.call_next_token()!
		}
		return true
	}

	if p.brackets_delimiter.len > 0 {
		if p.brackets_delimiter.last() == p.next_token.kind {
			// p.call_next_token()!

			return true
		}
	}
	return error(p.lexer.show_error_custom_error(p.gen_syntax_error()))
}

fn (mut p Parser) check_not_end_token() !bool {
	if p.current_token.kind() !in parser.ending_kinds {
		return true
	}
	return error(p.lexer.show_error_custom_error(p.gen_syntax_error()))
}

fn (mut p Parser) parse_until(until token.Kind) !([]ast.Node, ast.NodeKind) {
	p.call_next_token()!
	p.brackets_delimiter << until
	mut nodes := []ast.Node{}
	mut kind := ast.NodeKind(ast.Nil{})
	for {
		if p.current_token.kind == until {
			p.call_next_token()!
			break
		}
		node := p.expr()!
		if nodes.len == 0 {
			kind = node.kind
		} else if node.kind != kind {
			kind = ast.NodeKind(ast.Mixed.new([kind, node.kind]))
		} else if mut kind is ast.Mixed {
			kind.put_if_required(node.kind)
		}
		nodes << node
		if p.next_token.kind == until {
			p.call_next_token() or { break }
			break
		}
		// println(p)
		// p.call_next_token() or { break}

		p.ignore_next_newline()
		p.expect(._comma)!
		p.check_not_end_token()!
	}
	p.brackets_delimiter.delete_last()
	return nodes, kind
}
