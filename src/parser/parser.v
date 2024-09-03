module parser

import ast
import lexer
import token

struct Options {
	build_path string = '_build'
}

struct Parser {
	filename string
	options  &Options = unsafe { nil }
mut:
	lexer         &lexer.Lexer
	stmts         []ast.Node
	current_token token.Token
	next_token    token.Token
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
		if p.current_token.kind == .newline {
			p.call_next_token()!
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

fn (mut p Parser) stmt() !ast.Node {
	curr := p.current_token
	match curr.kind() {
		// .lit_int {
		// 	return ast.Node{left: curr.value().int(), kind: ast.Integer{}}
		// }
		// .lit_float {
		// 	return ast.Node{left: curr.value().f64(), kind: ast.Float{}}
		// }
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
		._float {
			ast.new_node_2(curr.value(), ast.Float{})
		}
		._string {
			ast.new_node_2(curr.value(), ast.String{})
		}
		._charlist {
			ast.new_node_2(curr.value(), ast.Charlist{})
		}
		._true {
			ast.new_node(ast.Boolean.new(true))
		}
		._false {
			ast.new_node(ast.Boolean.new(false))
		}
		._atom {
			ast.new_node_2(curr.value(), ast.Atom.new(curr.value()))
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
		p.check_end_expr()!
		p.call_next_token()!
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

fn (mut p Parser) check_end_expr() !bool {
	if p.next_token.kind() in [.eof, .newline] {
		return true
	} else {
		return error(p.lexer.show_error_custom_error(p.gen_syntax_error()))
	}
}
