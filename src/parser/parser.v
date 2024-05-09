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
	current_token token.Token
	next_token    token.Token
}

pub fn parse_stmt(text string) !ast.Node {
	mut l := lexer.new(text)!
	mut p := Parser{
		lexer: &l
	}
	p.current_token = p.lexer.read_next_token()!
	p.next_token = p.lexer.read_next_token()!
	return p.stmt()!
}

fn (mut p Parser) call_next_token() ! {
	p.current_token = p.next_token
	p.next_token = p.lexer.read_next_token()!
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
		.lit_int {
			ast.new_node_2(curr.value(), ast.Integer{})
			// p.check_end_expr()!
			// return node
		}
		.lit_float {
			ast.new_node_2(curr.value(), ast.Float{})
			// p.check_end_expr()!
			// return node
		}
		else {
			return error('eof')
		}
	}
	if p.next_token.is_infix() {
		p.call_next_token()!
		left := node
		operator := p.current_token.value()
		p.call_next_token()!
		right := p.expr()!
		p.call_next_token()!
		return ast.new_node_3(operator, ast.Function{}, [left, right])
	} else {
		p.check_end_expr()!
		p.call_next_token()!
		return node
	}
}

fn (p Parser) check_end_expr() !bool {
	if p.next_token.kind() in [.eof, .newline] {
		return true
	} else {
		return error(p.gen_syntax_error())
	}
}
