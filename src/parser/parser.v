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

fn (p Parser) stmt() !ast.Node {
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

fn (p Parser) expr() !ast.Node {
	curr := p.current_token
	match curr.kind() {
		.lit_int {
			node := ast.new_node_2(curr.value(), ast.Integer{})
			p.check_end_expr()!
			return node
		}
		.lit_float {
			node := ast.new_node_2(curr.value(), ast.Float{})
			p.check_end_expr()!
			return node
		}
		else {
			return error('eof')
		}
	}
}

fn (p Parser) check_end_expr() !bool {
	if p.next_token.kind() in [.eof, .newline] {
		return true
	} else {
		return error('ERROR: trouble at end of expression')
	}
}
