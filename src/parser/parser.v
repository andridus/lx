module parser

import ast
import lexer
import token

struct Options {
	build_path string = '_build'
}
struct Parser {
	filename string
	options &Options = unsafe {nil}
	mut:
		lexer &lexer.Lexer
		current_token token.Token
		next_token token.Token
}

fn parse_stmt(text string) !ast.Node {
	mut l := lexer.new(text)!
	mut p := Parser{
		lexer: &l
	}
	p.current_token = p.lexer.read_next_token()!
	p.next_token = p.lexer.read_next_token()!
	return p.stmt()!
}

fn (p Parser) stmt() !ast.Node{
	curr := p.current_token
	match curr.kind() {
		.lit_int {
			return ast.Node{left: curr.value().int(), kind: ast.Integer{}}
		}
		.lit_float {
			return ast.Node{left: curr.value().f64(), kind: ast.Float{}}
		}
		else {
			return error('eof')
		}
	}
}