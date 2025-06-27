module main

import lxc.lexer

fn main() {
	input := 'x @ y'
	mut lx := lexer.new_lexer(input, 'test.lx')
	for {
		tok := lx.next_token()
		println(tok)
		if tok is lexer.EOFToken {
			break
		}
	}
	println('Has errors: ${lx.has_errors()}')
	println('Errors: ${lx.get_errors()}')
}
