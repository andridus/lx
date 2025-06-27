module main

import lexer

fn main() {
	input := 'x = 42'
	mut lx := lexer.new_lexer(input, 'manual.lx')
	println('Input: ${input}')
	for i := 0; i < 10; i++ {
		tok := lx.next_token()
		println('Token ${i}: ${tok}')
		if tok is lexer.EOFToken {
			break
		}
	}
}
