module main

import lxc.lexer

fn main() {
	input := 'x count _unused _123'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	println('Input: "${input}"')
	println('Input length: ${input.len}')

	for i := 0; i < 10; i++ {
		token := lexer0.next_token()
		println('Token ${i}: ${token}')

		if token is lexer.EOFToken {
			println('Reached EOF')
			break
		}
	}
}