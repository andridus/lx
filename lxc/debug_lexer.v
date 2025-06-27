module main

import lexer

fn main() {
	input := 'x count _unused _123'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	println('Input: "${input}"')
	println('Input bytes: ${input.bytes()}')

	for i := 0; i < 10; i++ {
		println('\n--- Token ${i} ---')
		println('Position: ${lexer0.get_position_index()}')
		println('State: ${lexer0.get_state()}')
		println('Buffer: "${lexer0.get_buffer()}"')

		token := lexer0.next_token()
		println('Token: ${token}')

		if token is lexer.EOFToken {
			println('Reached EOF')
			break
		}
	}
}
