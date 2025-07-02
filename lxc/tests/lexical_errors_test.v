module main

import frontend.lexer

fn test_errors_unterminated_string() {
	input := '"hello world'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	token := lexer0.next_token()
	assert token is lexer.ErrorToken
	error_token := token as lexer.ErrorToken
	assert error_token.message.contains('Lexical error: Unterminated string')

	// After consuming error, no more errors should be present
	assert lexer0.has_errors() == false
}

// fn test_invalid_character() {
// 	input := 'x @ y'
// 	mut lexer0 := lexer.new_lexer(input, 'test.lx')

// 	// x should be recognized
// 	token1 := lexer0.next_token()
// 	assert token1 is lexer.IdentToken
// 	ident_token := token1 as lexer.IdentToken
// 	assert ident_token.value == 'x'

// 	// @ should cause error - lexer stops here
// 	token_err := lexer0.next_token()
// 	assert token_err is lexer.ErrorToken
// 	error_token := token_err as lexer.ErrorToken
// 	assert error_token.message.contains('Lexical error: Unexpected character')

// 	// No more tokens should be consumed after error
// }

fn test_malformed_number() {
	input := '123.456.789'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	token := lexer0.next_token()
	assert token is lexer.ErrorToken
	error_token := token as lexer.ErrorToken
	assert error_token.message.contains('Lexical error:')
}

fn test_invalid_escape_sequence() {
	input := '"hello\\xworld"'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	token := lexer0.next_token()
	assert token is lexer.ErrorToken
	error_token := token as lexer.ErrorToken
	assert error_token.message.contains('Lexical error:')
}

// fn test_multiple_invalid_characters() {
// 	input := 'x @ y # z $ w'
// 	mut lexer0 := lexer.new_lexer(input, 'test.lx')

// 	// x
// 	token1 := lexer0.next_token()
// 	assert token1 is lexer.IdentToken
// 	ident_token1 := token1 as lexer.IdentToken
// 	assert ident_token1.value == 'x'

// 	// @ should cause error - lexer stops here
// 	token_err := lexer0.next_token()
// 	assert token_err is lexer.ErrorToken
// 	error_token := token_err as lexer.ErrorToken
// 	assert error_token.message.contains('Lexical error: Unexpected character')

// 	// No more tokens should be consumed after error
// }

fn test_invalid_operator_sequence() {
	input := 'x === y'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	// x
	token1 := lexer0.next_token()
	assert token1 is lexer.IdentToken
	ident_token1 := token1 as lexer.IdentToken
	assert ident_token1.value == 'x'

	// === should cause error - lexer stops here
	token_err := lexer0.next_token()
	assert token_err is lexer.ErrorToken
	error_token := token_err as lexer.ErrorToken
	assert error_token.message.contains('Lexical error:')

	// No more tokens should be consumed after error
}

fn test_invalid_identifier_start() {
	input := '123abc'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	token := lexer0.next_token()
	assert token is lexer.ErrorToken
	error_token := token as lexer.ErrorToken
	assert error_token.message.contains('Lexical error:')

	// After consuming error, no more tokens should be consumed
	assert lexer0.has_errors() == true
}

// fn test_invalid_identifier_middle() {
// 	input := 'x@y'
// 	mut lexer0 := lexer.new_lexer(input, 'test.lx')

// 	// x
// 	token1 := lexer0.next_token()
// 	assert token1 is lexer.IdentToken
// 	ident_token1 := token1 as lexer.IdentToken
// 	assert ident_token1.value == 'x'

// 	// @ should cause error - lexer stops here
// 	token_err := lexer0.next_token()
// 	assert token_err is lexer.ErrorToken
// 	error_token := token_err as lexer.ErrorToken
// 	assert error_token.message.contains('Lexical error: Unexpected character')

// 	// No more tokens should be consumed after error
// }

fn test_invalid_float_format() {
	input := '123..456'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	token := lexer0.next_token()
	assert token is lexer.ErrorToken
	error_token := token as lexer.ErrorToken
	assert error_token.message.contains('Lexical error:')
}

fn test_invalid_float_format2() {
	input := '.1'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	token1 := lexer0.next_token()
	assert token1 is lexer.OperatorToken
	operator_token := token1 as lexer.OperatorToken
	assert operator_token.value == lexer.OperatorValue.dot

	token2 := lexer0.next_token()
	assert token2 is lexer.IntToken
	int_token := token2 as lexer.IntToken
	assert int_token.value == 1
}

fn test_invalid_float_format3() {
	input := '123.'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	token := lexer0.next_token()
	assert token is lexer.ErrorToken
	error_token := token as lexer.ErrorToken
	assert error_token.message.contains('Lexical error:')
}

fn test_invalid_atom_format() {
	input := ':123'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	token := lexer0.next_token()
	assert token is lexer.ErrorToken
	error_token := token as lexer.ErrorToken
	assert error_token.message.contains('Lexical error:')
}

fn test_invalid_atom_format2() {
	input := ':@atom'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	token := lexer0.next_token()
	assert token is lexer.ErrorToken
	error_token := token as lexer.ErrorToken
	assert error_token.message.contains('Lexical error:')
}

fn test_invalid_string_escape() {
	input := '"hello\\"'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	token := lexer0.next_token()
	assert token is lexer.ErrorToken
	error_token := token as lexer.ErrorToken
	assert error_token.message.contains('Lexical error:')
}

fn test_invalid_string_escape2() {
	input := '"hello\n\\"'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	token := lexer0.next_token()
	assert token is lexer.ErrorToken
	error_token := token as lexer.ErrorToken
	assert error_token.message.contains('Lexical error:')
}

// fn test_error_recovery() {
// 	input := 'x @ y = 42'
// 	mut lexer0 := lexer.new_lexer(input, 'test.lx')

// 	// x
// 	token1 := lexer0.next_token()
// 	assert token1 is lexer.IdentToken
// 	ident_token1 := token1 as lexer.IdentToken
// 	assert ident_token1.value == 'x'

// 	// @ should cause error - lexer stops here
// 	token_err := lexer0.next_token()
// 	assert token_err is lexer.ErrorToken
// 	error_token := token_err as lexer.ErrorToken
// 	assert error_token.message.contains('Lexical error: Unexpected character')

// 	// No more tokens should be consumed after error
// }

// fn test_error_recovery_multiple() {
// 	input := 'x @ y # z $ w = 42'
// 	mut lexer0 := lexer.new_lexer(input, 'test.lx')

// 	// x
// 	token1 := lexer0.next_token()
// 	assert token1 is lexer.IdentToken
// 	ident_token1 := token1 as lexer.IdentToken
// 	assert ident_token1.value == 'x'

// 	// @ should cause error - lexer stops here
// 	token_err := lexer0.next_token()
// 	assert token_err is lexer.ErrorToken
// 	error_token := token_err as lexer.ErrorToken
// 	assert error_token.message.contains('Lexical error: Unexpected character')

// 	// No more tokens should be consumed after error
// }

// fn test_error_line_tracking() {
// 	input := 'x = 42\ny @ z'
// 	mut lexer0 := lexer.new_lexer(input, 'test.lx')

// 	token1 := lexer0.next_token()
// 	assert token1 is lexer.IdentToken
// 	ident_token1 := token1 as lexer.IdentToken
// 	assert ident_token1.value == 'x'
// 	_, line1, _ := lexer0.get_position_info()
// 	assert line1 == 1

// 	token2 := lexer0.next_token()
// 	assert token2 is lexer.OperatorToken
// 	operator_token1 := token2 as lexer.OperatorToken
// 	assert operator_token1.value == lexer.OperatorValue.assign
// 	_, line2, _ := lexer0.get_position_info()
// 	assert line2 == 1

// 	token3 := lexer0.next_token()
// 	assert token3 is lexer.IntToken
// 	int_token1 := token3 as lexer.IntToken
// 	assert int_token1.value == 42
// 	_, line3, _ := lexer0.get_position_info()
// 	assert line3 == 1

// 	// Second line
// 	token4 := lexer0.next_token()
// 	assert token4 is lexer.IdentToken
// 	ident_token2 := token4 as lexer.IdentToken
// 	assert ident_token2.value == 'y'
// 	_, line4, _ := lexer0.get_position_info()
// 	assert line4 == 2

// 	// @ should cause error on line 2 - lexer stops here
// 	token_err := lexer0.next_token()
// 	assert token_err is lexer.ErrorToken
// 	error_token := token_err as lexer.ErrorToken
// 	assert error_token.message.contains('Lexical error: Unexpected character')

// 	// No more tokens should be consumed after error
// }

// fn test_error_message_content() {
// 	input := 'x @ y'
// 	mut lexer0 := lexer.new_lexer(input, 'test.lx')

// 	// x
// 	token1 := lexer0.next_token()
// 	assert token1 is lexer.IdentToken
// 	ident_token1 := token1 as lexer.IdentToken
// 	assert ident_token1.value == 'x'

// 	// @ should cause error
// 	token_err := lexer0.next_token()
// 	assert token_err is lexer.ErrorToken
// 	error_token := token_err as lexer.ErrorToken
// 	assert error_token.message.contains('Lexical error: Unexpected character')

// 	// No more tokens should be consumed after error
// }

fn test_lexer_reset_clears_errors() {
	input := 'x @ y'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	// Process input to generate errors
	lexer0.next_token() // x
	lexer0.next_token() // @ (error) - lexer stops here

	// Reset lexer
	lexer0.reset()

	// Errors should be cleared
	assert lexer0.has_errors() == false
	assert lexer0.get_errors().len == 0
}

fn test_lexer_reset_position() {
	input := 'x = 42'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	// Process some tokens
	lexer0.next_token() // x
	lexer0.next_token() // =

	// Check position
	position, line, column := lexer0.get_position_info()
	assert position > 0
	assert line == 1
	assert column > 1

	// Reset lexer
	lexer0.reset()

	// Position should be reset
	position2, line2, column2 := lexer0.get_position_info()
	assert position2 == 0
	assert line2 == 1
	assert column2 == 1
	assert lexer0.get_state() == .initial
	assert lexer0.get_buffer() == ''
}

// fn test_unexpected_character() {
// 	input := '@'
// 	mut lexer0 := lexer.new_lexer(input, 'test.lx')

// 	token := lexer0.next_token()
// 	assert token is lexer.ErrorToken
// 	error_token := token as lexer.ErrorToken
// 	assert error_token.message.contains('Lexical error: Unexpected character')

// 	// No more tokens should be consumed after error
// }

// fn test_invalid_character_in_identifier() {
// 	input := 'x@y'
// 	mut lexer0 := lexer.new_lexer(input, 'test.lx')

// 	// x should be recognized
// 	token1 := lexer0.next_token()
// 	assert token1 is lexer.IdentToken
// 	ident_token := token1 as lexer.IdentToken
// 	assert ident_token.value == 'x'

// 	// @ should cause error - lexer stops here
// 	token_err := lexer0.next_token()
// 	assert token_err is lexer.ErrorToken
// 	error_token := token_err as lexer.ErrorToken
// 	assert error_token.message.contains('Lexical error: Unexpected character')

// 	// No more tokens should be consumed after error
// }

// fn test_unknown_operator() {
// 	input := 'x @ y'
// 	mut lexer0 := lexer.new_lexer(input, 'test.lx')

// 	// x should be recognized
// 	token1 := lexer0.next_token()
// 	assert token1 is lexer.IdentToken
// 	ident_token := token1 as lexer.IdentToken
// 	assert ident_token.value == 'x'

// 	// @ should cause error - lexer stops here
// 	token_err := lexer0.next_token()
// 	assert token_err is lexer.ErrorToken
// 	error_token := token_err as lexer.ErrorToken
// 	assert error_token.message.contains('Lexical error: Unexpected character')

// 	// No more tokens should be consumed after error
// }

fn test_invalid_number_format() {
	input := '123abc'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	token := lexer0.next_token()
	assert token is lexer.ErrorToken
	error_token := token as lexer.ErrorToken
	assert error_token.message.contains('Lexical error:')

	// After consuming error, no more tokens should be consumed
	assert lexer0.has_errors() == true
}

// fn test_invalid_character_after_identifier() {
// 	input := 'x@y'
// 	mut lexer0 := lexer.new_lexer(input, 'test.lx')

// 	// x
// 	token1 := lexer0.next_token()
// 	assert token1 is lexer.IdentToken
// 	ident_token1 := token1 as lexer.IdentToken
// 	assert ident_token1.value == 'x'

// 	// @ should cause error - lexer stops here
// 	token_err := lexer0.next_token()
// 	assert token_err is lexer.ErrorToken
// 	error_token := token_err as lexer.ErrorToken
// 	assert error_token.message.contains('Lexical error: Unexpected character')

// 	// No more tokens should be consumed after error
// }

// fn test_invalid_operator_after_identifier() {
// 	input := 'x@y'
// 	mut lexer0 := lexer.new_lexer(input, 'test.lx')

// 	// x should be recognized
// 	token1 := lexer0.next_token()
// 	assert token1 is lexer.IdentToken
// 	ident_token := token1 as lexer.IdentToken
// 	assert ident_token.value == 'x'

// 	// @ should cause error - lexer stops here
// 	token_err := lexer0.next_token()
// 	assert token_err is lexer.ErrorToken
// 	error_token := token_err as lexer.ErrorToken
// 	assert error_token.message.contains('Lexical error: Unexpected character')

// 	// No more tokens should be consumed after error
// }

fn test_invalid_character_in_string() {
	input := '"hello@world"'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	token := lexer0.next_token()
	assert token is lexer.ErrorToken
	error_token := token as lexer.ErrorToken
	assert error_token.message.contains('Lexical error:')
}

fn test_invalid_character_in_number() {
	input := '123@456'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	token := lexer0.next_token()
	assert token is lexer.IntToken
	int_token := token as lexer.IntToken
	assert int_token.value == 123

	// @ should cause error - lexer stops here
	token_err := lexer0.next_token()
	assert token_err is lexer.ErrorToken
	error_token := token_err as lexer.ErrorToken
	assert error_token.message.contains('Lexical error: Unexpected character')

	// No more tokens should be consumed after error
}

// fn test_invalid_character_in_atom() {
// 	input := ':ok@error'
// 	mut lexer0 := lexer.new_lexer(input, 'test.lx')

// 	token := lexer0.next_token()
// 	assert token is lexer.AtomToken
// 	atom_token := token as lexer.AtomToken
// 	assert atom_token.value == 'ok'

// 	// @ should cause error - lexer stops here
// 	token_err := lexer0.next_token()
// 	assert token_err is lexer.ErrorToken
// 	error_token := token_err as lexer.ErrorToken
// 	assert error_token.message.contains('Lexical error: Unexpected character')

// 	// No more tokens should be consumed after error
// }

fn test_invalid_character_in_comment() {
	input := '# This is a comment @ with invalid char'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	token := lexer0.next_token()
	assert token is lexer.EOFToken

	// Comment should be ignored, no error expected
	assert lexer0.has_errors() == false
}

fn test_invalid_negative_integer() {
	input := '-abc'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	token1 := lexer0.next_token()
	assert token1 is lexer.OperatorToken
	operator_token := token1 as lexer.OperatorToken
	assert operator_token.value == lexer.OperatorValue.minus

	token2 := lexer0.next_token()
	assert token2 is lexer.IdentToken
	ident_token := token2 as lexer.IdentToken
	assert ident_token.value == 'abc'
}
