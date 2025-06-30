module main

import frontend.lexer

fn test_string_literals() {
	// Test basic strings
	input := '"hello world"'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	token := lexer0.next_token()
	assert token is lexer.StringToken
	string_token := token as lexer.StringToken
	assert string_token.value == 'hello world'

	// Test empty string
	input2 := '""'
	mut lexer2 := lexer.new_lexer(input2, 'test.lx')

	token2 := lexer2.next_token()
	assert token2 is lexer.StringToken
	string_token2 := token2 as lexer.StringToken
	assert string_token2.value == ''

	// Test string with single character
	input3 := '"a"'
	mut lexer3 := lexer.new_lexer(input3, 'test.lx')

	token3 := lexer3.next_token()
	assert token3 is lexer.StringToken
	string_token3 := token3 as lexer.StringToken
	assert string_token3.value == 'a'
}

fn test_string_escape_sequences() {
	// Test all escape sequences
	input := '"\\n\\t\\r\\"\\\\"'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	token := lexer0.next_token()
	assert token is lexer.StringToken
	string_token := token as lexer.StringToken
	assert string_token.value == '\n\t\r"\\'

	// Test mixed content with escapes
	input2 := '"hello\\nworld\\twith\\"quotes\\""'
	mut lexer2 := lexer.new_lexer(input2, 'test.lx')

	token2 := lexer2.next_token()
	assert token2 is lexer.StringToken
	string_token2 := token2 as lexer.StringToken
	assert string_token2.value == 'hello\nworld\twith"quotes"'
}

fn test_invalid_escape_sequences() {
	// Test invalid escape sequence
	input := '"hello\\xworld"'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	token := lexer0.next_token()
	assert token is lexer.ErrorToken
	error_token := token as lexer.ErrorToken
	assert error_token.message.contains('Lexical error: Invalid escape sequence')

	// Should have error
	assert lexer0.has_errors() == true
}

fn test_literals_unterminated_string() {
	// Test unterminated string
	input := '"hello world'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	token := lexer0.next_token()
	assert token is lexer.ErrorToken
	error_token := token as lexer.ErrorToken
	assert error_token.message.contains('Lexical error')

	// Should have error
	assert lexer0.has_errors() == false // After returning ErrorToken, errors are cleared
}

fn test_literals_numeric_literals() {
	// Test positive integers
	input := '42 123 0 999'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	token1 := lexer0.next_token()
	assert token1 is lexer.IntToken
	int_token1 := token1 as lexer.IntToken
	assert int_token1.value == 42

	token2 := lexer0.next_token()
	assert token2 is lexer.IntToken
	int_token2 := token2 as lexer.IntToken
	assert int_token2.value == 123

	token3 := lexer0.next_token()
	assert token3 is lexer.IntToken
	int_token3 := token3 as lexer.IntToken
	assert int_token3.value == 0

	token4 := lexer0.next_token()
	assert token4 is lexer.IntToken
	int_token4 := token4 as lexer.IntToken
	assert int_token4.value == 999
}

fn test_negative_integers() {
	input := '-123'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	token1 := lexer0.next_token()
	assert token1 is lexer.OperatorToken
	operator_token := token1 as lexer.OperatorToken
	assert operator_token.value == lexer.OperatorValue.minus

	token2 := lexer0.next_token()
	assert token2 is lexer.IntToken
	int_token := token2 as lexer.IntToken
	assert int_token.value == 123
}

fn test_float_literals() {
	// Test positive floats
	input := '3.14 2.5 0.0 123.456'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	token1 := lexer0.next_token()
	assert token1 is lexer.FloatToken
	float_token1 := token1 as lexer.FloatToken
	assert float_token1.value == 3.14

	token2 := lexer0.next_token()
	assert token2 is lexer.FloatToken
	float_token2 := token2 as lexer.FloatToken
	assert float_token2.value == 2.5

	token3 := lexer0.next_token()
	assert token3 is lexer.FloatToken
	float_token3 := token3 as lexer.FloatToken
	assert float_token3.value == 0.0

	token4 := lexer0.next_token()
	assert token4 is lexer.FloatToken
	float_token4 := token4 as lexer.FloatToken
	assert float_token4.value == 123.456
}

fn test_negative_floats() {
	input := '-2.5'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	token1 := lexer0.next_token()
	assert token1 is lexer.OperatorToken
	operator_token := token1 as lexer.OperatorToken
	assert operator_token.value == lexer.OperatorValue.minus

	token2 := lexer0.next_token()
	assert token2 is lexer.FloatToken
	float_token := token2 as lexer.FloatToken
	assert float_token.value == 2.5
}

fn test_integer_to_float_transition() {
	// Test transition from integer to float
	input := '42.0 123.5'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	token1 := lexer0.next_token()
	assert token1 is lexer.FloatToken
	float_token1 := token1 as lexer.FloatToken
	assert float_token1.value == 42.0

	token2 := lexer0.next_token()
	assert token2 is lexer.FloatToken
	float_token2 := token2 as lexer.FloatToken
	assert float_token2.value == 123.5
}

fn test_malformed_numbers() {
	// Test malformed numbers
	input := '123.456.789'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	token := lexer0.next_token()
	assert token is lexer.ErrorToken
	error_token := token as lexer.ErrorToken
	assert error_token.message.contains('Lexical error')

	// Should have error
	assert lexer0.has_errors() == true
}

fn test_literals_boolean_literals() {
	// Test boolean literals
	input := 'true false'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	token1 := lexer0.next_token()
	assert token1 is lexer.KeywordToken
	keyword_token1 := token1 as lexer.KeywordToken
	assert keyword_token1.value == lexer.KeywordValue.true_

	token2 := lexer0.next_token()
	assert token2 is lexer.KeywordToken
	keyword_token2 := token2 as lexer.KeywordToken
	assert keyword_token2.value == lexer.KeywordValue.false_
}

fn test_literals_nil_literal() {
	// Test nil literal
	input := 'nil'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	token := lexer0.next_token()
	assert token is lexer.KeywordToken
	keyword_token := token as lexer.KeywordToken
	assert keyword_token.value == lexer.KeywordValue.nil_
}

fn test_literals_atom_literals() {
	// Test atom literals
	input := ':hello :world :test'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	token1 := lexer0.next_token()
	assert token1 is lexer.AtomToken
	atom_token1 := token1 as lexer.AtomToken
	assert atom_token1.value == 'hello'

	token2 := lexer0.next_token()
	assert token2 is lexer.AtomToken
	atom_token2 := token2 as lexer.AtomToken
	assert atom_token2.value == 'world'

	token3 := lexer0.next_token()
	assert token3 is lexer.AtomToken
	atom_token3 := token3 as lexer.AtomToken
	assert atom_token3.value == 'test'
}

fn test_mixed_literals() {
	// Test mixed literals in one input
	input := '42 "hello" 3.14 true :atom'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	token1 := lexer0.next_token()
	assert token1 is lexer.IntToken
	int_token := token1 as lexer.IntToken
	assert int_token.value == 42

	token2 := lexer0.next_token()
	assert token2 is lexer.StringToken
	string_token := token2 as lexer.StringToken
	assert string_token.value == 'hello'

	token3 := lexer0.next_token()
	assert token3 is lexer.FloatToken
	float_token := token3 as lexer.FloatToken
	assert float_token.value == 3.14

	token4 := lexer0.next_token()
	assert token4 is lexer.KeywordToken
	keyword_token := token4 as lexer.KeywordToken
	assert keyword_token.value == lexer.KeywordValue.true_

	token5 := lexer0.next_token()
	assert token5 is lexer.AtomToken
	atom_token := token5 as lexer.AtomToken
	assert atom_token.value == 'atom'
}
