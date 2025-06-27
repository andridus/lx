module main

import lexer

fn test_lexer_initialization() {
	input := 'x = 42'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	// Test that lexer was created successfully
	assert lexer0.get_input() == 'x = 42'
	position, line, column := lexer0.get_position_info()
	assert position == 0
	assert line == 1
	assert column == 1
	assert lexer0.get_filename() == 'test.lx'
	assert lexer0.get_state() == .initial
	assert lexer0.get_buffer() == ''
	assert lexer0.get_errors().len == 0
}

fn test_basic_token_scanning() {
	input := 'x = 42'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	// Test sequence of tokens
	token1 := lexer0.next_token()
	assert token1 is lexer.IdentToken
	ident_token := token1 as lexer.IdentToken
	assert ident_token.value == 'x'

	token2 := lexer0.next_token()
	assert token2 is lexer.OperatorToken
	operator_token := token2 as lexer.OperatorToken
	assert operator_token == lexer.OperatorToken.assign

	token3 := lexer0.next_token()
	assert token3 is lexer.IntToken
	int_token := token3 as lexer.IntToken
	assert int_token.value == 42

	token4 := lexer0.next_token()
	assert token4 is lexer.EOFToken
}

fn test_whitespace_handling() {
	input := '  x  =  42  '
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	// Whitespace should be ignored
	token1 := lexer0.next_token()
	assert token1 is lexer.IdentToken
	ident_token := token1 as lexer.IdentToken
	assert ident_token.value == 'x'

	token2 := lexer0.next_token()
	assert token2 is lexer.OperatorToken
	operator_token := token2 as lexer.OperatorToken
	assert operator_token == lexer.OperatorToken.assign

	token3 := lexer0.next_token()
	assert token3 is lexer.IntToken
	int_token := token3 as lexer.IntToken
	assert int_token.value == 42
}

fn test_newline_handling() {
	input := 'x\n= 42'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	token1 := lexer0.next_token()
	assert token1 is lexer.IdentToken
	ident_token := token1 as lexer.IdentToken
	assert ident_token.value == 'x'

	token2 := lexer0.next_token()
	assert token2 is lexer.OperatorToken
	operator_token := token2 as lexer.OperatorToken
	assert operator_token == lexer.OperatorToken.assign
}

fn test_core_string_literals() {
	// Test basic strings
	input := '"hello world"'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	token := lexer0.next_token()
	assert token is lexer.StringToken
	string_token := token as lexer.StringToken
	assert string_token.value == 'hello world'

	// Test strings with escapes
	input2 := '"hello\\nworld\\twith\\"quotes\\""'
	mut lexer2 := lexer.new_lexer(input2, 'test.lx')

	token2 := lexer2.next_token()
	assert token2 is lexer.StringToken
	string_token2 := token2 as lexer.StringToken
	assert string_token2.value == 'hello\nworld\twith"quotes"'

	// Test unterminated string
	input3 := '"unterminated'
	mut lexer3 := lexer.new_lexer(input3, 'test.lx')

	token3 := lexer3.next_token()
	assert token3 is lexer.ErrorToken
	error_token3 := token3 as lexer.ErrorToken
	assert error_token3.message.contains('Unterminated string')
	// Após consumir erro, não deve haver mais erros
	assert lexer3.has_errors() == false
}

fn test_core_numeric_literals() {
	// Test integers
	input := '42 -123 0'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	token1 := lexer0.next_token()
	assert token1 is lexer.IntToken
	int_token1 := token1 as lexer.IntToken
	assert int_token1.value == 42

	token2 := lexer0.next_token()
	assert token2 is lexer.IntToken
	int_token2 := token2 as lexer.IntToken
	assert int_token2.value == -123

	token3 := lexer0.next_token()
	assert token3 is lexer.IntToken
	int_token3 := token3 as lexer.IntToken
	assert int_token3.value == 0

	// Test floats
	input2 := '3.14 -2.5 0.0'
	mut lexer2 := lexer.new_lexer(input2, 'test.lx')

	token4 := lexer2.next_token()
	assert token4 is lexer.FloatToken
	float_token1 := token4 as lexer.FloatToken
	assert float_token1.value == 3.14

	token5 := lexer2.next_token()
	assert token5 is lexer.FloatToken
	float_token2 := token5 as lexer.FloatToken
	assert float_token2.value == -2.5

	token6 := lexer2.next_token()
	assert token6 is lexer.FloatToken
	float_token3 := token6 as lexer.FloatToken
	assert float_token3.value == 0.0
}

fn test_core_boolean_literals() {
	input := 'true false'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	token1 := lexer0.next_token()
	assert token1 is lexer.KeywordToken
	keyword_token1 := token1 as lexer.KeywordToken
	assert keyword_token1 == lexer.KeywordToken.true_

	token2 := lexer0.next_token()
	assert token2 is lexer.KeywordToken
	keyword_token2 := token2 as lexer.KeywordToken
	assert keyword_token2 == lexer.KeywordToken.false_
}

fn test_core_atom_literals() {
	input := ':ok :error :timeout'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	token1 := lexer0.next_token()
	assert token1 is lexer.AtomToken
	atom_token1 := token1 as lexer.AtomToken
	assert atom_token1.value == 'ok'

	token2 := lexer0.next_token()
	assert token2 is lexer.AtomToken
	atom_token2 := token2 as lexer.AtomToken
	assert atom_token2.value == 'error'

	token3 := lexer0.next_token()
	assert token3 is lexer.AtomToken
	atom_token3 := token3 as lexer.AtomToken
	assert atom_token3.value == 'timeout'
}

fn test_core_nil_literal() {
	input := 'nil'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	token := lexer0.next_token()
	assert token is lexer.KeywordToken
	keyword_token := token as lexer.KeywordToken
	assert keyword_token == lexer.KeywordToken.nil_
}

fn test_core_variable_identifiers() {
	input := 'x count _unused _123'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	token1 := lexer0.next_token()
	assert token1 is lexer.IdentToken
	ident_token1 := token1 as lexer.IdentToken
	assert ident_token1.value == 'x'

	token2 := lexer0.next_token()
	assert token2 is lexer.IdentToken
	ident_token2 := token2 as lexer.IdentToken
	assert ident_token2.value == 'count'

	token3 := lexer0.next_token()
	assert token3 is lexer.IdentToken
	ident_token3 := token3 as lexer.IdentToken
	assert ident_token3.value == '_unused'

	token4 := lexer0.next_token()
	assert token4 is lexer.IdentToken
	ident_token4 := token4 as lexer.IdentToken
	assert ident_token4.value == '_123'
}

fn test_core_record_identifiers() {
	input := 'Person UserData Config'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	token1 := lexer0.next_token()
	assert token1 is lexer.UpperIdentToken
	upper_ident_token1 := token1 as lexer.UpperIdentToken
	assert upper_ident_token1.value == 'Person'

	token2 := lexer0.next_token()
	assert token2 is lexer.UpperIdentToken
	upper_ident_token2 := token2 as lexer.UpperIdentToken
	assert upper_ident_token2.value == 'UserData'

	token3 := lexer0.next_token()
	assert token3 is lexer.UpperIdentToken
	upper_ident_token3 := token3 as lexer.UpperIdentToken
	assert upper_ident_token3.value == 'Config'
}

fn test_core_special_identifiers() {
	input := '__MODULE__'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	token := lexer0.next_token()
	assert token is lexer.IdentToken
	ident_token := token as lexer.IdentToken
	assert ident_token.value == '__MODULE__'
}

fn test_core_assignment_operators() {
	input := '= <-'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	token1 := lexer0.next_token()
	assert token1 is lexer.OperatorToken
	operator_token1 := token1 as lexer.OperatorToken
	assert operator_token1 == lexer.OperatorToken.assign

	token2 := lexer0.next_token()
	assert token2 is lexer.OperatorToken
	operator_token2 := token2 as lexer.OperatorToken
	assert operator_token2 == lexer.OperatorToken.pattern_match
}

fn test_core_arithmetic_operators() {
	input := '+ - * /'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	token1 := lexer0.next_token()
	assert token1 is lexer.OperatorToken
	operator_token1 := token1 as lexer.OperatorToken
	assert operator_token1 == lexer.OperatorToken.plus

	token2 := lexer0.next_token()
	assert token2 is lexer.OperatorToken
	operator_token2 := token2 as lexer.OperatorToken
	assert operator_token2 == lexer.OperatorToken.minus

	token3 := lexer0.next_token()
	assert token3 is lexer.OperatorToken
	operator_token3 := token3 as lexer.OperatorToken
	assert operator_token3 == lexer.OperatorToken.mult

	token4 := lexer0.next_token()
	assert token4 is lexer.OperatorToken
	operator_token4 := token4 as lexer.OperatorToken
	assert operator_token4 == lexer.OperatorToken.div
}

fn test_core_comparison_operators() {
	input := '== != < > <= >='
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	token1 := lexer0.next_token()
	assert token1 is lexer.OperatorToken
	operator_token1 := token1 as lexer.OperatorToken
	assert operator_token1 == lexer.OperatorToken.eq

	token2 := lexer0.next_token()
	assert token2 is lexer.OperatorToken
	operator_token2 := token2 as lexer.OperatorToken
	assert operator_token2 == lexer.OperatorToken.neq

	token3 := lexer0.next_token()
	assert token3 is lexer.OperatorToken
	operator_token3 := token3 as lexer.OperatorToken
	assert operator_token3 == lexer.OperatorToken.lt

	token4 := lexer0.next_token()
	assert token4 is lexer.OperatorToken
	operator_token4 := token4 as lexer.OperatorToken
	assert operator_token4 == lexer.OperatorToken.gt

	token5 := lexer0.next_token()
	assert token5 is lexer.OperatorToken
	operator_token5 := token5 as lexer.OperatorToken
	assert operator_token5 == lexer.OperatorToken.leq

	token6 := lexer0.next_token()
	assert token6 is lexer.OperatorToken
	operator_token6 := token6 as lexer.OperatorToken
	assert operator_token6 == lexer.OperatorToken.geq
}

fn test_core_logical_operators() {
	input := 'and or not andalso orelse'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	token1 := lexer0.next_token()
	assert token1 is lexer.OperatorToken
	operator_token1 := token1 as lexer.OperatorToken
	assert operator_token1 == lexer.OperatorToken.and_

	token2 := lexer0.next_token()
	assert token2 is lexer.OperatorToken
	operator_token2 := token2 as lexer.OperatorToken
	assert operator_token2 == lexer.OperatorToken.or_

	token3 := lexer0.next_token()
	assert token3 is lexer.OperatorToken
	operator_token3 := token3 as lexer.OperatorToken
	assert operator_token3 == lexer.OperatorToken.not_

	token4 := lexer0.next_token()
	assert token4 is lexer.OperatorToken
	operator_token4 := token4 as lexer.OperatorToken
	assert operator_token4 == lexer.OperatorToken.andalso

	token5 := lexer0.next_token()
	assert token5 is lexer.OperatorToken
	operator_token5 := token5 as lexer.OperatorToken
	assert operator_token5 == lexer.OperatorToken.orelse
}

fn test_punctuation() {
	input := '() {} [] , ;'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	token1 := lexer0.next_token()
	assert token1 is lexer.PunctuationToken
	punct_token1 := token1 as lexer.PunctuationToken
	assert punct_token1 == lexer.PunctuationToken.lparen

	token2 := lexer0.next_token()
	assert token2 is lexer.PunctuationToken
	punct_token2 := token2 as lexer.PunctuationToken
	assert punct_token2 == lexer.PunctuationToken.rparen

	token3 := lexer0.next_token()
	assert token3 is lexer.PunctuationToken
	punct_token3 := token3 as lexer.PunctuationToken
	assert punct_token3 == lexer.PunctuationToken.lbrace

	token4 := lexer0.next_token()
	assert token4 is lexer.PunctuationToken
	punct_token4 := token4 as lexer.PunctuationToken
	assert punct_token4 == lexer.PunctuationToken.rbrace

	token5 := lexer0.next_token()
	assert token5 is lexer.PunctuationToken
	punct_token5 := token5 as lexer.PunctuationToken
	assert punct_token5 == lexer.PunctuationToken.lbracket

	token6 := lexer0.next_token()
	assert token6 is lexer.PunctuationToken
	punct_token6 := token6 as lexer.PunctuationToken
	assert punct_token6 == lexer.PunctuationToken.rbracket

	token7 := lexer0.next_token()
	assert token7 is lexer.PunctuationToken
	punct_token7 := token7 as lexer.PunctuationToken
	assert punct_token7 == lexer.PunctuationToken.comma

	token8 := lexer0.next_token()
	assert token8 is lexer.PunctuationToken
	punct_token8 := token8 as lexer.PunctuationToken
	assert punct_token8 == lexer.PunctuationToken.semicolon
}

fn test_core_inline_comments() {
	input := 'x = 42 # This is a comment'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	token1 := lexer0.next_token()
	assert token1 is lexer.IdentToken
	ident_token := token1 as lexer.IdentToken
	assert ident_token.value == 'x'

	token2 := lexer0.next_token()
	assert token2 is lexer.OperatorToken
	operator_token := token2 as lexer.OperatorToken
	assert operator_token == lexer.OperatorToken.assign

	token3 := lexer0.next_token()
	assert token3 is lexer.IntToken
	int_token := token3 as lexer.IntToken
	assert int_token.value == 42

	token4 := lexer0.next_token()
	assert token4 is lexer.EOFToken
	// Comment should be ignored
}

fn test_core_comment_only_line() {
	input := '# This is a comment line\nx = 42'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	token1 := lexer0.next_token()
	assert token1 is lexer.IdentToken
	ident_token := token1 as lexer.IdentToken
	assert ident_token.value == 'x'

	token2 := lexer0.next_token()
	assert token2 is lexer.OperatorToken
	operator_token := token2 as lexer.OperatorToken
	assert operator_token == lexer.OperatorToken.assign

	token3 := lexer0.next_token()
	assert token3 is lexer.IntToken
	int_token := token3 as lexer.IntToken
	assert int_token.value == 42
}

fn test_core_comment_with_code() {
	input := 'x = 42 # Set x to 42\ny = 10 # Set y to 10'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	// First line
	token1 := lexer0.next_token()
	assert token1 is lexer.IdentToken
	ident_token1 := token1 as lexer.IdentToken
	assert ident_token1.value == 'x'

	token2 := lexer0.next_token()
	assert token2 is lexer.OperatorToken
	operator_token1 := token2 as lexer.OperatorToken
	assert operator_token1 == lexer.OperatorToken.assign

	token3 := lexer0.next_token()
	assert token3 is lexer.IntToken
	int_token1 := token3 as lexer.IntToken
	assert int_token1.value == 42

	// Second line
	token4 := lexer0.next_token()
	assert token4 is lexer.IdentToken
	ident_token2 := token4 as lexer.IdentToken
	assert ident_token2.value == 'y'

	token5 := lexer0.next_token()
	assert token5 is lexer.OperatorToken
	operator_token2 := token5 as lexer.OperatorToken
	assert operator_token2 == lexer.OperatorToken.assign

	token6 := lexer0.next_token()
	assert token6 is lexer.IntToken
	int_token2 := token6 as lexer.IntToken
	assert int_token2.value == 10

	token7 := lexer0.next_token()
	assert token7 is lexer.EOFToken
}

fn test_lexer_errors() {
	input := '"unterminated'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')
	token := lexer0.next_token()
	assert token is lexer.ErrorToken
	error_token := token as lexer.ErrorToken
	assert error_token.message.contains('Unterminated string')
	// Após consumir erro, não deve haver mais erros
	assert lexer0.has_errors() == false
}

fn test_lexer_position_tracking() {
	input := 'x = 42\ny = 10'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	// First line
	lexer0.next_token()
	_, line1, column1 := lexer0.get_position_info()
	assert line1 == 1
	assert column1 == 2

	lexer0.next_token()
	_, line2, column2 := lexer0.get_position_info()
	assert line2 == 1
	assert column2 == 4

	lexer0.next_token()
	_, line3, column3 := lexer0.get_position_info()
	assert line3 == 1
	assert column3 == 7

	// Second line
	lexer0.next_token()
	_, line4, column4 := lexer0.get_position_info()
	assert line4 == 2
	assert column4 == 2
}
