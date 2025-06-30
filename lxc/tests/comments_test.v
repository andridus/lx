module main

import frontend.lexer

fn test_inline_comments() {
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

fn test_comment_only_line() {
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

fn test_comment_with_code() {
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

fn test_multiple_comment_lines() {
	input := '# First comment\n# Second comment\nx = 42'
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

fn test_comment_at_end_of_file() {
	input := 'x = 42 # End of file comment'
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
}

fn test_comment_with_special_characters() {
	input := 'x = 42 # Comment with @#$%^&*() characters'
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
	// Comment with special characters should be ignored
}

fn test_comment_with_quotes() {
	input := 'x = 42 # Comment with "quotes" and \'single quotes\''
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
	// Comment with quotes should be ignored
}

fn test_comment_with_operators() {
	input := 'x = 42 # Comment with + - * / = < > operators'
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
	// Comment with operators should be ignored
}

fn test_comment_with_keywords() {
	input := 'x = 42 # Comment with def case if else keywords'
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
	// Comment with keywords should be ignored
}

fn test_comment_with_identifiers() {
	input := 'x = 42 # Comment with variable_name and ModuleName'
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
	// Comment with identifiers should be ignored
}

fn test_comment_with_numbers() {
	input := 'x = 42 # Comment with 123 3.14 numbers'
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
	// Comment with numbers should be ignored
}

fn test_comment_with_atoms() {
	input := 'x = 42 # Comment with :ok :error atoms'
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
	// Comment with atoms should be ignored
}

fn test_comment_with_strings() {
	input := 'x = 42 # Comment with "hello world" strings'
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
	// Comment with strings should be ignored
}

fn test_comment_with_escape_sequences() {
	input := 'x = 42 # Comment with \\n\\t\\r escape sequences'
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
	// Comment with escape sequences should be ignored
}

fn test_comment_line_numbering() {
	input := 'x = 42\n# Comment on line 2\ny = 10\n# Comment on line 4'
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

	// Second line (comment)
	// Line number should advance to 3 for the next token

	// Third line
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

fn test_empty_comment() {
	input := 'x = 42 #\ny = 10'
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
}
