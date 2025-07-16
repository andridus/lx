module main

import frontend.lexer

fn test_key_token_basic() {
	code := 'name: "John"'
	mut lexer_instance := lexer.new_lexer(code, 'test')

	// First token should be KeyToken
	token1 := lexer_instance.next_token()
	assert token1 is lexer.KeyToken
	key_token := token1 as lexer.KeyToken
	assert key_token.value == 'name'

	// Second token should be StringToken
	token2 := lexer_instance.next_token()
	assert token2 is lexer.StringToken
	string_token := token2 as lexer.StringToken
	assert string_token.value == 'John'

	// Third token should be EOF
	token3 := lexer_instance.next_token()
	assert token3 is lexer.EOFToken
}

fn test_map_with_key_tokens() {
	code := '%{name: "John", age: 30}'
	mut lexer_instance := lexer.new_lexer(code, 'test')
	mut tokens := []lexer.Token{}

	for {
		token := lexer_instance.next_token()
		if token is lexer.EOFToken {
			break
		}
		if token is lexer.ErrorToken {
			panic('Lexical error: ${token.message}')
		}
		tokens << token
	}

	// Should have: %, {, name:, "John", ,, age:, 30, }
	assert tokens.len == 8
	assert tokens[0] is lexer.OperatorToken // %
	assert tokens[1] is lexer.PunctuationToken // {
	assert tokens[2] is lexer.KeyToken // name:
	assert tokens[3] is lexer.StringToken // "John"
	assert tokens[4] is lexer.PunctuationToken // ,
	assert tokens[5] is lexer.KeyToken // age:
	assert tokens[6] is lexer.IntToken // 30
	assert tokens[7] is lexer.PunctuationToken // }
}
