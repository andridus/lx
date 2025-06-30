module main

import frontend.lexer
import frontend.parser
import backend.erlang
import analysis.typechecker

fn assert_lx_generates_erlang(lx_code string, expected_erlang string) {
	// Create lexer from string
	mut lexer_instance := lexer.new_lexer(lx_code, 'test')
	mut tokens := []lexer.Token{}

	// Tokenize the source
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

	// Parse the tokens
	mut parser_instance := parser.new_main_parser(tokens)
	module_stmt := parser_instance.parse_module() or { panic('Failed to parse function') }

	// Generate Erlang code
	erlang_gen := erlang.new_erlang_generator()
	type_ctx := typechecker.new_type_context()
	codegen_result := erlang_gen.generate_module(module_stmt, type_ctx)

	assert codegen_result.success
	assert codegen_result.code == expected_erlang
}
