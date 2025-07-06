module main

import frontend.lexer
import frontend.parser
import backend.erlang
import analysis.typechecker
import backend.codegen

fn generates_erlang(lx_code string) codegen.CodegenResult {
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

	// Run typechecker
	mut type_ctx := typechecker.new_type_context()
	mut type_checker := typechecker.new_type_checker()
	type_checker.context = type_ctx
	type_checker.check_module(module_stmt)

	// Generate Erlang code
	mut erlang_gen := erlang.new_erlang_generator()
	codegen_result := erlang_gen.generate_module(module_stmt, type_ctx)

	return codegen_result
}
