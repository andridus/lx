module main

import frontend.parser
import frontend.lexer
import ast
import backend.erlang
import analysis.typechecker

fn test_simple_integer() {
	tokens := [
		lexer.Token(lexer.new_int_token(42)),
		lexer.Token(lexer.new_eof_token()),
	]
	mut parser0 := parser.new_main_parser(tokens)
	expr := parser0.parse_expression() or { panic('Failed to parse integer') }

	match expr {
		ast.LiteralExpr {
			match expr.value {
				ast.IntegerLiteral {
					assert expr.value.value == 42
				}
				else {
					panic('Expected IntegerLiteral')
				}
			}
		}
		else {
			panic('Expected LiteralExpr')
		}
	}
}

fn test_simple_addition() {
	tokens := [
		lexer.Token(lexer.new_int_token(1)),
		lexer.Token(lexer.new_operator_token(lexer.OperatorValue.plus)),
		lexer.Token(lexer.new_int_token(2)),
		lexer.Token(lexer.new_eof_token()),
	]
	mut parser0 := parser.new_main_parser(tokens)
	expr := parser0.parse_expression() or { panic('Failed to parse addition') }

	match expr {
		ast.BinaryExpr {
			assert expr.op == ast.BinaryOp.add
		}
		else {
			panic('Expected BinaryExpr')
		}
	}
}

fn test_simple_function_string() {
	source := 'def f() do\n1\nend'

	// Create lexer from string
	mut lexer_instance := lexer.new_lexer(source, 'test')
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

	// Simple assertion that code generation succeeded
	assert codegen_result.success

	// Expected Erlang code for: def f() do 1 end
	expected_code := '-module(main).\n-export([f/0]).\n\nf() ->\n1.\n'

	// Compare generated code with expected code
	assert codegen_result.code == expected_code
}
