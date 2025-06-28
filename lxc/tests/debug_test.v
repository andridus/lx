module main

import parser
import lexer
import ast

fn test_debug_parser() {
	tokens := [
		lexer.Token(lexer.IntToken{
			value: 1
		}),
		lexer.Token(lexer.OperatorToken.plus),
		lexer.Token(lexer.IntToken{
			value: 2
		}),
		lexer.Token(lexer.EOFToken{}),
	]

	println('Tokens: ${tokens}')

	mut parser0 := parser.new_main_parser(tokens)
	println('Parser created')

	// Vamos verificar o primeiro token
	println('Current token: ${parser0.current}')
	println('Is at end: ${parser0.is_at_end()}')

	// Tentar fazer parse da expressão diretamente no ExpressionParser
	mut expr_parser := parser.new_expression_parser(tokens)
	println('Expression parser created')
	println('Expr parser current token: ${expr_parser.current}')

	expr := expr_parser.parse_expression() or {
		println('Expression parser failed: ${expr_parser.get_errors()}')
		panic('Failed to parse expression')
	}

	println('Expression parse succeeded: ${expr}')

	// Agora tentar no MainParser
	mut main_parser := parser.new_main_parser(tokens)
	main_expr := main_parser.parse_expression() or {
		println('Main parser failed: ${main_parser.get_errors()}')
		panic('Failed to parse addition')
	}

	println('Main parser succeeded: ${main_expr}')
}
