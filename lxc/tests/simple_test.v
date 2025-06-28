module main

import parser
import lexer
import ast

fn test_simple_integer() {
	tokens := [
		lexer.Token(lexer.IntToken{
			value: 42
		}),
		lexer.Token(lexer.EOFToken{}),
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
		lexer.Token(lexer.IntToken{
			value: 1
		}),
		lexer.Token(lexer.OperatorToken.plus),
		lexer.Token(lexer.IntToken{
			value: 2
		}),
		lexer.Token(lexer.EOFToken{}),
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
