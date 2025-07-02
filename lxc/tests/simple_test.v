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
	assert_lx_generates_erlang('def f() do\n1\nend', '-module(main).
-export([f/0]).\n
-spec f() -> integer().
f() ->
1.\n
')
}
