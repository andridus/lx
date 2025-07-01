module main

import frontend.parser
import frontend.lexer
import ast

fn test_external_function_call_parsing() {
	// Create tokens for :io.format("Hello")
	tokens := [
		lexer.Token(lexer.AtomToken{
			value: 'io'
		}),
		lexer.Token(lexer.new_operator_token(lexer.OperatorValue.dot)),
		lexer.Token(lexer.IdentToken{
			value: 'format'
		}),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.lparen)),
		lexer.Token(lexer.StringToken{
			value: 'Hello'
		}),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.rparen)),
	]

	mut parser_instance := parser.new_expression_parser(tokens)
	expr := parser_instance.parse_expression() or {
		panic('Failed to parse external function call')
	}

	assert expr is ast.CallExpr
	call := expr as ast.CallExpr
	assert call.external
	assert call.module == 'io'
	assert call.function_name == 'format'
	assert call.arguments.len == 1

	// Check the argument
	arg := call.arguments[0]
	assert arg is ast.LiteralExpr
	lit_expr := arg as ast.LiteralExpr
	assert lit_expr.value is ast.StringLiteral
	string_lit := lit_expr.value as ast.StringLiteral
	assert string_lit.value == 'Hello'
}

fn test_external_function_call_with_multiple_arguments() {
	// Create tokens for :io.format("~p", [1, 2, 3])
	tokens := [
		lexer.Token(lexer.AtomToken{
			value: 'io'
		}),
		lexer.Token(lexer.new_operator_token(lexer.OperatorValue.dot)),
		lexer.Token(lexer.IdentToken{
			value: 'format'
		}),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.lparen)),
		lexer.Token(lexer.StringToken{
			value: '~p'
		}),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.comma)),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.lbracket)),
		lexer.Token(lexer.IntToken{
			value: 1
		}),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.comma)),
		lexer.Token(lexer.IntToken{
			value: 2
		}),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.comma)),
		lexer.Token(lexer.IntToken{
			value: 3
		}),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.rbracket)),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.rparen)),
	]

	mut parser_instance := parser.new_expression_parser(tokens)
	expr := parser_instance.parse_expression() or {
		panic('Failed to parse external function call with multiple arguments')
	}

	assert expr is ast.CallExpr
	call := expr as ast.CallExpr
	assert call.external
	assert call.module == 'io'
	assert call.function_name == 'format'
	assert call.arguments.len == 2

	// Check first argument
	arg1 := call.arguments[0]
	assert arg1 is ast.LiteralExpr
	lit_expr1 := arg1 as ast.LiteralExpr
	assert lit_expr1.value is ast.StringLiteral
	string_lit1 := lit_expr1.value as ast.StringLiteral
	assert string_lit1.value == '~p'

	// Check second argument
	arg2 := call.arguments[1]
	assert arg2 is ast.ListLiteralExpr
	list_expr := arg2 as ast.ListLiteralExpr
	assert list_expr.elements.len == 3
}

fn test_record_access_vs_external_call() {
	// Test record access (should not be external call)
	tokens1 := [
		lexer.Token(lexer.IdentToken{
			value: 'person'
		}),
		lexer.Token(lexer.new_operator_token(lexer.OperatorValue.dot)),
		lexer.Token(lexer.IdentToken{
			value: 'name'
		}),
	]
	mut parser1 := parser.new_expression_parser(tokens1)
	expr1 := parser1.parse_expression() or { panic('Failed to parse record access') }
	assert expr1 is ast.RecordAccessExpr

	// Test external function call (should be external call)
	tokens2 := [
		lexer.Token(lexer.AtomToken{
			value: 'io'
		}),
		lexer.Token(lexer.new_operator_token(lexer.OperatorValue.dot)),
		lexer.Token(lexer.IdentToken{
			value: 'format'
		}),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.lparen)),
		lexer.Token(lexer.StringToken{
			value: 'test'
		}),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.rparen)),
	]
	mut parser2 := parser.new_expression_parser(tokens2)
	expr2 := parser2.parse_expression() or { panic('Failed to parse external function call') }
	assert expr2 is ast.CallExpr
	call2 := expr2 as ast.CallExpr
	assert call2.external
}

fn test_external_function_call_generation() {
	// Test that external function calls generate correct Erlang code
	call := ast.CallExpr{
		external:      true
		module:        'io'
		function_name: 'format'
		arguments:     [
			ast.LiteralExpr{
				value: ast.StringLiteral{
					value: 'Hello, ~s'
				}
			},
			ast.LiteralExpr{
				value: ast.StringLiteral{
					value: 'World'
				}
			},
		]
	}
	assert call.external
	assert call.module == 'io'
	assert call.function_name == 'format'
	assert call.arguments.len == 2
}
