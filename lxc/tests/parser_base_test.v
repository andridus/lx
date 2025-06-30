module main

import frontend.parser
import frontend.lexer
import ast

fn test_parser_initialization() {
	tokens := [
		lexer.Token(lexer.new_ident_token('x')),
		lexer.Token(lexer.new_operator_token(lexer.OperatorValue.assign)),
		lexer.Token(lexer.new_int_token(42)),
		lexer.Token(lexer.new_eof_token()),
	]

	mut parser0 := parser.new_main_parser(tokens)

	assert parser0.tokens.len == 4
	assert parser0.position == 0
	assert parser0.current == lexer.Token(lexer.new_ident_token('x'))
	assert parser0.errors.len == 0
}

fn test_token_advancement() {
	tokens := [
		lexer.Token(lexer.new_ident_token('x')),
		lexer.Token(lexer.new_operator_token(lexer.OperatorValue.assign)),
		lexer.Token(lexer.new_int_token(42)),
		lexer.Token(lexer.new_eof_token()),
	]

	mut parser0 := parser.new_main_parser(tokens)

	assert parser0.current == lexer.Token(lexer.new_ident_token('x'))
	parser0.advance()
	assert parser0.position == 1
	assert parser0.current == lexer.Token(lexer.new_operator_token(lexer.OperatorValue.assign))
	parser0.advance()
	assert parser0.current == lexer.Token(lexer.new_int_token(42))
}

fn test_precedence_table() {
	table := parser.new_precedence_table()

	// Test operator precedence - commented out until precedence table is implemented
	assert table.get_precedence(lexer.OperatorValue.plus) == parser.Precedence.term
	assert table.get_precedence(lexer.OperatorValue.mult) == parser.Precedence.factor
	assert table.get_precedence(lexer.OperatorValue.assign) == parser.Precedence.assignment
	assert table.get_precedence(lexer.OperatorValue.eq) == parser.Precedence.equality
	assert table.get_precedence(lexer.OperatorValue.and_) == parser.Precedence.and_

	// For now, just test that the table can be created
	assert table != unsafe { nil }
}

fn test_parser_error_handling() {
	tokens := [
		lexer.Token(lexer.new_ident_token('x')),
		lexer.Token(lexer.new_error_token('Unexpected token')),
		lexer.Token(lexer.new_eof_token()),
	]

	mut parser0 := parser.new_main_parser(tokens)
	mut last_pos := parser0.position
	for !parser0.is_at_end() {
		_ = parser0.parse_expression()
		if parser0.position == last_pos {
			parser0.advance()
		}
		last_pos = parser0.position
	}
	assert parser0.errors.len > 0
}

fn test_parser_error_recovery_with_multiple_errors() {
	tokens := [
		lexer.Token(lexer.new_ident_token('x')),
		lexer.Token(lexer.new_error_token('First error')),
		lexer.Token(lexer.new_ident_token('y')),
		lexer.Token(lexer.new_error_token('Second error')),
		lexer.Token(lexer.new_eof_token()),
	]

	mut parser0 := parser.new_main_parser(tokens)
	mut last_pos := parser0.position
	for !parser0.is_at_end() {
		_ = parser0.parse_expression() or { break }
		if parser0.position == last_pos {
			parser0.advance()
		}
		last_pos = parser0.position
	}
	assert parser0.errors.len >= 2
}

fn test_parser_error_recovery_with_valid_code_after_error() {
	tokens := [
		lexer.Token(lexer.new_ident_token('x')),
		lexer.Token(lexer.new_error_token('Error in first expression')),
		lexer.Token(lexer.new_ident_token('y')),
		lexer.Token(lexer.new_operator_token(lexer.OperatorValue.assign)),
		lexer.Token(lexer.new_int_token(42)),
		lexer.Token(lexer.new_eof_token()),
	]

	mut parser0 := parser.new_main_parser(tokens)

	// Try to parse expressions until end
	mut last_pos := parser0.position
	for !parser0.is_at_end() {
		_ = parser0.parse_expression() or { break }
		if parser0.position == last_pos {
			parser0.advance()
		}
		last_pos = parser0.position
	}

	assert parser0.errors.len > 0
}

fn test_literal_parsing() {
	tokens := [
		lexer.Token(lexer.new_int_token(42)),
		lexer.Token(lexer.new_eof_token())
	]
	mut parser0 := parser.new_main_parser(tokens)
	expr := parser0.parse_expression() or { panic('Failed to parse integer') }
	match expr {
		ast.LiteralExpr {
			match expr.value {
				ast.IntegerLiteral { assert expr.value.value == 42 }
				else { assert false, 'Expected IntegerLiteral' }
			}
		}
		else {
			assert false, 'Expected LiteralExpr'
		}
	}

	// Test string literal
	tokens2 := [
		lexer.Token(lexer.new_string_token('hello')),
		lexer.Token(lexer.new_eof_token())
	]
	mut parser2 := parser.new_main_parser(tokens2)
	expr2 := parser2.parse_expression() or { panic('Failed to parse string') }
	match expr2 {
		ast.LiteralExpr {
			match expr2.value {
				ast.StringLiteral { assert expr2.value.value == 'hello' }
				else { assert false, 'Expected StringLiteral' }
			}
		}
		else {
			assert false, 'Expected LiteralExpr'
		}
	}

	// Test boolean literal
	tokens3 := [
		lexer.Token(lexer.new_bool_token(true)),
		lexer.Token(lexer.new_eof_token())
	]
	mut parser3 := parser.new_main_parser(tokens3)
	expr3 := parser3.parse_expression() or { panic('Failed to parse boolean') }
	match expr3 {
		ast.LiteralExpr {
			match expr3.value {
				ast.BooleanLiteral { assert expr3.value.value == true }
				else { assert false, 'Expected BooleanLiteral' }
			}
		}
		else {
			assert false, 'Expected LiteralExpr'
		}
	}

	// Test atom literal
	tokens4 := [
		lexer.Token(lexer.new_atom_token('ok')),
		lexer.Token(lexer.new_eof_token())
	]
	mut parser4 := parser.new_main_parser(tokens4)
	expr4 := parser4.parse_expression() or { panic('Failed to parse atom') }
	match expr4 {
		ast.LiteralExpr {
			match expr4.value {
				ast.AtomLiteral { assert expr4.value.value == 'ok' }
				else { assert false, 'Expected AtomLiteral' }
			}
		}
		else {
			assert false, 'Expected LiteralExpr'
		}
	}

	// Test nil literal
	tokens5 := [
		lexer.Token(lexer.new_keyword_token(lexer.KeywordValue.nil_)),
		lexer.Token(lexer.new_eof_token())
	]
	mut parser5 := parser.new_main_parser(tokens5)
	expr5 := parser5.parse_expression() or { panic('Failed to parse nil') }
	match expr5 {
		ast.LiteralExpr {
			match expr5.value {
				ast.NilLiteral { /* OK */ }
				else { assert false, 'Expected NilLiteral' }
			}
		}
		else {
			assert false, 'Expected LiteralExpr'
		}
	}
}

fn test_variable_parsing() {
	tokens := [
		lexer.Token(lexer.new_ident_token('x')),
		lexer.Token(lexer.new_eof_token())
	]
	mut parser0 := parser.new_main_parser(tokens)
	expr := parser0.parse_expression() or { panic('Failed to parse variable') }
	match expr {
		ast.VariableExpr {
			assert expr.name == 'x'
		}
		else {
			assert false, 'Expected VariableExpr'
		}
	}
}

fn test_assignment_parsing() {
	tokens := [
		lexer.Token(lexer.new_ident_token('x')),
		lexer.Token(lexer.new_operator_token(lexer.OperatorValue.assign)),
		lexer.Token(lexer.new_int_token(42)),
		lexer.Token(lexer.new_eof_token())
	]
	mut parser0 := parser.new_main_parser(tokens)
	expr := parser0.parse_expression() or { panic('Failed to parse assignment') }
	match expr {
		ast.AssignExpr {
			assert expr.name == 'x'
			match expr.value {
				ast.LiteralExpr {
					match expr.value.value {
						ast.IntegerLiteral { assert expr.value.value.value == 42 }
						else { assert false, 'Expected IntegerLiteral' }
					}
				}
				else { assert false, 'Expected LiteralExpr' }
			}
		}
		else {
			assert false, 'Expected AssignExpr'
		}
	}
}

fn test_function_call_parsing() {
	tokens := [
		lexer.Token(lexer.new_ident_token('sum')),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.lparen)),
		lexer.Token(lexer.new_int_token(3)),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.comma)),
		lexer.Token(lexer.new_int_token(4)),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.rparen)),
		lexer.Token(lexer.new_eof_token()),
	]
	mut parser0 := parser.new_main_parser(tokens)
	expr := parser0.parse_expression() or { panic('Failed to parse function call') }
	match expr {
		ast.CallExpr {
			assert expr.function is ast.VariableExpr
			if expr.function is ast.VariableExpr {
				assert expr.function.name == 'sum'
			}
			assert expr.arguments.len == 2
			assert expr.arguments[0] is ast.LiteralExpr
			assert expr.arguments[1] is ast.LiteralExpr
		}
		else {
			assert false, 'Expected CallExpr'
		}
	}
}

fn test_external_call_parsing() {
	tokens := [
		lexer.Token(lexer.new_ident_token('math')),
		lexer.Token(lexer.new_operator_token(lexer.OperatorValue.dot)),
		lexer.Token(lexer.new_ident_token('pow')),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.lparen)),
		lexer.Token(lexer.new_int_token(2)),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.comma)),
		lexer.Token(lexer.new_int_token(8)),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.rparen)),
		lexer.Token(lexer.new_eof_token()),
	]
	mut parser0 := parser.new_main_parser(tokens)

	expr := parser0.parse_expression() or { panic('Failed to parse external call') }
	match expr {
		ast.CallExpr {
			match expr.function {
				ast.RecordAccessExpr {
					match expr.function.record {
						ast.VariableExpr {
							assert expr.function.record.name == 'math'
						}
						else { assert false, 'Expected VariableExpr for record' }
					}
					assert expr.function.field == 'pow'
				}
				else { assert false, 'Expected RecordAccessExpr' }
			}
			assert expr.arguments.len == 2
		}
		else { assert false, 'Expected CallExpr' }
	}
}

fn test_parser_error_recovery() {
	tokens := [
		lexer.Token(lexer.IdentToken{
			value: 'x'
		}),
		lexer.Token(lexer.ErrorToken{
			message: 'Unexpected token'
		}),
		lexer.Token(lexer.new_eof_token()),
	]

	mut parser0 := parser.new_main_parser(tokens)
	mut last_pos := parser0.position
	for !parser0.is_at_end() {
		_ = parser0.parse_expression() or { break }
		if parser0.position == last_pos {
			parser0.advance()
		}
		last_pos = parser0.position
	}
	assert parser0.errors.len > 0
}

fn test_record_access_parsing() {
	tokens := [
		lexer.Token(lexer.new_ident_token('person')),
		lexer.Token(lexer.new_operator_token(lexer.OperatorValue.dot)),
		lexer.Token(lexer.new_ident_token('name')),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.lparen)),
		lexer.Token(lexer.new_int_token(1)),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.rparen)),
		lexer.Token(lexer.new_eof_token()),
	]
	mut parser0 := parser.new_main_parser(tokens)
	expr := parser0.parse_expression() or { panic('Failed to parse record access call') }
	match expr {
		ast.CallExpr {
			match expr.function {
				ast.RecordAccessExpr {
					match expr.function.record {
						ast.VariableExpr {
							assert expr.function.record.name == 'person'
						}
						else { assert false, 'Expected VariableExpr for record' }
					}
					assert expr.function.field == 'name'
				}
				else { assert false, 'Expected RecordAccessExpr' }
			}
			assert expr.arguments.len == 1
		}
		else {
			assert false, 'Expected CallExpr'
		}
	}
}
