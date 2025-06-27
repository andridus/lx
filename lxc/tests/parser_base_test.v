module main

import parser
import lexer
import ast

fn test_parser_initialization() {
	tokens := [
		lexer.Token(lexer.IdentToken{value: 'x'}),
		lexer.Token(lexer.OperatorToken.assign),
		lexer.Token(lexer.IntToken{value: 42}),
		lexer.Token(lexer.EOFToken{})
	]

	mut parser0 := parser.new_parser(tokens)

	assert parser0.tokens.len == 4
	assert parser0.position == 0
	assert parser0.current == lexer.Token(lexer.IdentToken{value: 'x'})
	assert parser0.errors.len == 0
}

fn test_token_advancement() {
	tokens := [
		lexer.Token(lexer.IdentToken{value: 'x'}),
		lexer.Token(lexer.OperatorToken.assign),
		lexer.Token(lexer.IntToken{value: 42}),
		lexer.Token(lexer.EOFToken{})
	]

	mut parser0 := parser.new_parser(tokens)

	assert parser0.current == lexer.Token(lexer.IdentToken{value: 'x'})
	parser0.advance()
	assert parser0.position == 1
	assert parser0.current == lexer.Token(lexer.OperatorToken.assign)
	parser0.advance()
	assert parser0.current == lexer.Token(lexer.IntToken{value: 42})
}

fn test_precedence_table() {
	table := parser.new_precedence_table()

	// Test operator precedence
	assert int(table.get_precedence(lexer.OperatorToken.plus)) == int(parser.Precedence.term)
	assert int(table.get_precedence(lexer.OperatorToken.mult)) == int(parser.Precedence.factor)
	assert int(table.get_precedence(lexer.OperatorToken.assign)) == int(parser.Precedence.assignment)
	assert int(table.get_precedence(lexer.OperatorToken.eq)) == int(parser.Precedence.equality)
	assert int(table.get_precedence(lexer.OperatorToken.and_)) == int(parser.Precedence.and_)
}

fn test_parser_error_handling() {
	tokens := [
		lexer.Token(lexer.IdentToken{value: 'x'}),
		lexer.Token(lexer.ErrorToken{message: 'Unexpected token'}),
		lexer.Token(lexer.EOFToken{})
	]

	mut parser0 := parser.new_parser(tokens)
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
		lexer.Token(lexer.IdentToken{value: 'x'}),
		lexer.Token(lexer.ErrorToken{message: 'First error'}),
		lexer.Token(lexer.IdentToken{value: 'y'}),
		lexer.Token(lexer.ErrorToken{message: 'Second error'}),
		lexer.Token(lexer.EOFToken{})
	]

	mut parser0 := parser.new_parser(tokens)
	mut last_pos := parser0.position
	for !parser0.is_at_end() {
		_ = parser0.parse_expression()
		if parser0.position == last_pos {
			parser0.advance()
		}
		last_pos = parser0.position
	}
	assert parser0.errors.len >= 2
}

fn test_parser_error_recovery_with_valid_code_after_error() {
	tokens := [
		lexer.Token(lexer.IdentToken{value: 'x'}),
		lexer.Token(lexer.ErrorToken{message: 'Error in first expression'}),
		lexer.Token(lexer.IdentToken{value: 'y'}),
		lexer.Token(lexer.OperatorToken.assign),
		lexer.Token(lexer.IntToken{value: 42}),
		lexer.Token(lexer.EOFToken{})
	]

	mut parser0 := parser.new_parser(tokens)

	// Parse all statements
	statements := parser0.parse()

	// Deve ter registrado o erro
	assert parser0.errors.len > 0

	// Deve ter conseguido parsear a segunda expressão válida
	assert statements.len > 0
}

fn test_literal_parsing() {
	// Test integer literal
	tokens := [lexer.Token(lexer.IntToken{value: 42}), lexer.Token(lexer.EOFToken{})]
	mut parser0 := parser.new_parser(tokens)
	expr := parser0.parse_expression()

	assert expr is ast.LiteralExpr
	literal_expr := expr as ast.LiteralExpr
	assert literal_expr.value is ast.IntegerLiteral
	int_literal := literal_expr.value as ast.IntegerLiteral
	assert int_literal.value == 42

	// Test string literal
	tokens2 := [lexer.Token(lexer.StringToken{value: 'hello'}), lexer.Token(lexer.EOFToken{})]
	mut parser2 := parser.new_parser(tokens2)
	expr2 := parser2.parse_expression()

	assert expr2 is ast.LiteralExpr
	literal_expr2 := expr2 as ast.LiteralExpr
	assert literal_expr2.value is ast.StringLiteral
	string_literal := literal_expr2.value as ast.StringLiteral
	assert string_literal.value == 'hello'

	// Test boolean literal
	tokens3 := [lexer.Token(lexer.KeywordToken.true_), lexer.Token(lexer.EOFToken{})]
	mut parser3 := parser.new_parser(tokens3)
	expr3 := parser3.parse_expression()

	assert expr3 is ast.LiteralExpr
	literal_expr3 := expr3 as ast.LiteralExpr
	assert literal_expr3.value is ast.BooleanLiteral
	bool_literal := literal_expr3.value as ast.BooleanLiteral
	assert bool_literal.value == true

	// Test atom literal
	tokens4 := [lexer.Token(lexer.AtomToken{value: 'ok'}), lexer.Token(lexer.EOFToken{})]
	mut parser4 := parser.new_parser(tokens4)
	expr4 := parser4.parse_expression()

	assert expr4 is ast.LiteralExpr
	literal_expr4 := expr4 as ast.LiteralExpr
	assert literal_expr4.value is ast.AtomLiteral
	atom_literal := literal_expr4.value as ast.AtomLiteral
	assert atom_literal.value == 'ok'

	// Test nil literal
	tokens5 := [lexer.Token(lexer.KeywordToken.nil_), lexer.Token(lexer.EOFToken{})]
	mut parser5 := parser.new_parser(tokens5)
	expr5 := parser5.parse_expression()

	assert expr5 is ast.LiteralExpr
	literal_expr5 := expr5 as ast.LiteralExpr
	assert literal_expr5.value is ast.NilLiteral
}

fn test_variable_parsing() {
	tokens := [lexer.Token(lexer.IdentToken{value: 'x'}), lexer.Token(lexer.EOFToken{})]
	mut parser0 := parser.new_parser(tokens)

	expr := parser0.parse_expression()
	assert expr is ast.VariableExpr
	var_expr := expr as ast.VariableExpr
	assert var_expr.name == 'x'
}

fn test_assignment_parsing() {
	tokens := [
		lexer.Token(lexer.IdentToken{value: 'x'}),
		lexer.Token(lexer.OperatorToken.assign),
		lexer.Token(lexer.IntToken{value: 42}),
		lexer.Token(lexer.EOFToken{})
	]
	mut parser0 := parser.new_parser(tokens)

	expr := parser0.parse_expression()
	assert expr is ast.AssignExpr
	assign_expr := expr as ast.AssignExpr
	assert assign_expr.name == 'x'
	assert assign_expr.value is ast.LiteralExpr
	value_expr := assign_expr.value as ast.LiteralExpr
	assert value_expr.value is ast.IntegerLiteral
	int_literal := value_expr.value as ast.IntegerLiteral
	assert int_literal.value == 42
}

fn test_function_call_parsing() {
	tokens := [
		lexer.Token(lexer.IdentToken{value: 'sum'}),
		lexer.Token(lexer.PunctuationToken.lparen),
		lexer.Token(lexer.IntToken{value: 3}),
		lexer.Token(lexer.PunctuationToken.comma),
		lexer.Token(lexer.IntToken{value: 4}),
		lexer.Token(lexer.PunctuationToken.rparen),
		lexer.Token(lexer.EOFToken{})
	]
	mut parser0 := parser.new_parser(tokens)

	expr := parser0.parse_expression()
	assert expr is ast.CallExpr
	call_expr := expr as ast.CallExpr
	assert call_expr.function is ast.VariableExpr
	func_expr := call_expr.function as ast.VariableExpr
	assert func_expr.name == 'sum'
	assert call_expr.arguments.len == 2
}

fn test_external_call_parsing() {
	tokens := [
		lexer.Token(lexer.IdentToken{value: 'math'}),
		lexer.Token(lexer.OperatorToken.dot),
		lexer.Token(lexer.IdentToken{value: 'pow'}),
		lexer.Token(lexer.PunctuationToken.lparen),
		lexer.Token(lexer.IntToken{value: 2}),
		lexer.Token(lexer.PunctuationToken.comma),
		lexer.Token(lexer.IntToken{value: 8}),
		lexer.Token(lexer.PunctuationToken.rparen),
		lexer.Token(lexer.EOFToken{})
	]
	mut parser0 := parser.new_parser(tokens)

	expr := parser0.parse_expression()
	assert expr is ast.CallExpr
	call_expr := expr as ast.CallExpr
	assert call_expr.function is ast.RecordAccessExpr
	access_expr := call_expr.function as ast.RecordAccessExpr
	assert access_expr.record is ast.VariableExpr
	record_expr := access_expr.record as ast.VariableExpr
	assert record_expr.name == 'math'
	assert access_expr.field == 'pow'
	assert call_expr.arguments.len == 2
}