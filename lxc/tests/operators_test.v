module main

import parser
import lexer
import ast

fn test_arithmetic_operators() {
	// Teste de adição
	tokens1 := [
		lexer.Token(lexer.IntToken{
			value: 1
		}),
		lexer.Token(lexer.OperatorToken.plus),
		lexer.Token(lexer.IntToken{
			value: 2
		}),
		lexer.Token(lexer.EOFToken{}),
	]
	mut parser1 := parser.new_parser(tokens1)
	expr1 := parser1.parse_expression()
	assert expr1 is ast.BinaryExpr
	bin_expr1 := expr1 as ast.BinaryExpr
	assert bin_expr1.op == ast.BinaryOp.add

	// Teste de subtração
	tokens2 := [
		lexer.Token(lexer.IntToken{
			value: 3
		}),
		lexer.Token(lexer.OperatorToken.minus),
		lexer.Token(lexer.IntToken{
			value: 4
		}),
		lexer.Token(lexer.EOFToken{}),
	]
	mut parser2 := parser.new_parser(tokens2)
	expr2 := parser2.parse_expression()
	assert expr2 is ast.BinaryExpr
	bin_expr2 := expr2 as ast.BinaryExpr
	assert bin_expr2.op == ast.BinaryOp.subtract

	// Teste de multiplicação
	tokens3 := [
		lexer.Token(lexer.IntToken{
			value: 5
		}),
		lexer.Token(lexer.OperatorToken.mult),
		lexer.Token(lexer.IntToken{
			value: 6
		}),
		lexer.Token(lexer.EOFToken{}),
	]
	mut parser3 := parser.new_parser(tokens3)
	expr3 := parser3.parse_expression()
	assert expr3 is ast.BinaryExpr
	bin_expr3 := expr3 as ast.BinaryExpr
	assert bin_expr3.op == ast.BinaryOp.multiply

	// Teste de divisão
	tokens4 := [
		lexer.Token(lexer.IntToken{
			value: 7
		}),
		lexer.Token(lexer.OperatorToken.div),
		lexer.Token(lexer.IntToken{
			value: 8
		}),
		lexer.Token(lexer.EOFToken{}),
	]
	mut parser4 := parser.new_parser(tokens4)
	expr4 := parser4.parse_expression()
	assert expr4 is ast.BinaryExpr
	bin_expr4 := expr4 as ast.BinaryExpr
	assert bin_expr4.op == ast.BinaryOp.divide
}

fn test_comparison_operators() {
	// Teste de igualdade
	tokens1 := [
		lexer.Token(lexer.IntToken{
			value: 1
		}),
		lexer.Token(lexer.OperatorToken.eq),
		lexer.Token(lexer.IntToken{
			value: 2
		}),
		lexer.Token(lexer.EOFToken{}),
	]
	mut parser1 := parser.new_parser(tokens1)
	expr1 := parser1.parse_expression()
	assert expr1 is ast.BinaryExpr
	bin_expr1 := expr1 as ast.BinaryExpr
	assert bin_expr1.op == ast.BinaryOp.equal

	// Teste de desigualdade
	tokens2 := [
		lexer.Token(lexer.IntToken{
			value: 3
		}),
		lexer.Token(lexer.OperatorToken.neq),
		lexer.Token(lexer.IntToken{
			value: 4
		}),
		lexer.Token(lexer.EOFToken{}),
	]
	mut parser2 := parser.new_parser(tokens2)
	expr2 := parser2.parse_expression()
	assert expr2 is ast.BinaryExpr
	bin_expr2 := expr2 as ast.BinaryExpr
	assert bin_expr2.op == ast.BinaryOp.not_equal

	// Teste de menor que
	tokens3 := [
		lexer.Token(lexer.IntToken{
			value: 5
		}),
		lexer.Token(lexer.OperatorToken.lt),
		lexer.Token(lexer.IntToken{
			value: 6
		}),
		lexer.Token(lexer.EOFToken{}),
	]
	mut parser3 := parser.new_parser(tokens3)
	expr3 := parser3.parse_expression()
	assert expr3 is ast.BinaryExpr
	bin_expr3 := expr3 as ast.BinaryExpr
	assert bin_expr3.op == ast.BinaryOp.less_than
}

fn test_logical_operators() {
	// Teste de AND
	tokens1 := [
		lexer.Token(lexer.KeywordToken.true_),
		lexer.Token(lexer.OperatorToken.and_),
		lexer.Token(lexer.KeywordToken.false_),
		lexer.Token(lexer.EOFToken{}),
	]
	mut parser1 := parser.new_parser(tokens1)
	expr1 := parser1.parse_expression()
	assert expr1 is ast.BinaryExpr
	bin_expr1 := expr1 as ast.BinaryExpr
	assert bin_expr1.op == ast.BinaryOp.and

	// Teste de OR
	tokens2 := [
		lexer.Token(lexer.KeywordToken.true_),
		lexer.Token(lexer.OperatorToken.or_),
		lexer.Token(lexer.KeywordToken.false_),
		lexer.Token(lexer.EOFToken{}),
	]
	mut parser2 := parser.new_parser(tokens2)
	expr2 := parser2.parse_expression()
	assert expr2 is ast.BinaryExpr
	bin_expr2 := expr2 as ast.BinaryExpr
	assert bin_expr2.op == ast.BinaryOp.or

	// Teste de ANDALSO
	tokens3 := [
		lexer.Token(lexer.KeywordToken.true_),
		lexer.Token(lexer.OperatorToken.andalso),
		lexer.Token(lexer.KeywordToken.false_),
		lexer.Token(lexer.EOFToken{}),
	]
	mut parser3 := parser.new_parser(tokens3)
	expr3 := parser3.parse_expression()
	assert expr3 is ast.BinaryExpr
	bin_expr3 := expr3 as ast.BinaryExpr
	assert bin_expr3.op == ast.BinaryOp.and
}

fn test_operator_precedence() {
	// Testar precedência: * > + > ==
	// 1 + 2 * 3 == 7 deve ser interpretado como: (1 + (2 * 3)) == 7
	tokens := [
		lexer.Token(lexer.IntToken{
			value: 1
		}),
		lexer.Token(lexer.OperatorToken.plus),
		lexer.Token(lexer.IntToken{
			value: 2
		}),
		lexer.Token(lexer.OperatorToken.mult),
		lexer.Token(lexer.IntToken{
			value: 3
		}),
		lexer.Token(lexer.OperatorToken.eq),
		lexer.Token(lexer.IntToken{
			value: 7
		}),
		lexer.Token(lexer.EOFToken{}),
	]
	mut parser0 := parser.new_parser(tokens)

	expr := parser0.parse_expression()
	assert expr is ast.BinaryExpr

	// Deve ser interpretado como: (1 + (2 * 3)) == 7
	bin_op := expr as ast.BinaryExpr
	assert bin_op.op == ast.BinaryOp.equal
	assert bin_op.right is ast.LiteralExpr
	right := bin_op.right as ast.LiteralExpr
	assert right.value is ast.IntegerLiteral
	right_int := right.value as ast.IntegerLiteral
	assert right_int.value == 7

	// Verificar lado esquerdo: (1 + (2 * 3))
	assert bin_op.left is ast.BinaryExpr
	left_bin_op := bin_op.left as ast.BinaryExpr
	assert left_bin_op.op == ast.BinaryOp.add
	assert left_bin_op.left is ast.LiteralExpr
	left_left := left_bin_op.left as ast.LiteralExpr
	assert left_left.value is ast.IntegerLiteral
	left_left_int := left_left.value as ast.IntegerLiteral
	assert left_left_int.value == 1

	// Verificar multiplicação: (2 * 3)
	assert left_bin_op.right is ast.BinaryExpr
	mult_bin_op := left_bin_op.right as ast.BinaryExpr
	assert mult_bin_op.op == ast.BinaryOp.multiply
	assert mult_bin_op.left is ast.LiteralExpr
	mult_left := mult_bin_op.left as ast.LiteralExpr
	assert mult_left.value is ast.IntegerLiteral
	mult_left_int := mult_left.value as ast.IntegerLiteral
	assert mult_left_int.value == 2
	assert mult_bin_op.right is ast.LiteralExpr
	mult_right := mult_bin_op.right as ast.LiteralExpr
	assert mult_right.value is ast.IntegerLiteral
	mult_right_int := mult_right.value as ast.IntegerLiteral
	assert mult_right_int.value == 3
}

fn test_associativity() {
	// Testar associatividade à esquerda
	// 1 - 2 - 3 deve ser interpretado como: (1 - 2) - 3
	tokens := [
		lexer.Token(lexer.IntToken{
			value: 1
		}),
		lexer.Token(lexer.OperatorToken.minus),
		lexer.Token(lexer.IntToken{
			value: 2
		}),
		lexer.Token(lexer.OperatorToken.minus),
		lexer.Token(lexer.IntToken{
			value: 3
		}),
		lexer.Token(lexer.EOFToken{}),
	]
	mut parser0 := parser.new_parser(tokens)

	expr := parser0.parse_expression()
	assert expr is ast.BinaryExpr

	// Deve ser interpretado como: (1 - 2) - 3
	bin_op := expr as ast.BinaryExpr
	assert bin_op.op == ast.BinaryOp.subtract
	assert bin_op.right is ast.LiteralExpr
	right := bin_op.right as ast.LiteralExpr
	assert right.value is ast.IntegerLiteral
	right_int := right.value as ast.IntegerLiteral
	assert right_int.value == 3

	// Verificar lado esquerdo: (1 - 2)
	assert bin_op.left is ast.BinaryExpr
	left_bin_op := bin_op.left as ast.BinaryExpr
	assert left_bin_op.op == ast.BinaryOp.subtract
	assert left_bin_op.left is ast.LiteralExpr
	left_left := left_bin_op.left as ast.LiteralExpr
	assert left_left.value is ast.IntegerLiteral
	left_left_int := left_left.value as ast.IntegerLiteral
	assert left_left_int.value == 1
	assert left_bin_op.right is ast.LiteralExpr
	left_right := left_bin_op.right as ast.LiteralExpr
	assert left_right.value is ast.IntegerLiteral
	left_right_int := left_right.value as ast.IntegerLiteral
	assert left_right_int.value == 2
}

fn test_complex_expression() {
	// Testar expressão complexa: a + b * c == d and e or f
	tokens := [
		lexer.Token(lexer.IdentToken{
			value: 'a'
		}),
		lexer.Token(lexer.OperatorToken.plus),
		lexer.Token(lexer.IdentToken{
			value: 'b'
		}),
		lexer.Token(lexer.OperatorToken.mult),
		lexer.Token(lexer.IdentToken{
			value: 'c'
		}),
		lexer.Token(lexer.OperatorToken.eq),
		lexer.Token(lexer.IdentToken{
			value: 'd'
		}),
		lexer.Token(lexer.OperatorToken.and_),
		lexer.Token(lexer.IdentToken{
			value: 'e'
		}),
		lexer.Token(lexer.OperatorToken.or_),
		lexer.Token(lexer.IdentToken{
			value: 'f'
		}),
		lexer.Token(lexer.EOFToken{}),
	]
	mut parser0 := parser.new_parser(tokens)

	expr := parser0.parse_expression()
	assert expr is ast.BinaryExpr

	// Deve ser interpretado como: ((a + (b * c)) == d) and (e or f)
	// O operador de mais alta precedência é or, então a estrutura deve ser:
	// (left) or f
	bin_op := expr as ast.BinaryExpr
	assert bin_op.op == ast.BinaryOp.or
	assert bin_op.right is ast.VariableExpr
	right := bin_op.right as ast.VariableExpr
	assert right.name == 'f'
}
