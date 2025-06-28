module main

import parser
import lexer
import ast

fn test_arithmetic_operators() {
	// Test addition
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
	mut parser1 := parser.new_main_parser(tokens1)
	expr1 := parser1.parse_expression() or { panic('Failed to parse addition') }
	match expr1 {
		ast.BinaryExpr {
			assert expr1.op == ast.BinaryOp.add
		}
		else {
			panic('Expected BinaryExpr')
		}
	}

	// Test subtraction
	tokens2 := [
		lexer.Token(lexer.IntToken{
			value: 5
		}),
		lexer.Token(lexer.OperatorToken.minus),
		lexer.Token(lexer.IntToken{
			value: 3
		}),
		lexer.Token(lexer.EOFToken{}),
	]
	mut parser2 := parser.new_main_parser(tokens2)
	expr2 := parser2.parse_expression() or { panic('Failed to parse subtraction') }
	match expr2 {
		ast.BinaryExpr {
			assert expr2.op == ast.BinaryOp.subtract
		}
		else {
			panic('Expected BinaryExpr')
		}
	}

	// Test multiplication
	tokens3 := [
		lexer.Token(lexer.IntToken{
			value: 4
		}),
		lexer.Token(lexer.OperatorToken.mult),
		lexer.Token(lexer.IntToken{
			value: 6
		}),
		lexer.Token(lexer.EOFToken{}),
	]
	mut parser3 := parser.new_main_parser(tokens3)
	expr3 := parser3.parse_expression() or { panic('Failed to parse multiplication') }
	match expr3 {
		ast.BinaryExpr {
			assert expr3.op == ast.BinaryOp.multiply
		}
		else {
			panic('Expected BinaryExpr')
		}
	}

	// Test division
	tokens4 := [
		lexer.Token(lexer.IntToken{
			value: 10
		}),
		lexer.Token(lexer.OperatorToken.div),
		lexer.Token(lexer.IntToken{
			value: 2
		}),
		lexer.Token(lexer.EOFToken{}),
	]
	mut parser4 := parser.new_main_parser(tokens4)
	expr4 := parser4.parse_expression() or { panic('Failed to parse division') }
	match expr4 {
		ast.BinaryExpr {
			assert expr4.op == ast.BinaryOp.divide
		}
		else {
			panic('Expected BinaryExpr')
		}
	}
}

fn test_comparison_operators() {
	// Test equality
	tokens1 := [
		lexer.Token(lexer.IntToken{
			value: 1
		}),
		lexer.Token(lexer.OperatorToken.eq),
		lexer.Token(lexer.IntToken{
			value: 1
		}),
		lexer.Token(lexer.EOFToken{}),
	]
	mut parser1 := parser.new_main_parser(tokens1)
	expr1 := parser1.parse_expression() or { panic('Failed to parse equality') }
	match expr1 {
		ast.BinaryExpr {
			assert expr1.op == ast.BinaryOp.equal
		}
		else {
			panic('Expected BinaryExpr')
		}
	}

	// Test inequality
	tokens2 := [
		lexer.Token(lexer.IntToken{
			value: 1
		}),
		lexer.Token(lexer.OperatorToken.neq),
		lexer.Token(lexer.IntToken{
			value: 2
		}),
		lexer.Token(lexer.EOFToken{}),
	]
	mut parser2 := parser.new_main_parser(tokens2)
	expr2 := parser2.parse_expression() or { panic('Failed to parse inequality') }
	match expr2 {
		ast.BinaryExpr {
			assert expr2.op == ast.BinaryOp.not_equal
		}
		else {
			panic('Expected BinaryExpr')
		}
	}

	// Test less than
	tokens3 := [
		lexer.Token(lexer.IntToken{
			value: 1
		}),
		lexer.Token(lexer.OperatorToken.lt),
		lexer.Token(lexer.IntToken{
			value: 2
		}),
		lexer.Token(lexer.EOFToken{}),
	]
	mut parser3 := parser.new_main_parser(tokens3)
	expr3 := parser3.parse_expression() or { panic('Failed to parse less than') }
	match expr3 {
		ast.BinaryExpr {
			assert expr3.op == ast.BinaryOp.less_than
		}
		else {
			panic('Expected BinaryExpr')
		}
	}
}

fn test_logical_operators() {
	// Test AND
	tokens1 := [
		lexer.Token(lexer.BoolToken{
			value: true
		}),
		lexer.Token(lexer.OperatorToken.and_),
		lexer.Token(lexer.BoolToken{
			value: false
		}),
		lexer.Token(lexer.EOFToken{}),
	]
	mut parser1 := parser.new_main_parser(tokens1)
	expr1 := parser1.parse_expression() or { panic('Failed to parse AND') }
	match expr1 {
		ast.BinaryExpr {
			assert expr1.op == ast.BinaryOp.and
		}
		else {
			panic('Expected BinaryExpr')
		}
	}

	// Test OR
	tokens2 := [
		lexer.Token(lexer.BoolToken{
			value: true
		}),
		lexer.Token(lexer.OperatorToken.or_),
		lexer.Token(lexer.BoolToken{
			value: false
		}),
		lexer.Token(lexer.EOFToken{}),
	]
	mut parser2 := parser.new_main_parser(tokens2)
	expr2 := parser2.parse_expression() or { panic('Failed to parse OR') }
	match expr2 {
		ast.BinaryExpr {
			assert expr2.op == ast.BinaryOp.or
		}
		else {
			panic('Expected BinaryExpr')
		}
	}

	// Test complex logical expression
	tokens3 := [
		lexer.Token(lexer.BoolToken{
			value: true
		}),
		lexer.Token(lexer.OperatorToken.and_),
		lexer.Token(lexer.BoolToken{
			value: false
		}),
		lexer.Token(lexer.OperatorToken.or_),
		lexer.Token(lexer.BoolToken{
			value: true
		}),
		lexer.Token(lexer.EOFToken{}),
	]
	mut parser3 := parser.new_main_parser(tokens3)
	expr3 := parser3.parse_expression() or { panic('Failed to parse complex logical expression') }
	match expr3 {
		ast.BinaryExpr {
			assert expr3.op == ast.BinaryOp.or
		}
		else {
			panic('Expected BinaryExpr')
		}
	}
}

fn test_operator_precedence() {
	// Test arithmetic precedence: 1 + 2 * 3 == 7
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

	mut parser0 := parser.new_main_parser(tokens)

	expr := parser0.parse_expression() or { panic('Failed to parse precedence test') }
	match expr {
		ast.BinaryExpr {
			// Deve ser interpretado como: (1 + (2 * 3)) == 7
			assert expr.op == ast.BinaryOp.equal
			match expr.right {
				ast.LiteralExpr {
					match expr.right.value {
						ast.IntegerLiteral { assert expr.right.value.value == 7 }
						else { panic('Expected IntegerLiteral 7') }
					}
				}
				else {
					panic('Expected LiteralExpr for 7')
				}
			}

			// Verificar que o lado esquerdo é uma expressão binária
			match expr.left {
				ast.BinaryExpr {
					assert expr.left.op == ast.BinaryOp.add
					// Verificar que o lado direito da adição é uma multiplicação
					match expr.left.right {
						ast.BinaryExpr {
							assert expr.left.right.op == ast.BinaryOp.multiply
						}
						else {
							panic('Expected BinaryExpr for multiplication')
						}
					}
				}
				else {
					panic('Expected BinaryExpr for addition')
				}
			}
		}
		else {
			panic('Expected BinaryExpr')
		}
	}
}

fn test_associativity() {
	// Test left associativity: 1 - 2 - 3
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

	mut parser0 := parser.new_main_parser(tokens)

	expr := parser0.parse_expression() or { panic('Failed to parse associativity test') }
	match expr {
		ast.BinaryExpr {
			// Deve ser interpretado como: (1 - 2) - 3
			assert expr.op == ast.BinaryOp.subtract
			match expr.right {
				ast.LiteralExpr {
					match expr.right.value {
						ast.IntegerLiteral { assert expr.right.value.value == 3 }
						else { panic('Expected IntegerLiteral 3') }
					}
				}
				else {
					panic('Expected LiteralExpr for 3')
				}
			}

			// Verificar que o lado esquerdo é uma subtração
			match expr.left {
				ast.BinaryExpr {
					assert expr.left.op == ast.BinaryOp.subtract
					match expr.left.left {
						ast.LiteralExpr {
							match expr.left.left.value {
								ast.IntegerLiteral { assert expr.left.left.value.value == 1 }
								else { panic('Expected IntegerLiteral 1') }
							}
						}
						else {
							panic('Expected LiteralExpr for 1')
						}
					}
					match expr.left.right {
						ast.LiteralExpr {
							match expr.left.right.value {
								ast.IntegerLiteral { assert expr.left.right.value.value == 2 }
								else { panic('Expected IntegerLiteral 2') }
							}
						}
						else {
							panic('Expected LiteralExpr for 2')
						}
					}
				}
				else {
					panic('Expected BinaryExpr for first subtraction')
				}
			}
		}
		else {
			panic('Expected BinaryExpr')
		}
	}
}

fn test_complex_expression_precedence() {
	// Test complex expression: a + b * c == d and e or f
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

	mut parser0 := parser.new_main_parser(tokens)

	expr := parser0.parse_expression() or { panic('Failed to parse complex precedence test') }
	match expr {
		ast.BinaryExpr {
			// Deve ser interpretado como: ((a + (b * c)) == d) and (e or f)
			// O operador de mais alta precedência é or, então a estrutura deve ser:
			// (left) or f
			assert expr.op == ast.BinaryOp.or
			match expr.right {
				ast.VariableExpr {
					assert expr.right.name == 'f'
				}
				else {
					panic('Expected VariableExpr for f')
				}
			}

			// Verificar o lado esquerdo: ((a + (b * c)) == d) and e
			match expr.left {
				ast.BinaryExpr {
					assert expr.left.op == ast.BinaryOp.and
					match expr.left.right {
						ast.VariableExpr {
							assert expr.left.right.name == 'e'
						}
						else {
							panic('Expected VariableExpr for e')
						}
					}

					// Verificar o lado esquerdo da and: (a + (b * c)) == d
					match expr.left.left {
						ast.BinaryExpr {
							assert expr.left.left.op == ast.BinaryOp.equal
							match expr.left.left.right {
								ast.VariableExpr {
									assert expr.left.left.right.name == 'd'
								}
								else {
									panic('Expected VariableExpr for d')
								}
							}

							// Verificar o lado esquerdo da equal: a + (b * c)
							match expr.left.left.left {
								ast.BinaryExpr {
									assert expr.left.left.left.op == ast.BinaryOp.add
									match expr.left.left.left.left {
										ast.VariableExpr {
											assert expr.left.left.left.left.name == 'a'
										}
										else {
											panic('Expected VariableExpr for a')
										}
									}

									// Verificar o lado direito da plus: b * c
									match expr.left.left.left.right {
										ast.BinaryExpr {
											assert expr.left.left.left.right.op == ast.BinaryOp.multiply
											match expr.left.left.left.right.left {
												ast.VariableExpr {
													assert expr.left.left.left.right.left.name == 'b'
												}
												else {
													panic('Expected VariableExpr for b')
												}
											}
											match expr.left.left.left.right.right {
												ast.VariableExpr {
													assert expr.left.left.left.right.right.name == 'c'
												}
												else {
													panic('Expected VariableExpr for c')
												}
											}
										}
										else {
											panic('Expected BinaryExpr for b * c')
										}
									}
								}
								else {
									panic('Expected BinaryExpr for a + (b * c)')
								}
							}
						}
						else {
							panic('Expected BinaryExpr for (a + (b * c)) == d')
						}
					}
				}
				else {
					panic('Expected BinaryExpr for ((a + (b * c)) == d) and e')
				}
			}
		}
		else {
			panic('Expected BinaryExpr')
		}
	}
}
