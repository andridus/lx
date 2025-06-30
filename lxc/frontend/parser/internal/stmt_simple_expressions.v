module internal

import ast
import lexer

// parse_simple_expression parses simple expressions without function calls
// This prevents the parser from consuming tokens from the next clause
fn (mut sp StatementParser) parse_simple_expression() ?ast.Expr {
	mut left := sp.parse_simple_atom()?

	// Check for binary operators
	for sp.current is lexer.OperatorToken {
		op_token := sp.current as lexer.OperatorToken
		mut op := ast.BinaryOp.add
		mut should_continue := false

		match op_token.value {
			.plus {
				op = .add
				should_continue = true
			}
			.minus {
				op = .subtract
				should_continue = true
			}
			.mult {
				op = .multiply
				should_continue = true
			}
			.div {
				op = .divide
				should_continue = true
			}
			.gt {
				op = .greater_than
				should_continue = true
			}
			.lt {
				op = .less_than
				should_continue = true
			}
			.geq {
				op = .greater_equal
				should_continue = true
			}
			.leq {
				op = .less_equal
				should_continue = true
			}
			.eq {
				op = .equal
				should_continue = true
			}
			.neq {
				op = .not_equal
				should_continue = true
			}
			else {
				break
			}
		}

		if !should_continue {
			break
		}

		sp.advance() // consume operator
		right := sp.parse_simple_atom()?

		left = ast.BinaryExpr{
			left:     left
			op:       op
			right:    right
			position: sp.get_current_position()
		}
	}

	return left
}

// parse_simple_atom parses atomic expressions (identifiers, literals)
fn (mut sp StatementParser) parse_simple_atom() ?ast.Expr {
	// Skip newlines before atom
	for sp.current is lexer.NewlineToken {
		sp.advance()
	}

	return match sp.current {
		lexer.IdentToken {
			token := sp.current as lexer.IdentToken
			sp.safe_advance()
			mut expr := ast.Expr(ast.VariableExpr{
				name: token.value
			})
			// Allow both record access and function calls
			for {
				match sp.current() {
					lexer.OperatorToken {
						op_token := sp.current() as lexer.OperatorToken
						// Stop if we encounter -> (marks end of clause body)
						if op_token.value == .arrow {
							break
						}
						if op_token.value == .dot {
							sp.safe_advance()
							match sp.current() {
								lexer.IdentToken {
									field_token := sp.current() as lexer.IdentToken
									sp.safe_advance()
									expr = ast.Expr(ast.RecordAccessExpr{
										record:   expr
										field:    field_token.value
										position: sp.get_current_position()
									})
									continue
								}
								else {
									sp.add_error('Expected field name after dot', 'Got ${sp.current().str()}')
									return none
								}
							}
						} else {
							break
						}
					}
					lexer.PunctuationToken {
						punc_token := sp.current() as lexer.PunctuationToken
						if punc_token.value == .lparen {
							if sp.is_potential_new_clause_start() {
								break
							}
							sp.safe_advance()
							mut arguments := []ast.Expr{}
							if !sp.check(lexer.punctuation(.rparen)) {
								for {
									arguments << sp.parse_simple_expression()?
									if !sp.match(lexer.punctuation(.comma)) {
										break
									}
								}
							}
							sp.consume(lexer.punctuation(.rparen), 'Expected closing parenthesis')?
							expr = ast.Expr(ast.CallExpr{
								function:  expr
								arguments: arguments
								position:  sp.get_current_position()
							})
							continue
						} else {
							break
						}
					}
					else {
						break
					}
				}
			}
			expr
		}
		lexer.StringToken {
			token := sp.current as lexer.StringToken
			sp.advance()
			ast.LiteralExpr{
				value: ast.StringLiteral{
					value: token.value
				}
			}
		}
		lexer.IntToken {
			token := sp.current as lexer.IntToken
			sp.advance()
			ast.LiteralExpr{
				value: ast.IntegerLiteral{
					value: token.value
				}
			}
		}
		lexer.BoolToken {
			token := sp.current as lexer.BoolToken
			sp.advance()
			ast.LiteralExpr{
				value: ast.BooleanLiteral{
					value: token.value
				}
			}
		}
		lexer.AtomToken {
			token := sp.current as lexer.AtomToken
			sp.advance()
			ast.LiteralExpr{
				value: ast.AtomLiteral{
					value: token.value
				}
			}
		}
		lexer.KeywordToken {
			keyword_token := sp.current as lexer.KeywordToken
			match keyword_token.value {
				.nil_ {
					sp.advance()
					ast.LiteralExpr{
						value: ast.NilLiteral{}
					}
				}
				else {
					sp.add_error('Expected simple expression', 'Got ${sp.current.str()}')
					none
				}
			}
		}
		else {
			sp.add_error('Expected simple expression', 'Got ${sp.current.str()}')
			none
		}
	}
}
