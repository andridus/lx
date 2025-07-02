module internal

import ast
import lexer

// parse_simple_expression parses simple expressions without function calls
fn (mut sp StatementParser) parse_simple_expression() ?ast.Expr {
	// Check if this is an atom followed by dot (external function call)
	if sp.current is lexer.AtomToken {
		return sp.parse_external_function_call()
	}

	// For other cases, use the simple atom parser
	mut left := sp.parse_simple_atom()?

	// Parse binary expressions
	for {
		if sp.current !is lexer.OperatorToken {
			break
		}

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
			.and_ {
				op = .and
				should_continue = true
			}
			.or_ {
				op = .or
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

// parse_external_function_call parses external function calls like :module.function(args)
fn (mut sp StatementParser) parse_external_function_call() ?ast.Expr {
	// Parse the atom (module name)
	atom_token := sp.current as lexer.AtomToken
	module_name := atom_token.value
	sp.advance()

	// Expect a dot
	if sp.current !is lexer.OperatorToken {
		sp.add_error('Expected dot after atom', 'Got ${sp.current.str()}')
		return none
	}
	op_token := sp.current as lexer.OperatorToken
	if op_token.value != .dot {
		sp.add_error('Expected dot after atom', 'Got ${sp.current.str()}')
		return none
	}
	sp.advance()

	// Expect an identifier (function name)
	if sp.current !is lexer.IdentToken {
		sp.add_error('Expected function name after dot', 'Got ${sp.current.str()}')
		return none
	}
	func_token := sp.current as lexer.IdentToken
	function_name := func_token.value
	sp.advance()

	// Check for arguments
	mut arguments := []ast.Expr{}
	if sp.current is lexer.PunctuationToken {
		punc_token := sp.current as lexer.PunctuationToken
		if punc_token.value == .lparen {
			sp.advance() // consume '('

			if !sp.check(lexer.punctuation(.rparen)) {
				for {
					arguments << sp.parse_simple_expression()?
					if !sp.match(lexer.punctuation(.comma)) {
						break
					}
				}
			}

			sp.consume(lexer.punctuation(.rparen), 'Expected closing parenthesis')?
		}
	}

	return ast.CallExpr{
		external:      true
		module:        module_name
		function_name: function_name
		arguments:     arguments
		position:      sp.get_current_position()
	}
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
				name:     token.value
				position: ast.new_position(token.position.line, token.position.column,
					token.position.filename)
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
			pos := ast.new_position(token.position.line, token.position.column, token.position.filename)
			ast.LiteralExpr{
				value: ast.StringLiteral{
					value: token.value
					position: pos
				}
				position: pos
			}
		}
		lexer.IntToken {
			token := sp.current as lexer.IntToken
			sp.advance()
			pos := ast.new_position(token.position.line, token.position.column, token.position.filename)
			ast.LiteralExpr{
				value: ast.IntegerLiteral{
					value: token.value
					position: pos
				}
				position: pos
			}
		}
		lexer.FloatToken {
			token := sp.current as lexer.FloatToken
			sp.advance()
			pos := ast.new_position(token.position.line, token.position.column, token.position.filename)
			ast.LiteralExpr{
				value: ast.FloatLiteral{
					value: token.value
					position: pos
				}
				position: pos
			}
		}
		lexer.BoolToken {
			token := sp.current as lexer.BoolToken
			sp.advance()
			pos := ast.new_position(token.position.line, token.position.column, token.position.filename)
			ast.LiteralExpr{
				value: ast.BooleanLiteral{
					value: token.value
					position: pos
				}
				position: pos
			}
		}
		lexer.AtomToken {
			token := sp.current as lexer.AtomToken
			sp.advance()
			pos := ast.new_position(token.position.line, token.position.column, token.position.filename)
			ast.LiteralExpr{
				value: ast.AtomLiteral{
					value: token.value
					position: pos
				}
				position: pos
			}
		}
		lexer.KeywordToken {
			keyword_token := sp.current as lexer.KeywordToken
			match keyword_token.value {
				.nil_ {
					sp.advance()
					pos := ast.new_position(keyword_token.position.line, keyword_token.position.column, keyword_token.position.filename)
					ast.LiteralExpr{
						value: ast.NilLiteral{
							position: pos
						}
						position: pos
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
