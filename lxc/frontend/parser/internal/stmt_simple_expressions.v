module internal

import ast
import lexer

// parse_simple_expression parses simple expressions without function calls
fn (mut sp StatementParser) parse_simple_expression() ?ast.Expr {
	// Check for unary operators first
	if sp.current is lexer.OperatorToken {
		op_token := sp.current as lexer.OperatorToken
		match op_token.value {
			.minus {
				sp.advance() // consume '-'
				operand := sp.parse_simple_expression()?
				return ast.UnaryExpr{
					op:       ast.UnaryOp.minus
					operand:  operand
					position: sp.get_current_position()
				}
			}
			.not_ {
				sp.advance() // consume 'not'
				operand := sp.parse_simple_expression()?
				return ast.UnaryExpr{
					op:       ast.UnaryOp.not
					operand:  operand
					position: sp.get_current_position()
				}
			}
			else {
				// Not a unary operator, continue with atom
			}
		}
	}

	// For all cases, use the simple atom parser first
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
			.concat {
				op = .append
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
			.send {
				// Handle send operator specially
				sp.advance() // consume '!'
				right := sp.parse_simple_atom()?

				return ast.SendExpr{
					pid:      left
					message:  right
					position: sp.get_current_position()
				}
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
		function:      ast.LiteralExpr{} // Dummy value for external calls
		external:      true
		module:        module_name
		function_name: function_name
		arguments:     arguments
		position:      sp.get_current_position()
	}
}

// parse_list_expression parses list expressions in statement context
fn (mut sp StatementParser) parse_list_expression() ?ast.Expr {
	sp.advance() // consume '['

	mut elements := []ast.Expr{}
	if !sp.check(lexer.punctuation(.rbracket)) {
		for {
			elements << sp.parse_simple_expression()?
			if !sp.match(lexer.punctuation(.comma)) {
				break
			}
		}
	}

	sp.consume(lexer.punctuation(.rbracket), 'Expected closing bracket')?

	return ast.ListLiteralExpr{
		elements: elements
		position: sp.get_current_position()
	}
}

// parse_parenthesized_expression parses parenthesized expressions in statement context
fn (mut sp StatementParser) parse_parenthesized_expression() ?ast.Expr {
	sp.advance() // consume '('
	expr := sp.parse_simple_expression()?
	sp.consume(lexer.punctuation(.rparen), 'Expected closing parenthesis')?

	return expr
}

// parse_simple_atom parses atomic expressions (identifiers, literals)
fn (mut sp StatementParser) parse_simple_atom() ?ast.Expr {
	// Skip newlines before atom
	for sp.current is lexer.NewlineToken {
		sp.advance()
	}

	if sp.current is lexer.UpperIdentToken && sp.peek() is lexer.PunctuationToken {
		punc := sp.peek() as lexer.PunctuationToken
		if punc.value == .lbrace {
			mut expr_parser := new_expression_parser(sp.tokens)
			expr_parser.position = sp.position
			expr_parser.current = sp.current
			record_expr := expr_parser.parse_record_value_expression()?
			sp.position = expr_parser.position
			sp.sync_current_token()
			return record_expr
		}
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
						} else if punc_token.value == .lbracket {
							// Map access: map[key]
							sp.safe_advance() // consume '['
							key := sp.parse_simple_expression()?
							sp.consume(lexer.punctuation(.rbracket), 'Expected ]')?
							expr = ast.Expr(ast.MapAccessExpr{
								map_expr: expr
								key:      key
								position: sp.get_current_position()
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
				value:    ast.StringLiteral{
					value:    token.value
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
				value:    ast.IntegerLiteral{
					value:    token.value
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
				value:    ast.FloatLiteral{
					value:    token.value
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
				value:    ast.BooleanLiteral{
					value:    token.value
					position: pos
				}
				position: pos
			}
		}
		lexer.AtomToken {
			token := sp.current as lexer.AtomToken
			// Check if this is an external function call (:module.function)
			if sp.peek() is lexer.OperatorToken {
				peek_token := sp.peek() as lexer.OperatorToken
				if peek_token.value == .dot {
					// This is an external function call, delegate to parse_external_function_call
					return sp.parse_external_function_call()
				}
			}
			// Regular atom literal
			sp.advance()
			pos := ast.new_position(token.position.line, token.position.column, token.position.filename)
			ast.LiteralExpr{
				value:    ast.AtomLiteral{
					value:    token.value
					position: pos
				}
				position: pos
			}
		}
		lexer.KeyToken {
			token := sp.current as lexer.KeyToken
			sp.advance()
			pos := ast.new_position(token.position.line, token.position.column, token.position.filename)
			ast.LiteralExpr{
				value:    ast.AtomLiteral{
					value:    token.value
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
					pos := ast.new_position(keyword_token.position.line, keyword_token.position.column,
						keyword_token.position.filename)
					ast.LiteralExpr{
						value:    ast.NilLiteral{
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
		lexer.OperatorToken {
			op_token := sp.current as lexer.OperatorToken
			match op_token.value {
				.modulo {
					// This is a map expression %{...}
					sp.parse_map_expression()
				}
				else {
					sp.add_error('Expected simple expression', 'Got ${sp.current.str()}')
					none
				}
			}
		}
		lexer.PunctuationToken {
			punc_token := sp.current as lexer.PunctuationToken
			match punc_token.value {
				.lbrace {
					sp.parse_tuple_expression()
				}
				.lbracket {
					sp.parse_list_expression()
				}
				.lparen {
					sp.parse_parenthesized_expression()
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

// is_map_update_syntax checks if this is a map update syntax: %{base_map | ...}
fn (sp StatementParser) is_map_update_syntax() bool {
	mut pos := sp.position

	// Look ahead to find a pipe operator before any colon, fat_arrow, or arrow
	for pos < sp.tokens.len {
		token := sp.tokens[pos]

		if token is lexer.OperatorToken {
			op_token := token as lexer.OperatorToken
			if op_token.value == .pipe {
				return true
			}
			if op_token.value == .fat_arrow || op_token.value == .arrow {
				return false
			}
		}

		if token is lexer.KeyToken {
			return false
		}

		if token is lexer.PunctuationToken {
			punc_token := token as lexer.PunctuationToken
			if punc_token.value == .rbrace {
				return false
			}
		}

		pos++
	}

	return false
}

// parse_map_expression parses map expressions in statement context
fn (mut sp StatementParser) parse_map_expression() ?ast.Expr {
	sp.advance() // consume '%'
	sp.consume(lexer.punctuation(.lbrace), 'Expected opening brace after %')?

	mut entries := []ast.MapEntry{}
	mut base_map := ast.Expr(ast.LiteralExpr{})

	if !sp.check(lexer.punctuation(.rbrace)) {
		// Check for map update syntax first: %{base_map | key: value, ...}
		// Look ahead to see if this is a map update
		if sp.is_map_update_syntax() {
			// Parse base map
			base_map = sp.parse_simple_expression()?
			sp.consume(lexer.operator(.pipe), 'Expected | after base map')?

			// Parse entries after the pipe
			if !sp.check(lexer.punctuation(.rbrace)) {
				for {
					// Check if we have a key token (identifier:)
					if sp.current.is_key() {
						key_value := sp.current.get_key_value()
						sp.advance() // consume key token

						// Create an atom literal for the key
						key := ast.LiteralExpr{
							value:    ast.AtomLiteral{
								value: key_value
							}
							position: sp.get_current_position()
						}

						value := sp.parse_simple_expression()?
						entries << ast.MapEntry{
							key:      key
							value:    value
							position: sp.get_current_position()
						}
					} else {
						// Parse regular key expression
						key := sp.parse_simple_expression()?

						// Check for fat_arrow or arrow (general key)
						if sp.match(lexer.operator(.fat_arrow)) {
							value := sp.parse_simple_expression()?
							entries << ast.MapEntry{
								key:      key
								value:    value
								position: sp.get_current_position()
							}
						} else if sp.match(lexer.operator(.arrow)) {
							value := sp.parse_simple_expression()?
							entries << ast.MapEntry{
								key:      key
								value:    value
								position: sp.get_current_position()
							}
						} else {
							sp.add_error('Expected => or -> in map entry', 'Got ${sp.current.str()}')
							return none
						}
					}

					if !sp.match(lexer.punctuation(.comma)) {
						break
					}
				}
			}
		} else {
			// Regular map literal: %{key: value, key2: value2}
			for {
				// Check if we have a key token (identifier:)
				if sp.current.is_key() {
					key_value := sp.current.get_key_value()
					sp.advance() // consume key token

					// Create an atom literal for the key
					key := ast.LiteralExpr{
						value:    ast.AtomLiteral{
							value: key_value
						}
						position: sp.get_current_position()
					}

					value := sp.parse_simple_expression()?
					entries << ast.MapEntry{
						key:      key
						value:    value
						position: sp.get_current_position()
					}
				} else {
					// Parse regular key expression
					key := sp.parse_simple_expression()?

					// Check for fat_arrow or arrow (general key)
					if sp.match(lexer.operator(.fat_arrow)) {
						value := sp.parse_simple_expression()?
						entries << ast.MapEntry{
							key:      key
							value:    value
							position: sp.get_current_position()
						}
					} else if sp.match(lexer.operator(.arrow)) {
						value := sp.parse_simple_expression()?
						entries << ast.MapEntry{
							key:      key
							value:    value
							position: sp.get_current_position()
						}
					} else {
						sp.add_error('Expected => or -> in map entry', 'Got ${sp.current.str()}')
						return none
					}
				}

				if !sp.match(lexer.punctuation(.comma)) {
					break
				}
			}
		}
	}

	sp.consume(lexer.punctuation(.rbrace), 'Expected closing brace')?

	// Check if this is a map update (has base_map) or regular map literal
	if base_map != ast.Expr(ast.LiteralExpr{}) {
		return ast.MapUpdateExpr{
			base_map: base_map
			entries:  entries
			position: sp.get_current_position()
		}
	} else {
		return ast.MapLiteralExpr{
			entries:  entries
			position: sp.get_current_position()
		}
	}
}
