module internal

import ast
import lexer

// parse_tuple_expression parses tuple expressions
fn (mut ep ExpressionParser) parse_tuple_expression() ?ast.Expr {
	ep.advance() // consume '{'

	mut elements := []ast.Expr{}
	if !ep.check(lexer.punctuation(.rbrace)) {
		for {
			elements << ep.parse_expression()?

			if !ep.match(lexer.punctuation(.comma)) {
				break
			}
		}
	}

	ep.consume(lexer.punctuation(.rbrace), 'Expected closing brace')?

	return ast.TupleExpr{
		elements: elements
		position: ep.get_current_position()
	}
}

// parse_list_expression parses list expressions
fn (mut ep ExpressionParser) parse_list_expression() ?ast.Expr {
	ep.advance() // consume '['

	mut elements := []ast.Expr{}
	if !ep.check(lexer.punctuation(.rbracket)) {
		for {
			elements << ep.parse_expression()?
			if !ep.match(lexer.punctuation(.comma)) {
				break
			}
		}
	}

	ep.consume(lexer.punctuation(.rbracket), 'Expected closing bracket')?

	return ast.ListLiteralExpr{
		elements: elements
		position: ep.get_current_position()
	}
}

// parse_map_expression parses map expressions
fn (mut ep ExpressionParser) parse_map_expression() ?ast.Expr {
	ep.advance() // consume '%'
	ep.consume(lexer.punctuation(.lbrace), 'Expected opening brace after %')?

	mut entries := []ast.MapEntry{}
	mut base_map := ast.Expr(ast.LiteralExpr{})

	if !ep.check(lexer.punctuation(.rbrace)) {
		// Check for map update syntax first: %{base_map | key: value, ...}
		// Look ahead to see if this is a map update
		if ep.is_map_update_syntax() {
			// Parse base map - use a specialized parser that stops at |
			base_map = ep.parse_base_map_expression()?

			// The | should be the next token
			if !ep.check(lexer.operator(.pipe)) {
				ep.add_error('Expected | after base map in map update', 'Got ${ep.current.str()}')
				return none
			}
			ep.advance() // consume '|'

			// Parse entries after the pipe
			if !ep.check(lexer.punctuation(.rbrace)) {
				for {
					// Check if we have a key token (identifier:)
					if ep.current.is_key() {
						key_value := ep.current.get_key_value()
						ep.advance() // consume key token

						// Create an atom literal for the key
						key := ast.LiteralExpr{
							value:    ast.AtomLiteral{
								value: key_value
							}
							position: ep.get_current_position()
						}

						value := ep.parse_expression()?
						entries << ast.MapEntry{
							key:      key
							value:    value
							position: ep.get_current_position()
						}
					} else {
						// Parse regular key expression
						key := ep.parse_expression()?

						// Check for fat_arrow or arrow (general key)
						if ep.match(lexer.operator(.fat_arrow)) {
							value := ep.parse_expression()?
							entries << ast.MapEntry{
								key:      key
								value:    value
								position: ep.get_current_position()
							}
						} else if ep.match(lexer.operator(.arrow)) {
							value := ep.parse_expression()?
							entries << ast.MapEntry{
								key:      key
								value:    value
								position: ep.get_current_position()
							}
						} else {
							ep.add_error('Expected => or -> in map entry', 'Got ${ep.current.str()}')
							return none
						}
					}

					if !ep.match(lexer.punctuation(.comma)) {
						break
					}
				}
			}
		} else {
			// Regular map literal: %{key: value, key2: value2}
			for {
				// Check if we have a key token (identifier:)
				if ep.current.is_key() {
					key_value := ep.current.get_key_value()
					ep.advance() // consume key token

					// Create an atom literal for the key
					key := ast.LiteralExpr{
						value:    ast.AtomLiteral{
							value: key_value
						}
						position: ep.get_current_position()
					}

					value := ep.parse_expression()?
					entries << ast.MapEntry{
						key:      key
						value:    value
						position: ep.get_current_position()
					}
				} else {
					// Parse regular key expression
					key := ep.parse_expression()?

					// Check for fat_arrow or arrow (general key)
					if ep.match(lexer.operator(.fat_arrow)) {
						value := ep.parse_expression()?
						entries << ast.MapEntry{
							key:      key
							value:    value
							position: ep.get_current_position()
						}
					} else if ep.match(lexer.operator(.arrow)) {
						value := ep.parse_expression()?
						entries << ast.MapEntry{
							key:      key
							value:    value
							position: ep.get_current_position()
						}
					} else {
						ep.add_error('Expected => or -> in map entry', 'Got ${ep.current.str()}')
						return none
					}
				}

				if !ep.match(lexer.punctuation(.comma)) {
					break
				}
			}
		}
	}

	ep.consume(lexer.punctuation(.rbrace), 'Expected closing brace')?

	// Check if this is a map update (has base_map) or regular map literal
	if base_map != ast.Expr(ast.LiteralExpr{}) {
		return ast.MapUpdateExpr{
			base_map: base_map
			entries:  entries
			position: ep.get_current_position()
		}
	} else {
		return ast.MapLiteralExpr{
			entries:  entries
			position: ep.get_current_position()
		}
	}
}

// parse_record_expression parses record expressions
fn (mut ep ExpressionParser) parse_record_expression() ?ast.Expr {
	ep.advance() // consume 'record'

	// Parse record name
	name_token := ep.current
	if !ep.current.is_identifier() {
		ep.add_error('Expected record name', 'Got ${ep.current.str()}')
		return none
	}
	ep.advance()

	ep.consume(lexer.punctuation(.lbrace), 'Expected opening brace')?

	mut fields := []ast.RecordField{}
	if !ep.check(lexer.punctuation(.rbrace)) {
		for {
			field_name := ep.current.get_value()
			if !ep.current.is_identifier() {
				ep.add_error('Expected field name', 'Got ${ep.current.str()}')
				return none
			}
			ep.advance()

			ep.consume(lexer.punctuation(.colon), 'Expected colon after field name')?

			value := ep.parse_expression()?
			fields << ast.RecordField{
				name:     field_name
				value:    value
				position: ep.get_current_position()
			}

			if !ep.match(lexer.punctuation(.comma)) {
				break
			}
		}
	}

	ep.consume(lexer.punctuation(.rbrace), 'Expected closing brace')?

	return ast.RecordLiteralExpr{
		name:     name_token.get_value()
		fields:   fields
		position: ep.get_current_position()
	}
}

// is_map_update_syntax checks if this is a map update syntax: %{base_map | ...}
fn (ep ExpressionParser) is_map_update_syntax() bool {
	mut pos := ep.position

	// Look ahead to find a pipe operator before any colon, fat_arrow, or arrow
	for pos < ep.tokens.len {
		token := ep.tokens[pos]

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

// parse_base_map_expression parses the base map expression in map updates, stopping at |
fn (mut ep ExpressionParser) parse_base_map_expression() ?ast.Expr {
	// Parse a simple expression that stops at |
	return ep.parse_atom_expression()
}
