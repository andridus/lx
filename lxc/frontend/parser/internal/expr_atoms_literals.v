module internal

import ast
import lexer

// parse_atom_expression parses atomic expressions
fn (mut ep ExpressionParser) parse_atom_expression() ?ast.Expr {
	return match ep.current {
		lexer.IdentToken {
			ep.parse_identifier_expression()
		}
		lexer.UpperIdentToken {
			if ep.peek() is lexer.PunctuationToken {
				punc := ep.peek() as lexer.PunctuationToken
				if punc.value == .lbrace {
					return ep.parse_record_value_expression()
				}
			}
			return ep.parse_identifier_expression()
		}
		lexer.StringToken {
			ep.parse_string_literal()
		}
		lexer.IntToken {
			ep.parse_integer_literal()
		}
		lexer.FloatToken {
			ep.parse_float_literal()
		}
		lexer.BoolToken {
			ep.parse_boolean_literal()
		}
		lexer.AtomToken {
			ep.parse_atom_literal()
		}
		lexer.NilToken {
			ep.parse_nil_literal()
		}
		lexer.KeyToken {
			ep.parse_key_as_atom()
		}
		lexer.ErrorToken {
			// Adiciona erro ao parser e avança
			err := ep.current as lexer.ErrorToken
			ep.add_error('Parse error: ${err.message}', 'ErrorToken')
			ep.advance()
			return ast.LiteralExpr{
				value: ast.NilLiteral{}
			}
		}
		lexer.KeywordToken {
			keyword_token := ep.current as lexer.KeywordToken
			match keyword_token.value {
				.true_ {
					ep.advance()
					ast.LiteralExpr{
						value: ast.BooleanLiteral{
							value: true
						}
					}
				}
				.false_ {
					ep.advance()
					ast.LiteralExpr{
						value: ast.BooleanLiteral{
							value: false
						}
					}
				}
				.if_ {
					ep.parse_if_expression()
				}
				.case_ {
					// Case expressions need special handling - delegate to StatementParser
					ep.parse_case_via_statement_parser()
				}
				.with {
					ep.parse_with_expression()
				}
				.for_ {
					ep.parse_for_expression()
				}
				.receive {
					// Receive expressions need special handling - delegate to StatementParser
					ep.parse_receive_via_statement_parser()
				}
				.record {
					ep.parse_record_expression()
				}
				.unsafe {
					ep.parse_unsafe_expression()
				}
				.match_ {
					ep.parse_match_rescue_expression()
				}
				.nil_ {
					ep.advance()
					return ast.LiteralExpr{
						value: ast.NilLiteral{}
					}
				}
				.do_ {
					ep.parse_block_expression()
				}
				else {
					ep.add_error('Unexpected token: ${ep.current.str()}', 'Expected expression')
					none
				}
			}
		}
		lexer.PunctuationToken {
			punc_token := ep.current as lexer.PunctuationToken
			match punc_token.value {
				.lparen {
					ep.parse_parenthesized_expression()
				}
				.lbrace {
					ep.parse_tuple_expression()
				}
				.lbracket {
					ep.parse_list_expression()
				}
				.colon {
					ep.parse_external_atom()
				}
				else {
					ep.add_error('Unexpected token: ${ep.current.str()}', 'Expected expression')
					none
				}
			}
		}
		lexer.OperatorToken {
			op_token := ep.current as lexer.OperatorToken
			match op_token.value {
				.modulo {
					ep.parse_map_expression()
				}
				else {
					ep.add_error('Unexpected token: ${ep.current.str()}', 'Expected expression')
					none
				}
			}
		}
		else {
			ep.add_error('Unexpected token: ${ep.current.str()}', 'Expected expression')
			none
		}
	}
}

// parse_identifier_expression parses identifier expressions
fn (mut ep ExpressionParser) parse_identifier_expression() ?ast.Expr {
	token := ep.current
	ep.advance()

	return ast.VariableExpr{
		name:     token.get_value()
		position: ast.new_position(token.get_position().line, token.get_position().column,
			token.get_position().filename)
	}
}

// parse_string_literal parses string literals
fn (mut ep ExpressionParser) parse_string_literal() ?ast.Expr {
	token := ep.current
	ep.advance()

	return ast.LiteralExpr{
		value: ast.StringLiteral{
			value: token.get_value()
		}
	}
}

// parse_integer_literal parses integer literals
fn (mut ep ExpressionParser) parse_integer_literal() ?ast.Expr {
	token := ep.current
	ep.advance()

	value := token.get_numeric_value() or { 0.0 }
	return ast.LiteralExpr{
		value: ast.IntegerLiteral{
			value: int(value)
		}
	}
}

// parse_float_literal parses float literals
fn (mut ep ExpressionParser) parse_float_literal() ?ast.Expr {
	token := ep.current
	ep.advance()

	value := token.get_numeric_value() or { 0.0 }
	return ast.LiteralExpr{
		value: ast.FloatLiteral{
			value: value
		}
	}
}

// parse_boolean_literal parses boolean literals
fn (mut ep ExpressionParser) parse_boolean_literal() ?ast.Expr {
	token := ep.current
	ep.advance()

	value := token.get_boolean_value() or { false }
	return ast.LiteralExpr{
		value: ast.BooleanLiteral{
			value: value
		}
	}
}

// parse_atom_literal parses atom literals
fn (mut ep ExpressionParser) parse_atom_literal() ?ast.Expr {
	token := ep.current
	ep.advance()

	return ast.LiteralExpr{
		value: ast.AtomLiteral{
			value: token.get_value()
		}
	}
}

// parse_nil_literal parses nil literals
fn (mut ep ExpressionParser) parse_nil_literal() ?ast.Expr {
	ep.advance()

	return ast.LiteralExpr{
		value: ast.NilLiteral{}
	}
}

// parse_parenthesized_expression parses parenthesized expressions
fn (mut ep ExpressionParser) parse_parenthesized_expression() ?ast.Expr {
	ep.advance() // consume '('
	expr := ep.parse_expression()?
	ep.consume(lexer.punctuation(.rparen), 'Expected closing parenthesis')?

	return expr
}

fn (mut ep ExpressionParser) parse_external_atom() ?ast.Expr {
	ep.advance()
	return match ep.current {
		lexer.AtomToken {
			ep.parse_atom_literal()
		}
		lexer.IdentToken {
			ident_token := ep.current as lexer.IdentToken
			ep.advance()
			ast.LiteralExpr{
				value: ast.AtomLiteral{
					value: ident_token.value
				}
			}
		}
		else {
			ep.add_error('Expected atom or identifier after :', 'Got ${ep.current.str()}')
			none
		}
	}
}

// parse_receive_via_statement_parser delegates receive parsing to StatementParser
fn (mut ep ExpressionParser) parse_receive_via_statement_parser() ?ast.Expr {
	// Create a StatementParser with the same tokens and position
	mut sp := StatementParser{
		Parser: Parser{
			tokens:   ep.tokens
			position: ep.position
			current:  ep.current
			errors:   ep.errors
		}
	}

	// Parse the receive expression using StatementParser
	result := sp.parse_receive_expression()?

	// Update the ExpressionParser's state from StatementParser
	ep.position = sp.position
	ep.current = sp.current
	ep.errors = sp.errors

	return result
}

// parse_case_via_statement_parser delegates case parsing to StatementParser
fn (mut ep ExpressionParser) parse_case_via_statement_parser() ?ast.Expr {
	// Create a StatementParser with the same tokens and position
	mut sp := StatementParser{
		Parser: Parser{
			tokens:   ep.tokens
			position: ep.position
			current:  ep.current
			errors:   ep.errors
		}
	}

	// Parse the case expression using StatementParser
	result := sp.parse_case_expression()?

	// Update the ExpressionParser's state from StatementParser
	ep.position = sp.position
	ep.current = sp.current
	ep.errors = sp.errors

	return result
}

// parse_key_as_atom parses a key token as an atom literal
fn (mut ep ExpressionParser) parse_key_as_atom() ?ast.Expr {
	token := ep.current as lexer.KeyToken
	ep.advance()

	return ast.LiteralExpr{
		value:    ast.AtomLiteral{
			value: token.value
		}
		position: ast.new_position(token.position.line, token.position.column, token.position.filename)
	}
}

fn (mut ep ExpressionParser) parse_record_value_expression() ?ast.Expr {
	record_name := ep.current.get_value()
	ep.advance()
	ep.consume(lexer.punctuation(.lbrace), 'Expected opening brace after record name')?

	// Check if this is a record update: RecordName{var | field: value, ...}
	// Look ahead to see if the first token is followed by |
	if !ep.check(lexer.punctuation(.rbrace)) {
		// Parse the first expression (should be the base record variable)
		base_record := ep.parse_atom_expression()?

		// Check if next token is pipe operator
		if ep.check(lexer.operator(.pipe)) {
			ep.advance() // consume '|'

			// Parse fields after the pipe
			mut fields := []ast.RecordField{}
			if !ep.check(lexer.punctuation(.rbrace)) {
				for {
					if ep.current is lexer.KeyToken {
						token := ep.current as lexer.KeyToken
						field_name := token.value
						ep.advance()
						value := ep.parse_expression()?
						fields << ast.RecordField{
							name:     field_name
							value:    value
							position: ep.get_current_position()
						}
					} else {
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
					}

					if !ep.match(lexer.punctuation(.comma)) {
						break
					}
				}
			}

			ep.consume(lexer.punctuation(.rbrace), 'Expected closing brace for record update')?

			return ast.RecordUpdateExpr{
				record_name: record_name
				base_record: base_record
				fields:      fields
				position:    ep.get_current_position()
			}
		}
	}

	// This is a regular record literal: RecordName{field: value, ...}
	// Reset position to after the opening brace
	ep.position -= 1 // Go back to the opening brace
	ep.current = ep.tokens[ep.position]

	mut fields := []ast.RecordField{}
	if !ep.check(lexer.punctuation(.rbrace)) {
		for {
			if ep.current is lexer.KeyToken {
				token := ep.current as lexer.KeyToken
				field_name := token.value
				ep.advance()
				value := ep.parse_expression()?
				fields << ast.RecordField{
					name:     field_name
					value:    value
					position: ep.get_current_position()
				}
			} else {
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
			}

			if !ep.match(lexer.punctuation(.comma)) {
				break
			}
		}
	}

	ep.consume(lexer.punctuation(.rbrace), 'Expected closing brace for record value')?

	return ast.RecordLiteralExpr{
		name:     record_name
		fields:   fields
		position: ep.get_current_position()
	}
}
