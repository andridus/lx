module internal

import ast
import lexer

// parse_pattern parses patterns for pattern matching
fn (mut ep ExpressionParser) parse_pattern() ?ast.Pattern {
	return ep.parse_base_pattern()
}

// parse_base_pattern parses the base pattern without assignment
fn (mut ep ExpressionParser) parse_base_pattern() ?ast.Pattern {
	return match ep.current {
		lexer.IdentToken {
			ep.parse_variable_pattern()
		}
		lexer.UpperIdentToken {
			// Check if this is a record pattern: RecordName{field: pattern, ...}
			if ep.peek() is lexer.PunctuationToken {
				punc := ep.peek() as lexer.PunctuationToken
				if punc.value == .lbrace {
					return ep.parse_record_pattern()
				}
			}
			ep.parse_variable_pattern()
		}
		lexer.StringToken {
			ep.parse_literal_pattern()
		}
		lexer.IntToken {
			ep.parse_literal_pattern()
		}
		lexer.FloatToken {
			ep.parse_literal_pattern()
		}
		lexer.BoolToken {
			ep.parse_literal_pattern()
		}
		lexer.AtomToken {
			ep.parse_atom_pattern()
		}
		lexer.NilToken {
			ep.parse_nil_pattern()
		}
		lexer.PunctuationToken {
			punc_token := ep.current as lexer.PunctuationToken
			match punc_token.value {
				.lparen {
					ep.parse_parenthesized_pattern()
				}
				.lbrace {
					ep.parse_tuple_pattern()
				}
				.lbracket {
					ep.parse_list_pattern()
				}
				else {
					ep.add_error('Unexpected token in pattern: ${ep.current.str()}', 'Expected pattern')
					none
				}
			}
		}
		lexer.OperatorToken {
			ep.parse_map_pattern()
		}
		else {
			ep.add_error('Unexpected token in pattern: ${ep.current.str()}', 'Expected pattern')
			none
		}
	}
}

// parse_variable_pattern parses variable patterns
fn (mut ep ExpressionParser) parse_variable_pattern() ?ast.Pattern {
	token := ep.current
	ep.advance()

	mut type_annotation := ?ast.TypeExpression(none)

	// Check for type annotation (:: Type)
	if ep.check(lexer.operator(.type_cons)) {
		ep.advance() // consume '::'
		type_annotation = ep.parse_type_expression()?
	}

	pos := token.get_position()
	return ast.VarPattern{
		name:            token.get_value()
		position:        ast.new_position(pos.line, pos.column, pos.filename)
		type_annotation: type_annotation
	}
}

// parse_literal_pattern parses literal patterns
fn (mut ep ExpressionParser) parse_literal_pattern() ?ast.Pattern {
	token := ep.current
	ep.advance()

	value := match token {
		lexer.StringToken {
			ast.Literal(ast.StringLiteral{
				value: token.value
			})
		}
		lexer.IntToken {
			ast.Literal(ast.IntegerLiteral{
				value: token.value
			})
		}
		lexer.FloatToken {
			ast.Literal(ast.FloatLiteral{
				value: token.value
			})
		}
		lexer.BoolToken {
			ast.Literal(ast.BooleanLiteral{
				value: token.value
			})
		}
		else {
			ast.Literal(ast.NilLiteral{})
		}
	}

	return ast.LiteralPattern{
		value: value
	}
}

// parse_atom_pattern parses atom patterns
fn (mut ep ExpressionParser) parse_atom_pattern() ?ast.Pattern {
	token := ep.current
	ep.advance()

	return ast.AtomPattern{
		value: token.get_value()
	}
}

// parse_nil_pattern parses nil patterns
fn (mut ep ExpressionParser) parse_nil_pattern() ?ast.Pattern {
	ep.advance()
	return ast.WildcardPattern{}
}

// parse_parenthesized_pattern parses parenthesized patterns
fn (mut ep ExpressionParser) parse_parenthesized_pattern() ?ast.Pattern {
	ep.advance() // consume '('
	pattern := ep.parse_pattern()?
	ep.consume(lexer.punctuation(.rparen), 'Expected closing parenthesis')?

	return pattern
}

// parse_tuple_pattern parses tuple patterns
fn (mut ep ExpressionParser) parse_tuple_pattern() ?ast.Pattern {
	ep.advance() // consume '{'

	mut elements := []ast.Pattern{}
	if !ep.check(lexer.punctuation(.rbrace)) {
		for {
			elements << ep.parse_pattern()?

			if !ep.match(lexer.punctuation(.comma)) {
				break
			}
		}
	}

	ep.consume(lexer.punctuation(.rbrace), 'Expected closing brace')?

	return ast.TuplePattern{
		elements: elements
	}
}

// parse_list_pattern parses list patterns
fn (mut ep ExpressionParser) parse_list_pattern() ?ast.Pattern {
	ep.advance() // consume '['

	if ep.check(lexer.punctuation(.rbracket)) {
		ep.advance() // consume ']'
		return ast.ListEmptyPattern{}
	}

	// Parse first element
	first := ep.parse_pattern()?

	// Check if this is a cons pattern [head | tail]
	if ep.match(lexer.operator(.pipe)) {
		rest := ep.parse_pattern()?
		ep.consume(lexer.punctuation(.rbracket), 'Expected closing bracket')?

		return ast.ListConsPattern{
			head: first
			tail: rest
		}
	}

	// Regular list pattern [elem1, elem2, ...]
	mut elements := [first]
	for ep.match(lexer.punctuation(.comma)) {
		elements << ep.parse_pattern()?
	}

	ep.consume(lexer.punctuation(.rbracket), 'Expected closing bracket')?

	return ast.ListLiteralPattern{
		elements: elements
	}
}

// parse_record_pattern parses record patterns: RecordName{field: pattern, ...}
fn (mut ep ExpressionParser) parse_record_pattern() ?ast.Pattern {
	record_name := ep.current.get_value()
	ep.advance() // consume record name
	ep.consume(lexer.punctuation(.lbrace), 'Expected opening brace after record name')?

	mut fields := []ast.RecordPatternField{}
	if !ep.check(lexer.punctuation(.rbrace)) {
		for {
			// if ep.current is lexer.KeywordToken {
			// 	kw := ep.current as lexer.KeywordToken
			// 	if kw.value == .when || kw.value == .do_ || kw.value == .else_ || kw.value == .end_ {
			// 		break
			// 	}
			// }
			// if ep.current is lexer.OperatorToken {
			// 	op := ep.current as lexer.OperatorToken
			// 	if op.value == .arrow {
			// 		break
			// 	}
			// }
			// if ep.check(lexer.punctuation(.rbrace)) {
			// 	break
			// }

			// if !(ep.current.is_key() || ep.current.is_identifier()) {
			// 	break
			// }
			// Parse field name
			mut field_name := ''

			// Check if this is a key token (field:)
			if ep.current.is_key() {
				field_name = ep.current.get_key_value()
				ep.advance() // consume key token
			} else if ep.current.is_identifier() {
				field_name = ep.current.get_value()
				ep.advance()

				// Expect colon after field name
				ep.consume(lexer.punctuation(.colon), 'Expected colon after field name')?
			} else {
				ep.add_error('Expected field name in record pattern', 'Got ${ep.current.str()}')
				return none
			}

			// Parse field pattern (which might include assignment)
			field_pattern := ep.parse_field_pattern()?

			fields << ast.RecordPatternField{
				name:     field_name
				pattern:  field_pattern
				position: ep.get_current_position()
			}

			if !ep.match(lexer.punctuation(.comma)) {
				break
			}
		}
	}

	ep.consume(lexer.punctuation(.rbrace), 'Expected closing brace for record pattern')?

	return ast.RecordPattern{
		name:   record_name
		fields: fields
	}
}

// parse_map_pattern parses map patterns
fn (mut ep ExpressionParser) parse_map_pattern() ?ast.Pattern {
	ep.advance() // consume '%'
	ep.consume(lexer.punctuation(.lbrace), 'Expected opening brace after %')?

	mut entries := []ast.MapPatternEntry{}
	if !ep.check(lexer.punctuation(.rbrace)) {
		for {
			// Check if we have a key token (identifier:)
			if ep.current.is_key() {
				key_value := ep.current.get_key_value()
				ep.advance() // consume key token

				// Create an atom pattern for the key
				key := ast.AtomPattern{
					value:    key_value
					position: ep.get_current_position()
				}

				value := ep.parse_pattern()?
				entries << ast.MapPatternEntry{
					key:      key
					value:    value
					position: ep.get_current_position()
				}
			} else {
				// Parse regular key pattern
				key := ep.parse_pattern()?

				// Check for fat_arrow or arrow (general key)
				if ep.match(lexer.operator(.fat_arrow)) {
					value := ep.parse_pattern()?
					entries << ast.MapPatternEntry{
						key:      key
						value:    value
						position: ep.get_current_position()
					}
				} else if ep.match(lexer.operator(.arrow)) {
					value := ep.parse_pattern()?
					entries << ast.MapPatternEntry{
						key:      key
						value:    value
						position: ep.get_current_position()
					}
				} else {
					ep.add_error('Expected : or => in map pattern', 'Got ${ep.current.str()}')
					return none
				}
			}

			if !ep.match(lexer.punctuation(.comma)) {
				break
			}
		}
	}

	ep.consume(lexer.punctuation(.rbrace), 'Expected closing brace')?

	return ast.MapPattern{
		entries: entries
	}
}

// parse_field_pattern parses field patterns that might include assignment
fn (mut ep ExpressionParser) parse_field_pattern() ?ast.Pattern {
	// Parse the base pattern first
	base_pattern := ep.parse_base_pattern()?

	// Check if this is followed by an assignment (pattern = variable)
	if ep.check(lexer.operator(.assign)) {
		ep.advance() // consume '='

		// The right side should be a variable
		if !ep.current.is_identifier() {
			ep.add_error('Expected variable name after = in pattern', 'Got ${ep.current.str()}')
			return none
		}

		// Create a variable pattern for the assigned variable
		var_name := ep.current.get_value()
		ep.advance()

		// We need to create a pattern that includes both the base pattern and the variable
		match base_pattern {
			ast.RecordPattern {
				// For record patterns, we add the variable to the assign_variable field
				return ast.RecordPattern{
					name:            base_pattern.name
					fields:          base_pattern.fields
					assign_variable: var_name
				}
			}
			else {
				// For other patterns, just return the base pattern
				// The variable binding will be handled by the generator
				return base_pattern
			}
		}
	}

	return base_pattern
}
