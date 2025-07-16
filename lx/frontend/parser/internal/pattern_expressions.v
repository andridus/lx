module internal

import ast
import lexer

// ========================================
// PATTERN PARSING
// This module handles all pattern parsing for pattern matching
// ========================================

// parse_pattern parses patterns for pattern matching
pub fn (mut p LXParser) parse_pattern() ?ast.Pattern {
	return p.parse_primary_pattern()
}

// ========================================
// PRIMARY PATTERNS
// Grammar: primary_pattern ::= literal_pattern | variable_pattern | tuple_pattern | list_pattern | record_pattern
// ========================================

// parse_primary_pattern parses primary patterns
fn (mut p LXParser) parse_primary_pattern() ?ast.Pattern {
	return match p.current {
		lexer.IdentToken {
			p.parse_variable_pattern()
		}
		lexer.UpperIdentToken {
			// Check if this is a record pattern
			if p.peek() is lexer.PunctuationToken {
				next := p.peek() as lexer.PunctuationToken
				if next.value == .lbrace {
					return p.parse_record_pattern()
				}
			}
			p.parse_variable_pattern()
		}
		lexer.IntToken {
			p.parse_integer_pattern()
		}
		lexer.FloatToken {
			p.parse_float_pattern()
		}
		lexer.StringToken {
			p.parse_string_pattern()
		}
		lexer.BoolToken {
			p.parse_boolean_pattern()
		}
		lexer.AtomToken {
			p.parse_atom_pattern()
		}
		lexer.NilToken {
			p.parse_nil_pattern()
		}
		lexer.PunctuationToken {
			punct := p.current as lexer.PunctuationToken
			match punct.value {
				.lparen {
					p.parse_parenthesized_pattern()
				}
				.lbrace {
					p.parse_tuple_pattern()
				}
				.lbracket {
					p.parse_list_pattern()
				}
				else {
					p.add_error('Unexpected punctuation in pattern', 'Got ${p.current.str()}')
					none
				}
			}
		}
		lexer.OperatorToken {
			op := p.current as lexer.OperatorToken
			match op.value {
				.modulo {
					p.parse_map_pattern()
				}
				else {
					p.add_error('Unexpected operator in pattern', 'Got ${p.current.str()}')
					none
				}
			}
		}
		else {
			p.add_error('Unexpected token in pattern', 'Got ${p.current.str()}')
			none
		}
	}
}

// ========================================
// LITERAL PATTERNS
// ========================================

// parse_variable_pattern parses variable patterns
fn (mut p LXParser) parse_variable_pattern() ?ast.Pattern {
	name := p.current.get_value()
	position := p.get_current_position()
	p.advance()

	mut type_annotation := ?ast.TypeExpression(none)

	// Check for type annotation (:: Type)
	if p.check(operator_token(.type_cons)) {
		p.advance() // consume '::'
		type_annotation = p.parse_type_expression()?
	}

	return ast.VarPattern{
		name:            name
		position:        position
		type_annotation: type_annotation
	}
}

// parse_integer_pattern parses integer literal patterns
fn (mut p LXParser) parse_integer_pattern() ?ast.Pattern {
	value := p.current.get_numeric_value() or { 0.0 }
	position := p.get_current_position()
	p.advance()

	return ast.LiteralPattern{
		value: ast.IntegerLiteral{
			value:    int(value)
			position: position
		}
	}
}

// parse_float_pattern parses float literal patterns
fn (mut p LXParser) parse_float_pattern() ?ast.Pattern {
	value := p.current.get_numeric_value() or { 0.0 }
	position := p.get_current_position()
	p.advance()

	return ast.LiteralPattern{
		value: ast.FloatLiteral{
			value:    value
			position: position
		}
	}
}

// parse_string_pattern parses string literal patterns
fn (mut p LXParser) parse_string_pattern() ?ast.Pattern {
	value := p.current.get_value()
	position := p.get_current_position()
	p.advance()

	return ast.LiteralPattern{
		value: ast.StringLiteral{
			value:    value
			position: position
		}
	}
}

// parse_boolean_pattern parses boolean literal patterns
fn (mut p LXParser) parse_boolean_pattern() ?ast.Pattern {
	value := p.current.get_boolean_value() or { false }
	position := p.get_current_position()
	p.advance()

	return ast.LiteralPattern{
		value: ast.BooleanLiteral{
			value:    value
			position: position
		}
	}
}

// parse_atom_pattern parses atom literal patterns
fn (mut p LXParser) parse_atom_pattern() ?ast.Pattern {
	value := p.current.get_value()
	position := p.get_current_position()
	p.advance()

	return ast.AtomPattern{
		value:    value
		position: position
	}
}

// parse_nil_pattern parses nil patterns
fn (mut p LXParser) parse_nil_pattern() ?ast.Pattern {
	p.advance()

	return ast.WildcardPattern{}
}

// ========================================
// COMPLEX PATTERNS
// ========================================

// parse_parenthesized_pattern parses parenthesized patterns
fn (mut p LXParser) parse_parenthesized_pattern() ?ast.Pattern {
	p.advance() // consume '('
	pattern := p.parse_pattern()?
	p.consume(punctuation_token(.rparen), 'Expected ) after pattern')?
	return pattern
}

// parse_tuple_pattern parses tuple patterns
fn (mut p LXParser) parse_tuple_pattern() ?ast.Pattern {
	p.advance() // consume '{'

	mut elements := []ast.Pattern{}

	if !p.check(punctuation_token(.rbrace)) {
		for {
			elements << p.parse_pattern()?

			if !p.match(punctuation_token(.comma)) {
				break
			}
		}
	}

	p.consume(punctuation_token(.rbrace), 'Expected } after tuple pattern')?

	return ast.TuplePattern{
		elements: elements
	}
}

// parse_list_pattern parses list patterns
fn (mut p LXParser) parse_list_pattern() ?ast.Pattern {
	p.advance() // consume '['

	if p.check(punctuation_token(.rbracket)) {
		p.advance() // consume ']'
		return ast.ListEmptyPattern{}
	}

	// Parse first element
	first := p.parse_pattern()?

	// Check if this is a cons pattern [head | tail]
	if p.match(operator_token(.pipe)) {
		tail := p.parse_pattern()?
		p.consume(punctuation_token(.rbracket), 'Expected ] after cons pattern')?

		return ast.ListConsPattern{
			head: first
			tail: tail
		}
	}

	// Regular list pattern [elem1, elem2, ...]
	mut elements := [first]
	for p.match(punctuation_token(.comma)) {
		elements << p.parse_pattern()?
	}

	p.consume(punctuation_token(.rbracket), 'Expected ] after list pattern')?

	return ast.ListLiteralPattern{
		elements: elements
	}
}

// parse_record_pattern parses record patterns: RecordName{field: pattern, ...}
fn (mut p LXParser) parse_record_pattern() ?ast.Pattern {
	record_name := p.current.get_value()
	p.advance() // consume record name

	p.consume(punctuation_token(.lbrace), 'Expected { after record name')?

	mut fields := []ast.RecordPatternField{}

	if !p.check(punctuation_token(.rbrace)) {
		for {
			// Parse field name
			mut field_name := ''

			// Check if this is a key token (field:)
			if p.current.is_key() {
				field_name = p.current.get_key_value()
				p.advance() // consume key token
			} else if p.current.is_identifier() {
				field_name = p.current.get_value()
				p.advance()

				// Expect colon after field name
				p.consume(punctuation_token(.colon), 'Expected : after field name')?
			} else {
				p.add_error('Expected field name in record pattern', 'Got ${p.current.str()}')
				return none
			}

			// Parse field pattern
			field_pattern := p.parse_pattern()?

			fields << ast.RecordPatternField{
				name:     field_name
				pattern:  field_pattern
				position: p.get_current_position()
			}

			if !p.match(punctuation_token(.comma)) {
				break
			}
		}
	}

	p.consume(punctuation_token(.rbrace), 'Expected } after record pattern')?

	return ast.RecordPattern{
		name:   record_name
		fields: fields
	}
}

// parse_map_pattern parses map patterns: %{key: pattern, ...}
fn (mut p LXParser) parse_map_pattern() ?ast.Pattern {
	p.advance() // consume '%'

	p.consume(punctuation_token(.lbrace), 'Expected { after %')?

	mut entries := []ast.MapPatternEntry{}

	if !p.check(punctuation_token(.rbrace)) {
		for {
			// Check if we have a key token (identifier:)
			if p.current.is_key() {
				key_value := p.current.get_key_value()
				p.advance() // consume key token

				// Create an atom pattern for the key
				key := ast.AtomPattern{
					value:    key_value
					position: p.get_current_position()
				}

				value := p.parse_pattern()?
				entries << ast.MapPatternEntry{
					key:      key
					value:    value
					position: p.get_current_position()
				}
			} else {
				// Parse regular key pattern
				key := p.parse_pattern()?

				// Check for fat_arrow or arrow (general key)
				if p.match(operator_token(.fat_arrow)) {
					value := p.parse_pattern()?
					entries << ast.MapPatternEntry{
						key:      key
						value:    value
						position: p.get_current_position()
					}
				} else if p.match(operator_token(.arrow)) {
					value := p.parse_pattern()?
					entries << ast.MapPatternEntry{
						key:      key
						value:    value
						position: p.get_current_position()
					}
				} else {
					p.add_error('Expected => or -> in map pattern', 'Got ${p.current.str()}')
					return none
				}
			}

			if !p.match(punctuation_token(.comma)) {
				break
			}
		}
	}

	p.consume(punctuation_token(.rbrace), 'Expected } after map pattern')?

	return ast.MapPattern{
		entries:         entries
		assign_variable: none
	}
}
