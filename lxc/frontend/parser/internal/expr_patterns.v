module internal

import ast
import lexer

// parse_pattern parses patterns for pattern matching
fn (mut ep ExpressionParser) parse_pattern() ?ast.Pattern {
	return match ep.current {
		lexer.IdentToken {
			ep.parse_variable_pattern()
		}
		lexer.UpperIdentToken {
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
			match punc_token {
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

	return ast.VarPattern{
		name: token.get_value()
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
	ep.consume(lexer.PunctuationToken.rparen, 'Expected closing parenthesis')?

	return pattern
}

// parse_tuple_pattern parses tuple patterns
fn (mut ep ExpressionParser) parse_tuple_pattern() ?ast.Pattern {
	ep.advance() // consume '{'

	mut elements := []ast.Pattern{}
	if !ep.check(lexer.PunctuationToken.rbrace) {
		for {
			elements << ep.parse_pattern()?

			if !ep.match(lexer.PunctuationToken.comma) {
				break
			}
		}
	}

	ep.consume(lexer.PunctuationToken.rbrace, 'Expected closing brace')?

	return ast.TuplePattern{
		elements: elements
	}
}

// parse_list_pattern parses list patterns
fn (mut ep ExpressionParser) parse_list_pattern() ?ast.Pattern {
	ep.advance() // consume '['

	if ep.check(lexer.PunctuationToken.rbracket) {
		ep.advance() // consume ']'
		return ast.ListEmptyPattern{}
	}

	mut elements := []ast.Pattern{}
	for {
		elements << ep.parse_pattern()?

		if !ep.match(lexer.PunctuationToken.comma) {
			break
		}
	}

	ep.consume(lexer.PunctuationToken.rbracket, 'Expected closing bracket')?

	return ast.ListLiteralPattern{
		elements: elements
	}
}

// parse_map_pattern parses map patterns
fn (mut ep ExpressionParser) parse_map_pattern() ?ast.Pattern {
	ep.advance() // consume '%'
	ep.consume(lexer.PunctuationToken.lbrace, 'Expected opening brace after %')?

	mut entries := []ast.MapPatternEntry{}
	if !ep.check(lexer.PunctuationToken.rbrace) {
		for {
			key := ep.parse_pattern()?

			// Check for colon (atom key) or arrow (general key)
			if ep.match(lexer.PunctuationToken.colon) {
				value := ep.parse_pattern()?
				entries << ast.MapPatternEntry{
					key:      key
					value:    value
					position: ep.get_current_position()
				}
			} else if ep.match(lexer.OperatorToken.arrow) {
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

			if !ep.match(lexer.PunctuationToken.comma) {
				break
			}
		}
	}

	ep.consume(lexer.PunctuationToken.rbrace, 'Expected closing brace')?

	return ast.MapPattern{
		entries: entries
	}
}
