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
			ep.parse_identifier_expression()
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
					ep.parse_case_expression()
				}
				.with {
					ep.parse_with_expression()
				}
				.for_ {
					ep.parse_for_expression()
				}
				.receive {
					ep.parse_receive_expression()
				}
				.record {
					ep.parse_record_expression()
				}
				.unsafe {
					ep.parse_unsafe_expression()
				}
				.nil_ {
					ep.advance()
					return ast.LiteralExpr{
						value: ast.NilLiteral{}
					}
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
				else {
					ep.add_error('Unexpected token: ${ep.current.str()}', 'Expected expression')
					none
				}
			}
		}
		lexer.OperatorToken {
			op_token := ep.current as lexer.OperatorToken
			match op_token.value {
				.record_update {
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
		name: token.get_value()
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
