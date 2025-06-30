module internal

import ast
import lexer

// parse_primary_expression parses primary expressions
fn (mut ep ExpressionParser) parse_primary_expression() ?ast.Expr {
	mut expr := ep.parse_atom_expression()?

	// Handle postfix operations
	for {
		if ep.current is lexer.PunctuationToken {
			punc_token := ep.current as lexer.PunctuationToken
			match punc_token.value {
				.lparen {
					expr = ep.parse_call_expression(expr)?
				}
				.lbracket {
					expr = ep.parse_map_access_expression(expr)?
				}
				else {
					break
				}
			}
		} else if ep.current is lexer.OperatorToken {
			op_token := ep.current as lexer.OperatorToken
			match op_token.value {
				.dot {
					expr = ep.parse_record_access_expression(expr)?
				}
				else {
					break
				}
			}
		} else {
			break
		}
	}

	return expr
}

// parse_call_expression parses function call expressions
fn (mut ep ExpressionParser) parse_call_expression(function ast.Expr) ?ast.Expr {
	ep.advance() // consume '('

	mut arguments := []ast.Expr{}
	if !ep.check(lexer.punctuation(.rparen)) {
		for {
			arguments << ep.parse_expression()?

			if !ep.match(lexer.punctuation(.comma)) {
				break
			}
		}
	}

	ep.consume(lexer.punctuation(.rparen), 'Expected closing parenthesis')?

	return ast.CallExpr{
		function:  function
		arguments: arguments
		position:  ep.get_current_position()
	}
}

// parse_record_access_expression parses record field access expressions
fn (mut ep ExpressionParser) parse_record_access_expression(record ast.Expr) ?ast.Expr {
	ep.advance() // consume '.'

	field_token := ep.current
	if !ep.current.is_identifier() {
		ep.add_error('Expected identifier after dot', 'Got ${ep.current.str()}')
		return none
	}
	ep.advance()

	return ast.RecordAccessExpr{
		record:   record
		field:    field_token.get_value()
		position: ep.get_current_position()
	}
}

// parse_map_access_expression parses map access expressions
fn (mut ep ExpressionParser) parse_map_access_expression(map_expr ast.Expr) ?ast.Expr {
	ep.advance() // consume '['
	key := ep.parse_expression()?
	ep.consume(lexer.punctuation(.rbracket), 'Expected closing bracket')?

	return ast.MapAccessExpr{
		map_expr: map_expr
		key:      key
		position: ep.get_current_position()
	}
}
