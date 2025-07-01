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
					// Check if this is an external function call (atom.function) or record access (identifier.field)
					if expr is ast.LiteralExpr {
						lit_expr := expr as ast.LiteralExpr
						if lit_expr.value is ast.AtomLiteral {
							expr = ep.parse_external_function_call(expr)?
						} else {
							expr = ep.parse_record_access_expression(expr)?
						}
					} else {
						expr = ep.parse_record_access_expression(expr)?
					}
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

	// Check if this is an external function call that already has module and function_name
	if function is ast.CallExpr {
		call_expr := function as ast.CallExpr
		if call_expr.external {
			return ast.CallExpr{
				external:      true
				module:        call_expr.module
				function_name: call_expr.function_name
				arguments:     arguments
				position:      ep.get_current_position()
			}
		}
	}

	return ast.CallExpr{
		function:  function
		arguments: arguments
		position:  ep.get_current_position()
	}
}

// parse_external_function_call parses external function calls like :module.function
fn (mut ep ExpressionParser) parse_external_function_call(module_expr ast.Expr) ?ast.Expr {
	ep.advance() // consume '.'

	field_token := ep.current
	if !ep.current.is_identifier() {
		ep.add_error('Expected function name after dot', 'Got ${ep.current.str()}')
		return none
	}
	ep.advance()

	// Extract module name from atom literal
	lit_expr := module_expr as ast.LiteralExpr
	atom_lit := lit_expr.value as ast.AtomLiteral
	module_name := atom_lit.value
	function_name := field_token.get_value()

	return ast.CallExpr{
		external:      true
		module:        module_name
		function_name: function_name
		arguments:     []
		position:      ep.get_current_position()
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

	// This is a record field access
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
