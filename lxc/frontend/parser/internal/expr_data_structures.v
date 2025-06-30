module internal

import ast
import lexer

// parse_tuple_expression parses tuple expressions
fn (mut ep ExpressionParser) parse_tuple_expression() ?ast.Expr {
	ep.advance() // consume '{'

	mut elements := []ast.Expr{}
	if !ep.check(lexer.PunctuationToken.rbrace) {
		for {
			elements << ep.parse_expression()?

			if !ep.match(lexer.PunctuationToken.comma) {
				break
			}
		}
	}

	ep.consume(lexer.PunctuationToken.rbrace, 'Expected closing brace')?

	return ast.TupleExpr{
		elements: elements
		position: ep.get_current_position()
	}
}

// parse_list_expression parses list expressions
fn (mut ep ExpressionParser) parse_list_expression() ?ast.Expr {
	ep.advance() // consume '['

	mut elements := []ast.Expr{}
	if !ep.check(lexer.PunctuationToken.rbracket) {
		for {
			elements << ep.parse_expression()?

			if !ep.match(lexer.PunctuationToken.comma) {
				break
			}
		}
	}

	ep.consume(lexer.PunctuationToken.rbracket, 'Expected closing bracket')?

	return ast.ListLiteralExpr{
		elements: elements
		position: ep.get_current_position()
	}
}

// parse_map_expression parses map expressions
fn (mut ep ExpressionParser) parse_map_expression() ?ast.Expr {
	ep.advance() // consume '%'
	ep.consume(lexer.PunctuationToken.lbrace, 'Expected opening brace after %')?

	mut entries := []ast.MapEntry{}
	if !ep.check(lexer.PunctuationToken.rbrace) {
		for {
			key := ep.parse_expression()?

			// Check for colon (atom key) or arrow (general key)
			if ep.match(lexer.PunctuationToken.colon) {
				value := ep.parse_expression()?
				entries << ast.MapEntry{
					key:      key
					value:    value
					position: ep.get_current_position()
				}
			} else if ep.match(lexer.OperatorToken.arrow) {
				value := ep.parse_expression()?
				entries << ast.MapEntry{
					key:      key
					value:    value
					position: ep.get_current_position()
				}
			} else {
				ep.add_error('Expected : or => in map entry', 'Got ${ep.current.str()}')
				return none
			}

			if !ep.match(lexer.PunctuationToken.comma) {
				break
			}
		}
	}

	ep.consume(lexer.PunctuationToken.rbrace, 'Expected closing brace')?

	return ast.MapLiteralExpr{
		entries:  entries
		position: ep.get_current_position()
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

	ep.consume(lexer.PunctuationToken.lbrace, 'Expected opening brace')?

	mut fields := []ast.RecordField{}
	if !ep.check(lexer.PunctuationToken.rbrace) {
		for {
			field_name := ep.current.get_value()
			if !ep.current.is_identifier() {
				ep.add_error('Expected field name', 'Got ${ep.current.str()}')
				return none
			}
			ep.advance()

			ep.consume(lexer.PunctuationToken.colon, 'Expected colon after field name')?

			value := ep.parse_expression()?
			fields << ast.RecordField{
				name:     field_name
				value:    value
				position: ep.get_current_position()
			}

			if !ep.match(lexer.PunctuationToken.comma) {
				break
			}
		}
	}

	ep.consume(lexer.PunctuationToken.rbrace, 'Expected closing brace')?

	return ast.RecordLiteralExpr{
		name:     name_token.get_value()
		fields:   fields
		position: ep.get_current_position()
	}
}
