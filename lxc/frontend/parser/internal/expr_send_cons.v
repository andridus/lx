module internal

import ast
import lexer

// parse_send_expression parses message sending expressions
fn (mut ep ExpressionParser) parse_send_expression() ?ast.Expr {
	mut left := ep.parse_cons_expression()?

	for ep.match(lexer.operator(.send)) {
		right := ep.parse_cons_expression()?
		left = ast.SendExpr{
			pid:      left
			message:  right
			position: ep.get_current_position()
		}
	}

	return left
}

// parse_cons_expression parses list cons expressions
fn (mut ep ExpressionParser) parse_cons_expression() ?ast.Expr {
	mut left := ep.parse_primary_expression()?

	for ep.match(lexer.operator(.type_cons)) {
		right := ep.parse_primary_expression()?
		left = ast.ListConsExpr{
			head:     left
			tail:     right
			position: ep.get_current_position()
		}
	}

	return left
}
