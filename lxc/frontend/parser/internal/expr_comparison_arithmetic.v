module internal

import ast
import lexer

// parse_comparison_expression parses comparison expressions
fn (mut ep ExpressionParser) parse_comparison_expression() ?ast.Expr {
	mut left := ep.parse_concatenation_expression()?
	for ep.current is lexer.OperatorToken {
		op_token := ep.current as lexer.OperatorToken
		mut op := ast.BinaryOp.equal
		mut should_continue := false
		match op_token.value {
			.eq {
				op = .equal
				should_continue = true
			}
			.neq {
				op = .not_equal
				should_continue = true
			}
			.lt {
				op = .less_than
				should_continue = true
			}
			.gt {
				op = .greater_than
				should_continue = true
			}
			.leq {
				op = .less_equal
				should_continue = true
			}
			.geq {
				op = .greater_equal
				should_continue = true
			}
			else {
				break
			}
		}
		if !should_continue {
			break
		}
		ep.advance()
		right := ep.parse_concatenation_expression()?
		left = ast.BinaryExpr{
			left:     left
			op:       op
			right:    right
			position: ep.get_current_position()
		}
	}
	return left
}

// parse_concatenation_expression parses string concatenation expressions
fn (mut ep ExpressionParser) parse_concatenation_expression() ?ast.Expr {
	mut left := ep.parse_additive_expression()?

	for ep.match(lexer.operator(.concat)) {
		right := ep.parse_additive_expression()?
		left = ast.BinaryExpr{
			left:     left
			op:       .append
			right:    right
			position: ep.get_current_position()
		}
	}

	return left
}

// parse_additive_expression parses addition and subtraction expressions
fn (mut ep ExpressionParser) parse_additive_expression() ?ast.Expr {
	mut left := ep.parse_multiplicative_expression()?
	for ep.current is lexer.OperatorToken {
		op_token := ep.current as lexer.OperatorToken
		mut op := ast.BinaryOp.add
		mut should_continue := false

		match op_token.value {
			.plus {
				op = .add
				should_continue = true
			}
			.minus {
				op = .subtract
				should_continue = true
			}
			else {
				break
			}
		}

		if !should_continue {
			break
		}

		ep.advance()
		right := ep.parse_multiplicative_expression()?
		left = ast.BinaryExpr{
			left:     left
			op:       op
			right:    right
			position: ep.get_current_position()
		}
	}
	return left
}

// parse_multiplicative_expression parses multiplication and division expressions
fn (mut ep ExpressionParser) parse_multiplicative_expression() ?ast.Expr {
	mut left := ep.parse_unary_expression()?
	for ep.current is lexer.OperatorToken {
		op_token := ep.current as lexer.OperatorToken
		mut op := ast.BinaryOp.multiply
		mut should_continue := false

		match op_token.value {
			.mult {
				op = .multiply
				should_continue = true
			}
			.div {
				op = .divide
				should_continue = true
			}
			else {
				break
			}
		}

		if !should_continue {
			break
		}

		ep.advance()
		right := ep.parse_unary_expression()?
		left = ast.BinaryExpr{
			left:     left
			op:       op
			right:    right
			position: ep.get_current_position()
		}
	}
	return left
}

// parse_unary_expression parses unary expressions
fn (mut ep ExpressionParser) parse_unary_expression() ?ast.Expr {
	if ep.current is lexer.OperatorToken {
		op_token := ep.current as lexer.OperatorToken
		if op_token.value == .not_ {
			operand := ep.parse_unary_expression()?
			return ast.UnaryExpr{
				op:       .not
				operand:  operand
				position: ep.get_current_position()
			}
		}
	}

	return ep.parse_send_expression()
}
