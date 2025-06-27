module parser

import ast
import lexer
import errors

pub fn (mut p Parser) parse_expression() ast.Expr {
	// Consume and register all consecutive ErrorTokens
	mut found_error := false
	for p.current is lexer.ErrorToken {
		p.check_and_handle_error_token()
		found_error = true
	}
	if found_error {
		return ast.LiteralExpr{
			value: ast.Literal(ast.NilLiteral{})
		}
	}
	return p.parse_assignment()
}

pub fn (mut p Parser) parse_assignment() ast.Expr {
	if p.check_and_handle_error_token() {
		return ast.LiteralExpr{
			value: ast.Literal(ast.NilLiteral{})
		}
	}
	mut expr := p.parse_or()

	if p.match(lexer.OperatorToken.assign) {
		value := p.parse_assignment()

		if expr is ast.VariableExpr {
			ve := expr as ast.VariableExpr
			return ast.AssignExpr{
				name: ve.name
				value: value
				position: ast.Position{
					line: 1
					column: p.position
				}
			}
		}

		pos := ast.Position{
			line: 1
			column: p.position
		}

		comp_error := errors.new_compilation_error(
			errors.ErrorKind(errors.SyntaxError{
				message: 'Invalid assignment target'
				expected: 'variable'
				found: expr.str()
			}),
			pos,
			'Invalid assignment target'
		)

		p.errors << comp_error
	}

	return expr
}

pub fn (mut p Parser) parse_or() ast.Expr {
	if p.check_and_handle_error_token() {
		return ast.LiteralExpr{
			value: ast.Literal(ast.NilLiteral{})
		}
	}
	mut expr := p.parse_and()

	for p.match(lexer.OperatorToken.or_) || p.match(lexer.OperatorToken.orelse) {
		operator := p.tokens[p.position - 1]
		right := p.parse_and()

		op_token := operator as lexer.OperatorToken
		op := if op_token == lexer.OperatorToken.or_ || op_token == lexer.OperatorToken.orelse {
			ast.BinaryOp.or
		} else {
			ast.BinaryOp.or
		}

		expr = ast.BinaryExpr{
			left: expr
			op: op
			right: right
			position: ast.Position{
				line: 1
				column: p.position
			}
		}
	}

	return expr
}

pub fn (mut p Parser) parse_and() ast.Expr {
	if p.check_and_handle_error_token() {
		return ast.LiteralExpr{
			value: ast.Literal(ast.NilLiteral{})
		}
	}
	mut expr := p.parse_equality()

	for p.match(lexer.OperatorToken.and_) || p.match(lexer.OperatorToken.andalso) {
		operator := p.tokens[p.position - 1]
		right := p.parse_equality()

		op_token := operator as lexer.OperatorToken
		op := if op_token == lexer.OperatorToken.and_ || op_token == lexer.OperatorToken.andalso {
			ast.BinaryOp.and
		} else {
			ast.BinaryOp.and
		}

		expr = ast.BinaryExpr{
			left: expr
			op: op
			right: right
			position: ast.Position{
				line: 1
				column: p.position
			}
		}
	}

	return expr
}

pub fn (mut p Parser) parse_equality() ast.Expr {
	if p.check_and_handle_error_token() {
		return ast.LiteralExpr{
			value: ast.Literal(ast.NilLiteral{})
		}
	}
	mut expr := p.parse_comparison()

	for p.match(lexer.OperatorToken.neq) || p.match(lexer.OperatorToken.eq) {
		operator := p.tokens[p.position - 1]
		right := p.parse_comparison()

		op_token := operator as lexer.OperatorToken
		op := if op_token == lexer.OperatorToken.eq {
			ast.BinaryOp.equal
		} else if op_token == lexer.OperatorToken.neq {
			ast.BinaryOp.not_equal
		} else {
			ast.BinaryOp.equal
		}

		expr = ast.BinaryExpr{
			left: expr
			op: op
			right: right
			position: ast.Position{
				line: 1
				column: p.position
			}
		}
	}

	return expr
}

pub fn (mut p Parser) parse_comparison() ast.Expr {
	if p.check_and_handle_error_token() {
		return ast.LiteralExpr{
			value: ast.Literal(ast.NilLiteral{})
		}
	}
	mut expr := p.parse_term()

	for p.match(lexer.OperatorToken.gt) || p.match(lexer.OperatorToken.geq) ||
		p.match(lexer.OperatorToken.lt) || p.match(lexer.OperatorToken.leq) {
		operator := p.tokens[p.position - 1]
		right := p.parse_term()

		op_token := operator as lexer.OperatorToken
		op := if op_token == lexer.OperatorToken.gt {
			ast.BinaryOp.greater_than
		} else if op_token == lexer.OperatorToken.geq {
			ast.BinaryOp.greater_equal
		} else if op_token == lexer.OperatorToken.lt {
			ast.BinaryOp.less_than
		} else if op_token == lexer.OperatorToken.leq {
			ast.BinaryOp.less_equal
		} else {
			ast.BinaryOp.less_than
		}

		expr = ast.BinaryExpr{
			left: expr
			op: op
			right: right
			position: ast.Position{
				line: 1
				column: p.position
			}
		}
	}

	return expr
}

pub fn (mut p Parser) parse_term() ast.Expr {
	if p.check_and_handle_error_token() {
		return ast.LiteralExpr{
			value: ast.Literal(ast.NilLiteral{})
		}
	}
	mut expr := p.parse_factor()

	for p.match(lexer.OperatorToken.minus) || p.match(lexer.OperatorToken.plus) {
		operator := p.tokens[p.position - 1]
		right := p.parse_factor()

		op_token := operator as lexer.OperatorToken
		op := if op_token == lexer.OperatorToken.plus {
			ast.BinaryOp.add
		} else if op_token == lexer.OperatorToken.minus {
			ast.BinaryOp.subtract
		} else {
			ast.BinaryOp.add
		}

		expr = ast.BinaryExpr{
			left: expr
			op: op
			right: right
			position: ast.Position{
				line: 1
				column: p.position
			}
		}
	}

	return expr
}

pub fn (mut p Parser) parse_factor() ast.Expr {
	if p.check_and_handle_error_token() {
		return ast.LiteralExpr{
			value: ast.Literal(ast.NilLiteral{})
		}
	}
	mut expr := p.parse_unary()

	for p.match(lexer.OperatorToken.div) || p.match(lexer.OperatorToken.mult) {
		operator := p.tokens[p.position - 1]
		right := p.parse_unary()

		op_token := operator as lexer.OperatorToken
		op := if op_token == lexer.OperatorToken.mult {
			ast.BinaryOp.multiply
		} else if op_token == lexer.OperatorToken.div {
			ast.BinaryOp.divide
		} else {
			ast.BinaryOp.multiply
		}

		expr = ast.BinaryExpr{
			left: expr
			op: op
			right: right
			position: ast.Position{
				line: 1
				column: p.position
			}
		}
	}

	return expr
}

pub fn (mut p Parser) parse_unary() ast.Expr {
	if p.check_and_handle_error_token() {
		return ast.LiteralExpr{
			value: ast.Literal(ast.NilLiteral{})
		}
	}
	if p.match(lexer.OperatorToken.not_) || p.match(lexer.OperatorToken.minus) {
		operator := p.tokens[p.position - 1]
		right := p.parse_unary()
		op_token := operator as lexer.OperatorToken
		op := if op_token == lexer.OperatorToken.not_ {
			ast.BinaryOp.and
		} else if op_token == lexer.OperatorToken.minus {
			ast.BinaryOp.subtract
		} else {
			ast.BinaryOp.and
		}
		return ast.BinaryExpr{
			left: ast.LiteralExpr{
				value: ast.Literal(ast.BooleanLiteral{
					value: true
				})
			}
			op: op
			right: right
			position: ast.Position{
				line: 1
				column: p.position
			}
		}
	}
	return p.parse_call()
}