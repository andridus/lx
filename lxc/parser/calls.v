module parser

import ast
import lexer

pub fn (mut p Parser) parse_call() ast.Expr {
	if p.check_and_handle_error_token() {
		return ast.LiteralExpr{
			value: ast.Literal(ast.NilLiteral{})
		}
	}
	mut expr := p.parse_primary()
	for {
		if p.match(lexer.PunctuationToken.lparen) {
			expr = p.finish_call(expr)
		} else if p.match(lexer.OperatorToken.dot) {
			if p.current is lexer.IdentToken {
				field := p.current.get_value()
				p.advance()
				if p.match(lexer.PunctuationToken.lparen) {
					mut args := []ast.Expr{}
					if !p.check(lexer.PunctuationToken.rparen) {
						for {
							args << p.parse_expression()
							if !p.match(lexer.PunctuationToken.comma) {
								break
							}
						}
					}
					p.consume(lexer.PunctuationToken.rparen, 'Expect ")" after arguments') or {
						return expr
					}
					return ast.CallExpr{
						function: ast.RecordAccessExpr{
							record: expr
							field: field
							position: ast.Position{
								line: 1
								column: p.position
							}
						}
						arguments: args
						position: ast.Position{
							line: 1
							column: p.position
						}
					}
				} else {
					expr = ast.RecordAccessExpr{
						record: expr
						field: field
						position: ast.Position{
							line: 1
							column: p.position
						}
					}
				}
			} else {
				break
			}
		} else {
			break
		}
	}
	return expr
}

pub fn (mut p Parser) finish_call(callee ast.Expr) ast.Expr {
	mut arguments := []ast.Expr{}
	if !p.check(lexer.PunctuationToken.rparen) {
		for {
			arguments << p.parse_expression()
			if !p.match(lexer.PunctuationToken.comma) {
				break
			}
		}
	}
	p.consume(lexer.PunctuationToken.rparen, 'Expect ")" after arguments') or {
		return callee
	}
	return ast.CallExpr{
		function: callee
		arguments: arguments
		position: ast.Position{
			line: 1
			column: p.position
		}
	}
}