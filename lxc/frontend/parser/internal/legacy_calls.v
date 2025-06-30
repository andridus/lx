module internal

import ast
import lexer

pub fn (mut p Parser) parse_call() ast.Expr {
	mut expr := p.parse_primary()

	// Handle postfix operations
	for {
		if p.match(lexer.PunctuationToken.lparen) {
			expr = p.finish_call(expr)
		} else if p.match(lexer.OperatorToken.dot) {
			if p.current is lexer.IdentToken {
				ident_token := p.current as lexer.IdentToken
				field := ident_token.value
				p.advance()
				expr = ast.RecordAccessExpr{
					record:   expr
					field:    field
					position: ast.Position{}
				}
			}
		} else {
			break
		}
	}

	return expr
}

pub fn (mut p Parser) finish_call(callee ast.Expr) ast.Expr {
	mut args := []ast.Expr{}

	if !p.check(lexer.PunctuationToken.rparen) {
		for {
			mut expr_parser := new_expression_parser(p.tokens[p.position..])
			expr_parser.position = 0
			expr_parser.current = p.current
			arg := expr_parser.parse_primary() // parse_primary para evitar recursão infinita
			args << arg
			p.position += expr_parser.position
			if p.position < p.tokens.len {
				p.current = p.tokens[p.position]
			} else {
				p.current = lexer.EOFToken{}
			}
			if !p.match(lexer.PunctuationToken.comma) {
				break
			}
		}
	}

	p.consume(lexer.PunctuationToken.rparen, 'Expected ")" after arguments') or {
		return ast.CallExpr{
			function:  callee
			arguments: args
			position:  ast.Position{}
		}
	}

	return ast.CallExpr{
		function:  callee
		arguments: args
		position:  ast.Position{}
	}
}

// parse_record_access parses record field access
pub fn (mut p Parser) parse_record_access() ast.Expr {
	record := p.parse_primary()

	if p.match(lexer.OperatorToken.dot) {
		if p.current is lexer.IdentToken {
			ident_token := p.current as lexer.IdentToken
			field := ident_token.value
			p.advance()
			return ast.RecordAccessExpr{
				record:   record
				field:    field
				position: ast.Position{}
			}
		}
	}

	return record
}

// parse_map_access parses map key access
pub fn (mut p Parser) parse_map_access() ast.Expr {
	map_expr := p.parse_primary()

	if p.match(lexer.PunctuationToken.lbracket) {
		mut expr_parser := new_expression_parser(p.tokens[p.position..])
		expr_parser.position = 0
		expr_parser.current = p.current
		key := expr_parser.parse_primary()
		p.position += expr_parser.position
		if p.position < p.tokens.len {
			p.current = p.tokens[p.position]
		} else {
			p.current = lexer.EOFToken{}
		}
		p.consume(lexer.PunctuationToken.rbracket, 'Expected "]" after map key') or {
			return ast.MapAccessExpr{
				map_expr: map_expr
				key:      key
				position: ast.Position{}
			}
		}
		return ast.MapAccessExpr{
			map_expr: map_expr
			key:      key
			position: ast.Position{}
		}
	}

	return map_expr
}
