module parser

import ast
import lexer

pub fn (mut p Parser) parse_tuple() ast.Expr {
	mut elements := []ast.Expr{}
	for !p.check(lexer.PunctuationToken.rbrace) && !p.is_at_end() {
		elements << p.parse_expression()
		if !p.match(lexer.PunctuationToken.comma) {
			break
		}
	}
	p.consume(lexer.PunctuationToken.rbrace, 'Expect "}" after tuple elements') or {
		return ast.TupleExpr{
			elements: elements
			position: ast.Position{
				line:   1
				column: p.position
			}
		}
	}
	return ast.TupleExpr{
		elements: elements
		position: ast.Position{
			line:   1
			column: p.position
		}
	}
}

pub fn (mut p Parser) parse_list() ast.Expr {
	mut elements := []ast.Expr{}
	for !p.check(lexer.PunctuationToken.rbracket) && !p.is_at_end() {
		elements << p.parse_expression()
		if !p.match(lexer.PunctuationToken.comma) {
			break
		}
	}
	p.consume(lexer.PunctuationToken.rbracket, 'Expect "]" after list elements') or {
		return ast.ListLiteralExpr{
			elements: elements
			position: ast.Position{
				line:   1
				column: p.position
			}
		}
	}
	return ast.ListLiteralExpr{
		elements: elements
		position: ast.Position{
			line:   1
			column: p.position
		}
	}
}

pub fn (mut p Parser) parse_cons() ast.Expr {
	head := p.parse_expression()
	p.consume(lexer.OperatorToken.record_update, 'Expect "|" in cons expression') or {
		return ast.ListConsExpr{
			head:     head
			tail:     ast.ListEmptyExpr{}
			position: ast.Position{
				line:   1
				column: p.position
			}
		}
	}
	tail := p.parse_expression()
	p.consume(lexer.PunctuationToken.rbracket, 'Expect "]" after cons expression') or {
		return ast.ListConsExpr{
			head:     head
			tail:     tail
			position: ast.Position{
				line:   1
				column: p.position
			}
		}
	}
	return ast.ListConsExpr{
		head:     head
		tail:     tail
		position: ast.Position{
			line:   1
			column: p.position
		}
	}
}

pub fn (mut p Parser) parse_map() ast.Expr {
	mut entries := []ast.MapEntry{}
	for !p.check(lexer.PunctuationToken.rbrace) && !p.is_at_end() {
		key := p.parse_expression()
		if p.match(lexer.OperatorToken.arrow) {
			value := p.parse_expression()
			entries << ast.MapEntry{
				key:      key
				value:    value
				position: ast.Position{
					line:   1
					column: p.position
				}
			}
		} else if p.match(lexer.PunctuationToken.colon) {
			value := p.parse_expression()
			entries << ast.MapEntry{
				key:      key
				value:    value
				position: ast.Position{
					line:   1
					column: p.position
				}
			}
		} else {
			p.consume(lexer.OperatorToken.arrow, 'Expect "=>" or ":" in map entry') or { break }
		}
		if !p.match(lexer.PunctuationToken.comma) {
			break
		}
	}
	p.consume(lexer.PunctuationToken.rbrace, 'Expect "}" after map entries') or {
		return ast.MapLiteralExpr{
			entries:  entries
			position: ast.Position{
				line:   1
				column: p.position
			}
		}
	}
	return ast.MapLiteralExpr{
		entries:  entries
		position: ast.Position{
			line:   1
			column: p.position
		}
	}
}
