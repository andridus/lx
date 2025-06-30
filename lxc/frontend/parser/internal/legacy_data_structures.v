module internal

import ast
import lexer

pub fn (mut p Parser) parse_tuple() ast.Expr {
	mut elements := []ast.Expr{}
	for !p.check(lexer.PunctuationToken.rbrace) && !p.is_at_end() {
		mut expr_parser := new_expression_parser(p.tokens[p.position..])
		expr_parser.position = 0
		expr_parser.current = p.current
		element := expr_parser.parse_primary()
		elements << element
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
	p.consume(lexer.PunctuationToken.rbrace, 'Expected "}" after tuple elements') or {
		return ast.TupleExpr{
			elements: elements
			position: ast.Position{}
		}
	}
	return ast.TupleExpr{
		elements: elements
		position: ast.Position{}
	}
}

pub fn (mut p Parser) parse_list() ast.Expr {
	mut elements := []ast.Expr{}
	for !p.check(lexer.PunctuationToken.rbracket) && !p.is_at_end() {
		mut expr_parser := new_expression_parser(p.tokens[p.position..])
		expr_parser.position = 0
		expr_parser.current = p.current
		element := expr_parser.parse_primary()
		elements << element
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
	p.consume(lexer.PunctuationToken.rbracket, 'Expected "]" after list elements') or {
		return ast.ListLiteralExpr{
			elements: elements
			position: ast.Position{}
		}
	}
	return ast.ListLiteralExpr{
		elements: elements
		position: ast.Position{}
	}
}

pub fn (mut p Parser) parse_cons() ast.Expr {
	mut expr_parser := new_expression_parser(p.tokens[p.position..])
	expr_parser.position = 0
	expr_parser.current = p.current
	head := expr_parser.parse_primary()
	p.position += expr_parser.position
	if p.position < p.tokens.len {
		p.current = p.tokens[p.position]
	} else {
		p.current = lexer.EOFToken{}
	}
	p.consume(lexer.OperatorToken.record_update, 'Expect "|" in cons expression') or {
		return ast.ListConsExpr{
			head:     head
			tail:     ast.ListEmptyExpr{}
			position: ast.Position{}
		}
	}
	mut expr_parser2 := new_expression_parser(p.tokens[p.position..])
	expr_parser2.position = 0
	expr_parser2.current = p.current
	tail := expr_parser2.parse_primary()
	p.position += expr_parser2.position
	if p.position < p.tokens.len {
		p.current = p.tokens[p.position]
	} else {
		p.current = lexer.EOFToken{}
	}
	p.consume(lexer.PunctuationToken.rbracket, 'Expect "]" after cons expression') or {
		return ast.ListConsExpr{
			head:     head
			tail:     tail
			position: ast.Position{}
		}
	}
	return ast.ListConsExpr{
		head:     head
		tail:     tail
		position: ast.Position{}
	}
}

pub fn (mut p Parser) parse_map() ast.Expr {
	mut entries := []ast.MapEntry{}
	for !p.check(lexer.PunctuationToken.rbrace) && !p.is_at_end() {
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
		if p.match(lexer.OperatorToken.arrow) {
			mut expr_parser2 := new_expression_parser(p.tokens[p.position..])
			expr_parser2.position = 0
			expr_parser2.current = p.current
			value := expr_parser2.parse_primary()
			p.position += expr_parser2.position
			if p.position < p.tokens.len {
				p.current = p.tokens[p.position]
			} else {
				p.current = lexer.EOFToken{}
			}
			entries << ast.MapEntry{
				key:      key
				value:    value
				position: ast.Position{}
			}
		} else if p.match(lexer.PunctuationToken.colon) {
			mut expr_parser2 := new_expression_parser(p.tokens[p.position..])
			expr_parser2.position = 0
			expr_parser2.current = p.current
			value := expr_parser2.parse_primary()
			p.position += expr_parser2.position
			if p.position < p.tokens.len {
				p.current = p.tokens[p.position]
			} else {
				p.current = lexer.EOFToken{}
			}
			entries << ast.MapEntry{
				key:      key
				value:    value
				position: ast.Position{}
			}
		} else {
			break
		}
		if !p.match(lexer.PunctuationToken.comma) {
			break
		}
	}
	p.consume(lexer.PunctuationToken.rbrace, 'Expected "}" after map entries') or {
		return ast.MapLiteralExpr{
			entries:  entries
			position: ast.Position{}
		}
	}
	return ast.MapLiteralExpr{
		entries:  entries
		position: ast.Position{}
	}
}
