module parser

import ast
import lexer
import errors

pub fn (mut p Parser) parse_primary() ast.Expr {
	if p.check_and_handle_error_token() {
		return ast.LiteralExpr{
			value: ast.Literal(ast.NilLiteral{})
		}
	}
	if p.match(lexer.KeywordToken.false_) {
		return ast.LiteralExpr{
			value: ast.Literal(ast.BooleanLiteral{
				value: false
			})
		}
	}
	if p.match(lexer.KeywordToken.true_) {
		return ast.LiteralExpr{
			value: ast.Literal(ast.BooleanLiteral{
				value: true
			})
		}
	}
	if p.match(lexer.KeywordToken.nil_) {
		return ast.LiteralExpr{
			value: ast.Literal(ast.NilLiteral{})
		}
	}
	if p.current is lexer.StringToken {
		p.advance()
		return ast.LiteralExpr{
			value: ast.Literal(ast.StringLiteral{
				value: p.tokens[p.position - 1].get_value()
			})
		}
	}
	if p.current is lexer.IntToken {
		p.advance()
		value := p.tokens[p.position - 1].get_numeric_value() or { 0.0 }
		return ast.LiteralExpr{
			value: ast.Literal(ast.IntegerLiteral{
				value: int(value)
			})
		}
	}
	if p.current is lexer.FloatToken {
		p.advance()
		value := p.tokens[p.position - 1].get_numeric_value() or { 0.0 }
		return ast.LiteralExpr{
			value: ast.Literal(ast.FloatLiteral{
				value: value
			})
		}
	}
	if p.current is lexer.AtomToken {
		p.advance()
		return ast.LiteralExpr{
			value: ast.Literal(ast.AtomLiteral{
				value: p.tokens[p.position - 1].get_value()
			})
		}
	}
	if p.current is lexer.IdentToken {
		p.advance()
		return ast.VariableExpr{
			name: p.tokens[p.position - 1].get_value()
		}
	}
	if p.current is lexer.ErrorToken {
		pos := ast.Position{
			line:   1
			column: p.position + 1
		}
		comp_error := errors.new_compilation_error(errors.ErrorKind(errors.SyntaxError{
			message:  (p.current as lexer.ErrorToken).message
			expected: 'expression'
			found:    p.current.str()
		}), pos, 'Unexpected token ' + p.current.str())
		p.errors << comp_error
		p.advance()
		return ast.LiteralExpr{
			value: ast.Literal(ast.NilLiteral{})
		}
	}
	if p.match(lexer.PunctuationToken.lbrace) {
		if p.check(lexer.OperatorToken.record_update) {
			p.advance()
			return p.parse_map()
		} else {
			return p.parse_tuple()
		}
	}
	if p.match(lexer.PunctuationToken.lbracket) {
		if p.peek() is lexer.OperatorToken
			&& (p.peek() as lexer.OperatorToken) == lexer.OperatorToken.record_update {
			return p.parse_cons()
		} else {
			return p.parse_list()
		}
	}
	if p.match(lexer.PunctuationToken.lparen) {
		expr := p.parse_primary()
		p.consume(lexer.PunctuationToken.rparen, 'Expect ")" after expression') or { return expr }
		return expr
	}
	pos := ast.Position{
		line:   1
		column: p.position + 1
	}
	comp_error := errors.new_compilation_error(errors.ErrorKind(errors.SyntaxError{
		message:  'Unexpected token'
		expected: 'expression'
		found:    p.current.str()
	}), pos, 'Unexpected token ${p.current.str()}')
	p.errors << comp_error
	return ast.LiteralExpr{
		value: ast.Literal(ast.NilLiteral{})
	}
}

// parse_parenthesized parses parenthesized expressions
pub fn (mut p Parser) parse_parenthesized() ast.Expr {
	if p.match(lexer.PunctuationToken.lparen) {
		mut expr_parser := new_expression_parser(p.tokens[p.position..])
		expr_parser.position = 0
		expr_parser.current = p.current
		expr := expr_parser.parse_primary()
		p.position += expr_parser.position
		if p.position < p.tokens.len {
			p.current = p.tokens[p.position]
		} else {
			p.current = lexer.EOFToken{}
		}
		p.consume(lexer.PunctuationToken.rparen, 'Expect ")" after expression') or { return expr }
		return expr
	}
	return ast.LiteralExpr{ value: ast.NilLiteral{} }
}
