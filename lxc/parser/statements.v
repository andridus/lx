module parser

import ast

pub fn (mut p Parser) parse() []ast.Stmt {
	mut statements := []ast.Stmt{}
	mut last_position := p.position

	for !p.is_at_end() {
		stmt := p.parse_statement()
		if stmt is ast.ExprStmt {
			statements << stmt
		}

		// Check if we're stuck (position didn't change)
		if p.position == last_position && !p.is_at_end() {
			p.advance()
		}
		last_position = p.position
	}
	return statements
}

pub fn (mut p Parser) parse_statement() ast.Stmt {
	// Always check for error tokens at the start of a statement
	p.check_and_handle_error_token()
	return ast.ExprStmt{
		expr: p.parse_expression()
	}
}