module internal

import ast
import lexer

// parse_statement_block parses a block of statements
fn (mut sp StatementParser) parse_statement_block() ?[]ast.Stmt {
	mut statements := []ast.Stmt{}

	for {
		if sp.current is lexer.KeywordToken {
			keyword_token := sp.current as lexer.KeywordToken
			if keyword_token == .end_ {
				break
			}
		}

		stmt := sp.parse_statement()?
		statements << stmt
	}

	return statements
}

// parse_clause_body parses a block of statements until next clause or end
fn (mut sp StatementParser) parse_clause_body() ?[]ast.Stmt {
	mut statements := []ast.Stmt{}

	for !sp.is_at_end() {
		// Detecta início de nova cláusula multi-head: ( ... ) ->
		if sp.is_potential_new_clause_start() {
			break
		}
		if sp.check(lexer.KeywordToken.end_) {
			break
		}
		// Se o próximo token é (, verificar se é início de nova cláusula antes de consumir
		if sp.check(lexer.PunctuationToken.lparen) {
			if sp.is_potential_new_clause_start() {
				break
			}
		}
		// Parse assignment statements
		if sp.current is lexer.IdentToken && sp.peek().str() == 'Operator(=)' {
			stmt := sp.parse_assignment_statement()?
			statements << stmt
		} else {
			expr := sp.parse_clause_expression()?
			statements << ast.ExprStmt{
				expr: expr
			}
		}
	}

	if statements.len == 0 {
		expr := sp.parse_simple_expression()?
		statements << ast.ExprStmt{
			expr: expr
		}
	}

	return statements
}

// Detecta início de nova cláusula multi-head pelo padrão ( ... ) ->
fn (sp StatementParser) is_potential_new_clause_start() bool {
	mut pos := sp.position
	if pos >= sp.tokens.len {
		return false
	}
	if sp.tokens[pos] !is lexer.PunctuationToken {
		return false
	}
	punc := sp.tokens[pos] as lexer.PunctuationToken
	if punc != .lparen {
		return false
	}
	// Procurar pelo fechamento do parêntese
	mut paren_count := 1
	pos++
	for pos < sp.tokens.len && paren_count > 0 {
		tok := sp.tokens[pos]
		if tok is lexer.PunctuationToken {
			p := tok as lexer.PunctuationToken
			if p == .lparen {
				paren_count++
			}
			if p == .rparen {
				paren_count--
			}
		}
		pos++
	}
	if paren_count != 0 {
		return false
	}
	// Após fechar parêntese, deve vir ->
	if pos < sp.tokens.len && sp.tokens[pos] is lexer.OperatorToken {
		op := sp.tokens[pos] as lexer.OperatorToken
		if op == .arrow {
			return true
		}
	}
	return false
}

// parse_assignment_statement parses assignment statements
fn (mut sp StatementParser) parse_assignment_statement() ?ast.Stmt {
	// Parse variable name
	if !sp.current.is_identifier() {
		sp.add_error('Expected variable name', 'Got ${sp.current.str()}')
		return none
	}
	name := sp.current.get_value()
	sp.advance()

	// Parse assignment operator
	sp.consume(lexer.OperatorToken.assign, 'Expected = in assignment')?

	// Parse value
	value := sp.parse_expression()?

	return ast.ExprStmt{
		expr: ast.AssignExpr{
			name:     name
			value:    value
			position: sp.get_current_position()
		}
	}
}

// parse_clause_expression parses expressions for clause bodies
// This is similar to parse_simple_expression but supports tuples
fn (mut sp StatementParser) parse_clause_expression() ?ast.Expr {
	return match sp.current {
		lexer.PunctuationToken {
			punc_token := sp.current as lexer.PunctuationToken
			match punc_token {
				.lbrace {
					sp.parse_tuple_expression()
				}
				else {
					sp.add_error('Unexpected punctuation', 'Got ${sp.current.str()}')
					none
				}
			}
		}
		else {
			sp.parse_restricted_expression()
		}
	}
}

// parse_tuple_expression parses tuple expressions
fn (mut sp StatementParser) parse_tuple_expression() ?ast.Expr {
	sp.advance() // consume '{'

	mut elements := []ast.Expr{}
	if !sp.check(lexer.PunctuationToken.rbrace) {
		for {
			elements << sp.parse_simple_expression()?

			if !sp.match(lexer.PunctuationToken.comma) {
				break
			}
		}
	}

	sp.consume(lexer.PunctuationToken.rbrace, 'Expected closing brace')?

	return ast.TupleExpr{
		elements: elements
		position: sp.get_current_position()
	}
}

// parse_restricted_expression parses expressions but stops before specific tokens
// This prevents consuming tokens that belong to the next clause header
fn (mut sp StatementParser) parse_restricted_expression() ?ast.Expr {
	// Save current position
	start_pos := sp.position

	// Try to parse a simple expression first
	expr := sp.parse_simple_expression()?

	// Check if we consumed a token that should stop us
	if sp.current is lexer.KeywordToken {
		keyword_token := sp.current as lexer.KeywordToken
		if keyword_token == .end_ {
			// Rollback to start position
			sp.position = start_pos
			sp.sync_current_token()
			// Return just the simple expression
			return sp.parse_simple_expression()
		}
	}

	return expr
}
