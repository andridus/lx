module internal

import ast
import lexer

// parse_statement_block parses a block of statements
fn (mut sp StatementParser) parse_statement_block() ?[]ast.Stmt {
	mut statements := []ast.Stmt{}

	for {
		if sp.current is lexer.KeywordToken {
			keyword_token := sp.current as lexer.KeywordToken
			if keyword_token.value == .end_ {
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
	// Skip newlines at the start of clause body
	for sp.current is lexer.NewlineToken {
		sp.advance()
	}

	mut statements := []ast.Stmt{}

	for !sp.is_at_end() {
		// Detecta início de nova cláusula multi-head: ( ... ) ->
		if sp.is_potential_new_clause_start() {
			break
		}
		if sp.check(lexer.keyword(.end_)) {
			break
		}
		// Se o próximo token é (, verificar se é início de nova cláusula antes de consumir
		if sp.check(lexer.punctuation(.lparen)) {
			if sp.is_potential_new_clause_start() {
				break
			}
		}
		// Parse assignment statements
		if sp.current is lexer.IdentToken && sp.peek() is lexer.OperatorToken {
			op_token := sp.peek() as lexer.OperatorToken
			if op_token.value == .assign || op_token.value == .type_cons {
				stmt := sp.parse_assignment_statement()?
				statements << stmt
			} else {
				expr := sp.parse_clause_expression()?
				statements << ast.ExprStmt{
					expr: expr
				}
			}
		} else {
			expr := sp.parse_clause_expression()?
			statements << ast.ExprStmt{
				expr: expr
			}
		}
	}

	if statements.len == 0 && !sp.is_potential_new_clause_start() && !sp.check(lexer.keyword(.end_)) {
		expr := sp.parse_simple_expression()?
		statements << ast.ExprStmt{
			expr: expr
		}
	}

	return statements
}

fn (sp StatementParser) is_potential_new_clause_start() bool {
	mut pos := sp.position
	if pos >= sp.tokens.len {
		return false
	}
	if sp.tokens[pos] !is lexer.PunctuationToken {
		return false
	}
	punc := sp.tokens[pos] as lexer.PunctuationToken
	if punc.value != .lparen {
		return false
	}
	// Procurar pelo fechamento do parêntese
	mut paren_count := 1
	pos++
	for pos < sp.tokens.len && paren_count > 0 {
		tok := sp.tokens[pos]
		if tok is lexer.PunctuationToken {
			p := tok as lexer.PunctuationToken
			if p.value == .lparen {
				paren_count++
			}
			if p.value == .rparen {
				paren_count--
			}
		}
		pos++
	}
	if paren_count != 0 {
		return false
	}
	if pos < sp.tokens.len && sp.tokens[pos] is lexer.OperatorToken {
		op := sp.tokens[pos] as lexer.OperatorToken
		if op.value == .arrow {
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

	// Save the position BEFORE consuming the identifier
	ident_position := sp.get_current_position()
	ident_token := sp.current as lexer.IdentToken
	name := ident_token.value
	sp.advance()

	mut type_annotation := ?ast.TypeExpression(none)

	// Check for type annotation
	if sp.check(lexer.operator(.type_cons)) {
		sp.advance() // consume '::'
		type_annotation = sp.parse_type_expression()?
	}

	// Parse assignment operator
	sp.consume(lexer.operator(.assign), 'Expected = in assignment')?

	// Parse value
	value := sp.parse_expression()?

	return ast.ExprStmt{
		expr: ast.AssignExpr{
			name:            name
			value:           value
			type_annotation: type_annotation
			position:        ident_position
		}
	}
}

// parse_clause_expression parses expressions for clause bodies
// This is similar to parse_simple_expression but supports tuples
fn (mut sp StatementParser) parse_clause_expression() ?ast.Expr {
	return match sp.current {
		lexer.PunctuationToken {
			punc_token := sp.current as lexer.PunctuationToken
			match punc_token.value {
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
	if !sp.check(lexer.punctuation(.rbrace)) {
		for {
			elements << sp.parse_simple_expression()?

			if !sp.match(lexer.punctuation(.comma)) {
				break
			}
		}
	}

	sp.consume(lexer.punctuation(.rbrace), 'Expected closing brace')?

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
		if keyword_token.value == .end_ {
			// Rollback to start position
			sp.position = start_pos
			sp.sync_current_token()
			// Return just the simple expression
			return sp.parse_simple_expression()
		}
	}

	return expr
}
