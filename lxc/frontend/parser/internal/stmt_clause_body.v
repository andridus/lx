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
			if keyword_token.value == .else_ {
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
// This is similar to parse_simple_expression but supports tuples and control flow
fn (mut sp StatementParser) parse_clause_expression() ?ast.Expr {
	return match sp.current {
		lexer.KeywordToken {
			keyword_token := sp.current as lexer.KeywordToken
			match keyword_token.value {
				.if_ {
					sp.parse_if_expression()
				}
				.case_ {
					sp.parse_case_expression()
				}
				.with {
					sp.parse_with_expression()
				}
				.for_ {
					sp.parse_for_expression()
				}
				.receive {
					sp.parse_receive_expression()
				}
				else {
					sp.parse_restricted_expression()
				}
			}
		}
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

// parse_if_expression parses if expressions in statement context
fn (mut sp StatementParser) parse_if_expression() ?ast.Expr {
	sp.advance() // consume 'if'

	condition := sp.parse_expression()?
	sp.consume(lexer.keyword(.do_), 'Expected do after if condition')?

	then_body := sp.parse_statement_block()?

	mut else_body := []ast.Stmt{}
	if sp.match(lexer.keyword(.else_)) {
		else_body = sp.parse_statement_block()?
	}

	sp.consume(lexer.keyword(.end_), 'Expected end after if expression')?

	return ast.IfExpr{
		condition: condition
		then_body: then_body
		else_body: else_body
		position:  sp.get_current_position()
	}
}

// parse_case_expression parses case expressions in statement context
fn (mut sp StatementParser) parse_case_expression() ?ast.Expr {
	sp.advance() // consume 'case'

	value := sp.parse_expression()?
	sp.consume(lexer.keyword(.do_), 'Expected do after case value')?

	mut cases := []ast.MatchCase{}
	for !sp.check(lexer.keyword(.end_)) {
		pattern := sp.parse_pattern()?

		mut guard := ast.Expr(ast.LiteralExpr{
			value: ast.BooleanLiteral{
				value: true
			}
		})
		if sp.match(lexer.keyword(.when)) {
			guard = sp.parse_expression()?
		}

		sp.consume(lexer.operator(.arrow), 'Expected -> after pattern')?

		body := sp.parse_statement_block()?

		cases << ast.MatchCase{
			pattern:  pattern
			guard:    guard
			body:     body
			position: sp.get_current_position()
		}
	}

	sp.consume(lexer.keyword(.end_), 'Expected end after case expression')?

	return ast.CaseExpr{
		value:    value
		cases:    cases
		position: sp.get_current_position()
	}
}

// parse_with_expression parses with expressions in statement context
fn (mut sp StatementParser) parse_with_expression() ?ast.Expr {
	sp.advance() // consume 'with'

	mut bindings := []ast.WithBinding{}
	for !sp.check(lexer.keyword(.do_)) {
		pattern := sp.parse_pattern()?
		sp.consume(lexer.operator(.pattern_match), 'Expected <- in with binding')?
		value := sp.parse_expression()?

		bindings << ast.WithBinding{
			pattern:  pattern
			value:    value
			position: sp.get_current_position()
		}

		if !sp.match(lexer.punctuation(.comma)) {
			break
		}
	}

	sp.consume(lexer.keyword(.do_), 'Expected do after with bindings')?

	body := sp.parse_statement_block()?

	mut else_body := []ast.Stmt{}
	if sp.match(lexer.keyword(.else_)) {
		else_body = sp.parse_statement_block()?
	}

	sp.consume(lexer.keyword(.end_), 'Expected end after with expression')?

	return ast.WithExpr{
		bindings:  bindings
		body:      body
		else_body: else_body
		position:  sp.get_current_position()
	}
}

// parse_for_expression parses for expressions in statement context
fn (mut sp StatementParser) parse_for_expression() ?ast.Expr {
	sp.advance() // consume 'for'

	pattern := sp.parse_pattern()?
	sp.consume(lexer.keyword(.in), 'Expected in after pattern')?

	collection := sp.parse_expression()?

	mut guard := ast.Expr(ast.LiteralExpr{
		value: ast.BooleanLiteral{
			value: true
		}
	})
	if sp.match(lexer.keyword(.when)) {
		guard = sp.parse_expression()?
	}

	sp.consume(lexer.keyword(.do_), 'Expected do after for expression')?

	body := sp.parse_statement_block()?

	sp.consume(lexer.keyword(.end_), 'Expected end after for expression')?

	return ast.ForExpr{
		pattern:    pattern
		collection: collection
		guard:      guard
		body:       body
		position:   sp.get_current_position()
	}
}

// parse_receive_expression parses receive expressions in statement context
fn (mut sp StatementParser) parse_receive_expression() ?ast.Expr {
	sp.advance() // consume 'receive'
	sp.consume(lexer.keyword(.do_), 'Expected do after receive')?

	mut cases := []ast.ReceiveCase{}
	for !sp.check(lexer.keyword(.after)) && !sp.check(lexer.keyword(.end_)) {
		pattern := sp.parse_pattern()?

		mut guard := ast.Expr(ast.LiteralExpr{
			value: ast.BooleanLiteral{
				value: true
			}
		})
		if sp.match(lexer.keyword(.when)) {
			guard = sp.parse_expression()?
		}

		sp.consume(lexer.operator(.arrow), 'Expected -> after pattern')?

		body := sp.parse_statement_block()?

		cases << ast.ReceiveCase{
			pattern:  pattern
			guard:    guard
			body:     body
			position: sp.get_current_position()
		}
	}

	mut timeout := ast.Expr(ast.LiteralExpr{
		value: ast.IntegerLiteral{
			value: 0
		}
	})
	if sp.match(lexer.keyword(.after)) {
		timeout = sp.parse_expression()?
		sp.consume(lexer.keyword(.do_), 'Expected do after timeout')?
		// timeout_body := sp.parse_statement_block()?
		// For now, we'll use the timeout expression directly
		// In a full implementation, we'd need to handle the timeout body
	}

	sp.consume(lexer.keyword(.end_), 'Expected end after receive expression')?

	return ast.ReceiveExpr{
		cases:    cases
		timeout:  timeout
		position: sp.get_current_position()
	}
}
