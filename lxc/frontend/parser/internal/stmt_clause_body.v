module internal

import ast
import lexer

// parse_statement_block parses a block of statements
fn (mut sp StatementParser) parse_statement_block() ?[]ast.Stmt {
	mut statements := []ast.Stmt{}

	for !sp.is_at_end() {
		// Skip newlines
		if sp.current is lexer.NewlineToken {
			sp.advance()
			continue
		}

		// Check for end keywords that should break
		if sp.current is lexer.KeywordToken {
			keyword_token := sp.current as lexer.KeywordToken
			if keyword_token.value == .end_ {
				break
			}
			if keyword_token.value == .else_ {
				break
			}
			if keyword_token.value == .after {
				break
			}
		}

		// Check for pattern tokens that indicate next case
		if sp.is_next_case_pattern() {
			break
		}

		// Parse assignment statements
		if sp.current is lexer.IdentToken && sp.peek() is lexer.OperatorToken {
			op_token := sp.peek() as lexer.OperatorToken
			if op_token.value == .assign || op_token.value == .type_cons {
				stmt := sp.parse_assignment_statement()?
				statements << stmt
				continue
			}
		}

		// Parse expression (including control flow) and wrap it in ExprStmt
		expr := sp.parse_clause_expression()?
		statements << ast.ExprStmt{
			expr: expr
		}

		// If this is the last statement (expression), break to avoid infinite loop
		if sp.is_last_statement_in_clause() {
			break
		}
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

// is_next_case_pattern checks if the current token starts a new case pattern
fn (sp StatementParser) is_next_case_pattern() bool {
	return sp.is_pattern_followed_by_arrow(sp.position)
}

// is_pattern_followed_by_arrow checks if there's a pattern at the given position followed by ->
fn (sp StatementParser) is_pattern_followed_by_arrow(pos int) bool {
	if pos >= sp.tokens.len {
		return false
	}

	// Check for simple patterns (literals, identifiers, atoms)
	if sp.tokens[pos] is lexer.IntToken || sp.tokens[pos] is lexer.StringToken
		|| sp.tokens[pos] is lexer.IdentToken || sp.tokens[pos] is lexer.AtomToken {
		// Look ahead to see if this is a pattern (followed by ->)
		mut lookahead_pos := pos + 1
		if lookahead_pos < sp.tokens.len {
			if sp.tokens[lookahead_pos] is lexer.OperatorToken {
				op_token := sp.tokens[lookahead_pos] as lexer.OperatorToken
				if op_token.value == .arrow {
					return true
				}
			}
		}
	}

	// Check for tuple patterns {pattern, pattern, ...}
	if sp.tokens[pos] is lexer.PunctuationToken {
		punc_token := sp.tokens[pos] as lexer.PunctuationToken
		if punc_token.value == .lbrace {
			// Find the matching closing brace
			mut brace_count := 1
			mut scan_pos := pos + 1
			for scan_pos < sp.tokens.len && brace_count > 0 {
				if sp.tokens[scan_pos] is lexer.PunctuationToken {
					p := sp.tokens[scan_pos] as lexer.PunctuationToken
					if p.value == .lbrace {
						brace_count++
					} else if p.value == .rbrace {
						brace_count--
					}
				}
				scan_pos++
			}

			// Check if there's an arrow after the closing brace
			if brace_count == 0 && scan_pos < sp.tokens.len {
				if sp.tokens[scan_pos] is lexer.OperatorToken {
					op_token := sp.tokens[scan_pos] as lexer.OperatorToken
					if op_token.value == .arrow {
						return true
					}
				}
			}
		}
	}

	// Check for list patterns [pattern, pattern, ...]
	if sp.tokens[pos] is lexer.PunctuationToken {
		punc_token := sp.tokens[pos] as lexer.PunctuationToken
		if punc_token.value == .lbracket {
			// Find the matching closing bracket
			mut bracket_count := 1
			mut scan_pos := pos + 1
			for scan_pos < sp.tokens.len && bracket_count > 0 {
				if sp.tokens[scan_pos] is lexer.PunctuationToken {
					p := sp.tokens[scan_pos] as lexer.PunctuationToken
					if p.value == .lbracket {
						bracket_count++
					} else if p.value == .rbracket {
						bracket_count--
					}
				}
				scan_pos++
			}

			// Check if there's an arrow after the closing bracket
			if bracket_count == 0 && scan_pos < sp.tokens.len {
				if sp.tokens[scan_pos] is lexer.OperatorToken {
					op_token := sp.tokens[scan_pos] as lexer.OperatorToken
					if op_token.value == .arrow {
						return true
					}
				}
			}
		}
	}

	return false
}

// is_last_statement_in_clause checks if we should stop parsing statements in a clause
fn (sp StatementParser) is_last_statement_in_clause() bool {
	// Skip newlines to look ahead
	mut pos := sp.position
	for pos < sp.tokens.len && sp.tokens[pos] is lexer.NewlineToken {
		pos++
	}

	if pos >= sp.tokens.len {
		return true
	}

	// Check if the next meaningful token is a case pattern or end keyword
	if sp.tokens[pos] is lexer.KeywordToken {
		keyword_token := sp.tokens[pos] as lexer.KeywordToken
		if keyword_token.value == .end_ || keyword_token.value == .else_ || keyword_token.value == .after {
			return true
		}
	}

	// Check if next token starts a new case pattern (using the improved pattern detection)
	return sp.is_pattern_followed_by_arrow(pos)
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
				.match_ {
					sp.parse_match_rescue_expression()
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
		then_body: ast.BlockExpr{
			body:     then_body
			position: sp.get_current_position()
		}
		else_body: ast.BlockExpr{
			body:     else_body
			position: sp.get_current_position()
		}
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
			body:     ast.BlockExpr{
				body:     body
				position: sp.get_current_position()
			}
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
		body:      ast.BlockExpr{
			body:     body
			position: sp.get_current_position()
		}
		else_body: ast.BlockExpr{
			body:     else_body
			position: sp.get_current_position()
		}
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
		body:       ast.BlockExpr{
			body:     body
			position: sp.get_current_position()
		}
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
			body:     ast.BlockExpr{
				body:     body
				position: sp.get_current_position()
			}
			position: sp.get_current_position()
		}
	}

	mut timeout := ?ast.TimeoutClause(none)
	if sp.match(lexer.keyword(.after)) {
		timeout_expr := sp.parse_expression()?
		sp.consume(lexer.operator(.arrow), 'Expected -> after timeout expression')?
		timeout_body := sp.parse_statement_block()?

		timeout = ast.TimeoutClause{
			timeout:  timeout_expr
			body:     ast.BlockExpr{
				body:     timeout_body
				position: sp.get_current_position()
			}
			position: sp.get_current_position()
		}
	}

	sp.consume(lexer.keyword(.end_), 'Expected end after receive expression')?

	return ast.ReceiveExpr{
		cases:    cases
		timeout:  timeout
		position: sp.get_current_position()
	}
}

// parse_match_rescue_expression parses match rescue expressions
// Syntax: match pattern <- value rescue variable do rescue_body end
// Or simple match: match pattern <- value
fn (mut sp StatementParser) parse_match_rescue_expression() ?ast.Expr {
	sp.advance() // consume 'match'

	pattern := sp.parse_pattern()?
	sp.consume(lexer.operator(.pattern_match), 'Expected <- in match expression')?
	value := sp.parse_expression()?

	// Check if this is a simple match or match rescue
	if sp.check(lexer.keyword(.rescue)) {
		// Match rescue version
		sp.consume(lexer.keyword(.rescue), 'Expected rescue after match expression')?

		// Parse rescue variable
		if sp.current !is lexer.IdentToken {
			sp.add_error('Expected variable name after rescue', 'Got ${sp.current.str()}')
			return none
		}
		rescue_var_token := sp.current as lexer.IdentToken
		rescue_var := rescue_var_token.value
		sp.advance()

		sp.consume(lexer.keyword(.do_), 'Expected do after rescue variable')?

		rescue_body := sp.parse_statement_block()?

		sp.consume(lexer.keyword(.end_), 'Expected end after rescue body')?

		return ast.MatchRescueExpr{
			pattern:     pattern
			value:       value
			rescue_var:  rescue_var
			rescue_body: ast.BlockExpr{
				body:     rescue_body
				position: sp.get_current_position()
			}
			position:    sp.get_current_position()
		}
	} else {
		// Simple match version
		return ast.SimpleMatchExpr{
			pattern:  pattern
			value:    value
			position: sp.get_current_position()
		}
	}
}
