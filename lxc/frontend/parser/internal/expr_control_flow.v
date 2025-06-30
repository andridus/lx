module internal

import ast
import lexer

// parse_if_expression parses if expressions
fn (mut ep ExpressionParser) parse_if_expression() ?ast.Expr {
	ep.advance() // consume 'if'

	condition := ep.parse_expression()?
	ep.consume(lexer.keyword(.do_), 'Expected do after if condition')?

	then_body := ep.parse_statement_block()?

	mut else_body := []ast.Stmt{}
	if ep.match(lexer.keyword(.else_)) {
		else_body = ep.parse_statement_block()?
	}

	ep.consume(lexer.keyword(.end_), 'Expected end after if expression')?

	return ast.IfExpr{
		condition: condition
		then_body: then_body
		else_body: else_body
		position:  ep.get_current_position()
	}
}

// parse_case_expression parses case expressions
fn (mut ep ExpressionParser) parse_case_expression() ?ast.Expr {
	ep.advance() // consume 'case'

	value := ep.parse_expression()?
	ep.consume(lexer.keyword(.do_), 'Expected do after case value')?

	mut cases := []ast.MatchCase{}
	for !ep.check(lexer.keyword(.end_)) {
		pattern := ep.parse_pattern()?

		mut guard := ast.Expr(ast.LiteralExpr{
			value: ast.BooleanLiteral{
				value: true
			}
		})
		if ep.match(lexer.keyword(.when)) {
			guard = ep.parse_expression()?
		}

		ep.consume(lexer.operator(.arrow), 'Expected -> after pattern')?

		body := ep.parse_statement_block()?

		cases << ast.MatchCase{
			pattern:  pattern
			guard:    guard
			body:     body
			position: ep.get_current_position()
		}
	}

	ep.consume(lexer.keyword(.end_), 'Expected end after case expression')?

	return ast.CaseExpr{
		value:    value
		cases:    cases
		position: ep.get_current_position()
	}
}

// parse_with_expression parses with expressions
fn (mut ep ExpressionParser) parse_with_expression() ?ast.Expr {
	ep.advance() // consume 'with'

	mut bindings := []ast.WithBinding{}
	for !ep.check(lexer.keyword(.do_)) {
		pattern := ep.parse_pattern()?
		ep.consume(lexer.operator(.pattern_match), 'Expected <- in with binding')?
		value := ep.parse_expression()?

		bindings << ast.WithBinding{
			pattern:  pattern
			value:    value
			position: ep.get_current_position()
		}

		if !ep.match(lexer.punctuation(.comma)) {
			break
		}
	}

	ep.consume(lexer.keyword(.do_), 'Expected do after with bindings')?

	body := ep.parse_statement_block()?

	mut else_body := []ast.Stmt{}
	if ep.match(lexer.keyword(.else_)) {
		else_body = ep.parse_statement_block()?
	}

	ep.consume(lexer.keyword(.end_), 'Expected end after with expression')?

	return ast.WithExpr{
		bindings:  bindings
		body:      body
		else_body: else_body
		position:  ep.get_current_position()
	}
}

// parse_for_expression parses for expressions (list comprehensions)
fn (mut ep ExpressionParser) parse_for_expression() ?ast.Expr {
	ep.advance() // consume 'for'

	pattern := ep.parse_pattern()?
	ep.consume(lexer.keyword(.in), 'Expected in after pattern')?

	collection := ep.parse_expression()?

	mut guard := ast.Expr(ast.LiteralExpr{
		value: ast.BooleanLiteral{
			value: true
		}
	})
	if ep.match(lexer.keyword(.when)) {
		guard = ep.parse_expression()?
	}

	ep.consume(lexer.keyword(.do_), 'Expected do after for expression')?

	body := ep.parse_statement_block()?

	ep.consume(lexer.keyword(.end_), 'Expected end after for expression')?

	return ast.ForExpr{
		pattern:    pattern
		collection: collection
		guard:      guard
		body:       body
		position:   ep.get_current_position()
	}
}

// parse_receive_expression parses receive expressions
fn (mut ep ExpressionParser) parse_receive_expression() ?ast.Expr {
	ep.advance() // consume 'receive'
	ep.consume(lexer.keyword(.do_), 'Expected do after receive')?

	mut cases := []ast.ReceiveCase{}
	for !ep.check(lexer.keyword(.after)) && !ep.check(lexer.keyword(.end_)) {
		pattern := ep.parse_pattern()?

		mut guard := ast.Expr(ast.LiteralExpr{
			value: ast.BooleanLiteral{
				value: true
			}
		})
		if ep.match(lexer.keyword(.when)) {
			guard = ep.parse_expression()?
		}

		ep.consume(lexer.operator(.arrow), 'Expected -> after pattern')?

		body := ep.parse_statement_block()?

		cases << ast.ReceiveCase{
			pattern:  pattern
			guard:    guard
			body:     body
			position: ep.get_current_position()
		}
	}

	mut timeout := ast.Expr(ast.LiteralExpr{
		value: ast.IntegerLiteral{
			value: 0
		}
	})
	if ep.match(lexer.keyword(.after)) {
		timeout = ep.parse_expression()?
		ep.consume(lexer.keyword(.do_), 'Expected do after timeout')?
		// timeout_body := ep.parse_statement_block()?
		// For now, we'll use the timeout expression directly
		// In a full implementation, we'd need to handle the timeout body
	}

	ep.consume(lexer.keyword(.end_), 'Expected end after receive expression')?

	return ast.ReceiveExpr{
		cases:    cases
		timeout:  timeout
		position: ep.get_current_position()
	}
}

// parse_unsafe_expression parses unsafe expressions
fn (mut ep ExpressionParser) parse_unsafe_expression() ?ast.Expr {
	ep.advance() // consume 'unsafe'

	// Parse the expression that follows unsafe
	expr := ep.parse_expression()?

	// For now, we'll return the expression as-is
	// In a full implementation, we'd mark it as unsafe
	return expr
}

// parse_statement_block parses a block of statements
fn (mut ep ExpressionParser) parse_statement_block() ?[]ast.Stmt {
	mut statements := []ast.Stmt{}

	for !ep.check(lexer.keyword(.end_)) && !ep.is_at_end() {
		stmt := ep.parse_statement()?
		statements << stmt
	}

	return statements
}

// parse_statement parses a single statement
fn (mut ep ExpressionParser) parse_statement() ?ast.Stmt {
	expr := ep.parse_expression()?
	return ast.ExprStmt{
		expr: expr
	}
}
