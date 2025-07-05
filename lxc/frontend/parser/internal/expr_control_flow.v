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
		then_body: ast.BlockExpr{
			body:     then_body
			position: ep.get_current_position()
		}
		else_body: ast.BlockExpr{
			body:     else_body
			position: ep.get_current_position()
		}
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
			body:     ast.BlockExpr{
				body:     body
				position: ep.get_current_position()
			}
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
		body:      ast.BlockExpr{
			body:     body
			position: ep.get_current_position()
		}
		else_body: ast.BlockExpr{
			body:     else_body
			position: ep.get_current_position()
		}
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
		body:       ast.BlockExpr{
			body:     body
			position: ep.get_current_position()
		}
		position:   ep.get_current_position()
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
		// Check for else keyword to break early
		if ep.check(lexer.keyword(.else_)) {
			break
		}
		// Check for after keyword (in receive expressions)
		if ep.check(lexer.keyword(.after)) {
			break
		}



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

// parse_match_rescue_expression parses match rescue expressions
// Syntax: match pattern <- value rescue variable do rescue_body end
// Or simple match: match pattern <- value
fn (mut ep ExpressionParser) parse_match_rescue_expression() ?ast.Expr {
	ep.advance() // consume 'match'

	pattern := ep.parse_pattern()?
	ep.consume(lexer.operator(.pattern_match), 'Expected <- in match expression')?
	value := ep.parse_expression()?

	// Check if this is a simple match or match rescue
	if ep.check(lexer.keyword(.rescue)) {
		// Match rescue version
		ep.consume(lexer.keyword(.rescue), 'Expected rescue after match expression')?

		// Parse rescue variable
		if ep.current !is lexer.IdentToken {
			ep.add_error('Expected variable name after rescue', 'Got ${ep.current.str()}')
			return none
		}
		rescue_var_token := ep.current as lexer.IdentToken
		rescue_var := rescue_var_token.value
		ep.advance()

		ep.consume(lexer.keyword(.do_), 'Expected do after rescue variable')?

		rescue_body := ep.parse_statement_block()?

		ep.consume(lexer.keyword(.end_), 'Expected end after rescue body')?

		return ast.MatchRescueExpr{
			pattern:     pattern
			value:       value
			rescue_var:  rescue_var
			rescue_body: ast.BlockExpr{
				body:     rescue_body
				position: ep.get_current_position()
			}
			position:    ep.get_current_position()
		}
	} else {
		// Simple match version
		return ast.SimpleMatchExpr{
			pattern:  pattern
			value:    value
			position: ep.get_current_position()
		}
	}
}
