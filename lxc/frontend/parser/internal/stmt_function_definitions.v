module internal

import ast
import lexer

// collect_pending_directives collects directive tokens that precede the current position
fn (mut sp StatementParser) collect_pending_directives() []string {
	mut directives := []string{}

	// Look backwards for directive tokens
	mut pos := sp.position - 1
	for pos >= 0 {
		token := sp.tokens[pos]
		if token is lexer.DirectiveToken {
			directive_token := token as lexer.DirectiveToken
			// Adiciona a diretiva sem o @ extra
			directives.prepend(directive_token.directive)
		} else if token is lexer.NewlineToken {
			// Continue looking through newlines
		} else {
			// Stop at first non-directive, non-newline token
			break
		}
		pos--
	}

	return directives
}

// parse_function_statement parses function definitions
pub fn (mut sp StatementParser) parse_function_statement() ?ast.Stmt {
	// Collect pending directives
	directives := sp.collect_pending_directives()

	sp.advance() // consume 'def'

	// Parse function name
	if !sp.current.is_identifier() {
		sp.add_error('Expected function name', 'Got ${sp.current.str()}')
		return none
	}
	name := sp.current.get_value()
	sp.advance()

	// Check for multi-clause function: def func do ... end
	if sp.check(lexer.keyword(.do_)) {
		sp.advance()
		mut clauses := []ast.FunctionClause{}

		// Parse multiple function headers inside do...end block
		for !sp.check(lexer.keyword(.end_)) && !sp.is_at_end() {
			sp.skip_irrelevant_tokens()

			// Skip tokens until we find a potential clause start
			for sp.position < sp.tokens.len && !(sp.tokens[sp.position] is lexer.PunctuationToken
				&& (sp.tokens[sp.position] as lexer.PunctuationToken).value == .lparen)
				&& !sp.check(lexer.keyword(.end_)) {
				sp.position++
				sp.sync_current_token()
			}

			if sp.is_potential_new_clause_start() {
				clause := sp.parse_function_header()?
				clauses << clause
			} else {
				break
			}
		}

		if !sp.check(lexer.keyword(.end_)) {
			sp.add_error('Expected end after function headers', 'Got ${sp.current.str()}')
			return none
		}
		sp.consume(lexer.keyword(.end_), 'Expected end after function headers')?

		// Validate that we have at least one clause
		if clauses.len == 0 {
			sp.add_error('Multi-clause function must have at least one clause', '( for single-clause function or do for multi-clause function')
			return none
		}

		return ast.FunctionStmt{
			name:       name
			clauses:    clauses
			is_private: false
			directives: directives
			position:   sp.get_current_position()
		}
	}

	// Single-clause function: def func(...) do ... end
	if sp.check(lexer.punctuation(.lparen)) {
		clause := sp.parse_function_clause()?
		return ast.FunctionStmt{
			name:       name
			clauses:    [clause]
			is_private: false
			directives: directives
			position:   sp.get_current_position()
		}
	}

	// If we get here, neither do nor ( was found
	sp.add_error('Expected ( for single-clause function or do for multi-clause function',
		'Got ${sp.current.str()}')
	return none
}

// parse_private_function_statement parses private function definitions
fn (mut sp StatementParser) parse_private_function_statement() ?ast.Stmt {
	// Collect pending directives
	directives := sp.collect_pending_directives()

	sp.advance() // consume 'defp'

	if !sp.current.is_identifier() {
		sp.add_error('Expected function name', 'Got ${sp.current.str()}')
		return none
	}
	name := sp.current.get_value()
	sp.advance()

	mut clauses := []ast.FunctionClause{}

	// Check for multi-clause function: defp func do ... end
	if sp.check(lexer.keyword(.do_)) {
		sp.advance() // consume 'do'

		// Parse multiple function headers inside do...end block
		for !sp.check(lexer.keyword(.end_)) && !sp.is_at_end() {
			clause := sp.parse_function_header()?
			clauses << clause
		}

		if !sp.check(lexer.keyword(.end_)) {
			sp.add_error('Expected end after function headers', 'Got ${sp.current.str()}')
			return none
		}
		sp.consume(lexer.keyword(.end_), 'Expected end after function headers')?

		// Validate that we have at least one clause
		if clauses.len == 0 {
			sp.add_error('Multi-clause function must have at least one clause', '( for single-clause function or do for multi-clause function')
			return none
		}
	} else {
		// Single-clause function: defp func(...) do ... end
		if !sp.check(lexer.punctuation(.lparen)) {
			sp.add_error('Expected ( for single-clause function or do for multi-clause function',
				'Got ${sp.current.str()}')
			return none
		}

		clause := sp.parse_function_clause()?
		clauses << clause
	}

	return ast.FunctionStmt{
		name:       name
		clauses:    clauses
		is_private: true
		directives: directives
		position:   sp.get_current_position()
	}
}

// parse_function_clause parses a single function clause
fn (mut sp StatementParser) parse_function_clause() ?ast.FunctionClause {
	// Parse parameters
	mut parameters := []ast.Pattern{}

	if sp.check(lexer.punctuation(.lparen)) {
		sp.advance() // consume '('
		for !sp.check(lexer.punctuation(.rparen)) {
			param := sp.parse_pattern()?
			parameters << param

			if !sp.match(lexer.punctuation(.comma)) {
				break
			}
		}
		sp.consume(lexer.punctuation(.rparen), 'Expected closing parenthesis')?
	}

	// Parse guard (optional)
	mut guard := ast.Expr(ast.LiteralExpr{
		value: ast.BooleanLiteral{
			value: true
		}
	})
	if sp.match(lexer.keyword(.when)) {
		guard = sp.parse_expression()?
	}

	// Parse body
	sp.consume(lexer.keyword(.do_), 'Expected do after function clause')?
	body := sp.parse_clause_body()?
	sp.consume(lexer.keyword(.end_), 'Expected end after function body')?

	return ast.FunctionClause{
		parameters: parameters
		guard:      guard
		body:       ast.BlockExpr{
			body: body
			position: sp.get_current_position()
		}
		position:   sp.get_current_position()
	}
}

// parse_function_header parses function headers in new syntax: (params) when guard -> body
fn (mut sp StatementParser) parse_function_header() ?ast.FunctionClause {
	// Parse parameters
	mut parameters := []ast.Pattern{}

	sp.consume(lexer.punctuation(.lparen), 'Expected opening parenthesis for function header')?

	// Parse parameter list
	if !sp.check(lexer.punctuation(.rparen)) {
		for {
			param := sp.parse_pattern()?
			parameters << param

			if !sp.match(lexer.punctuation(.comma)) {
				break
			}
		}
	}
	sp.consume(lexer.punctuation(.rparen), 'Expected closing parenthesis')?

	// Parse guard (optional)
	mut guard := ast.Expr(ast.LiteralExpr{
		value: ast.BooleanLiteral{
			value: true
		}
	})
	if sp.match(lexer.keyword(.when)) {
		guard = sp.parse_expression()?
	}

	// Parse arrow
	sp.consume(lexer.operator(.arrow), 'Expected -> after function header')?

	// Skip newlines after arrow
	for sp.current is lexer.NewlineToken {
		sp.advance()
	}

	// Parse body - block of statements until next clause or end
	body := sp.parse_clause_body()?

	return ast.FunctionClause{
		parameters: parameters
		guard:      guard
		body:       ast.BlockExpr{
			body: body
			position: sp.get_current_position()
		}
		position:   sp.get_current_position()
	}
}

fn (mut sp StatementParser) skip_irrelevant_tokens() {
}
