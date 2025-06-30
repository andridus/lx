module internal

import ast
import lexer

// parse_function_statement parses function definitions
pub fn (mut sp StatementParser) parse_function_statement() ?ast.Stmt {
	sp.advance() // consume 'def'

	// Parse function name
	if !sp.current.is_identifier() {
		sp.add_error('Expected function name', 'Got ${sp.current.str()}')
		return none
	}
	name := sp.current.get_value()
	sp.advance()

	if sp.check(lexer.keyword(.do_)) {
		sp.advance()
		mut clauses := []ast.FunctionClause{}
		for !sp.check(lexer.keyword(.end_)) && !sp.is_at_end() {
			sp.skip_irrelevant_tokens()
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
		return ast.FunctionStmt{
			name:       name
			clauses:    clauses
			is_private: false
			position:   sp.get_current_position()
		}
	}

	// SÓ SE NÃO FOR 'do', verifica se é '('
	if sp.check(lexer.punctuation(.lparen)) {
		clause := sp.parse_function_clause()?
		return ast.FunctionStmt{
			name:       name
			clauses:    [clause]
			is_private: false
			position:   sp.get_current_position()
		}
	}

	sp.add_error('Expected do or ( after function name (multi-head or single-head)', 'Got ${sp.current.str()}')
	return none
}

// parse_private_function_statement parses private function definitions
fn (mut sp StatementParser) parse_private_function_statement() ?ast.Stmt {
	sp.advance() // consume 'defp'

	name := sp.current.get_value()
	if !sp.current.is_identifier() {
		sp.add_error('Expected function name', 'Got ${sp.current.str()}')
		return none
	}
	sp.advance()

	mut clauses := []ast.FunctionClause{}

	// Check for new multi-header syntax: defp name do ... end
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
	} else {
		// Parse traditional single clause function
		clause := sp.parse_function_clause()?
		clauses << clause
	}

	// For now, we'll use the same structure as public functions
	// In a full implementation, we'd mark it as private
	return ast.FunctionStmt{
		name:       name
		clauses:    clauses
		is_private: true
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
		guard = sp.parse_simple_expression()?
	}

	// Parse body
	sp.consume(lexer.keyword(.do_), 'Expected do after function clause')?
	body := sp.parse_clause_body()?
	sp.consume(lexer.keyword(.end_), 'Expected end after function body')?

	return ast.FunctionClause{
		parameters: parameters
		guard:      guard
		body:       body
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
		guard = sp.parse_simple_expression()?
	}

	// Parse arrow
	sp.consume(lexer.operator(.arrow), 'Expected -> after function header')?

	// Parse body - block of statements until next clause or end
	body := sp.parse_clause_body()?

	return ast.FunctionClause{
		parameters: parameters
		guard:      guard
		body:       body
		position:   sp.get_current_position()
	}
}

fn (mut sp StatementParser) skip_irrelevant_tokens() {
}
