module internal

import ast
import lexer

// ========================================
// FUNCTION CLAUSE PARSING
// This module handles function clause parsing with proper context management
// ========================================

// parse_function_clauses parses function clauses for function definitions
pub fn (mut p LXParser) parse_function_clauses() ?[]ast.FunctionClause {
	// Check for multi-clause function: def func do ... end
	if p.check(keyword_token(.do_)) {
		return p.parse_multi_clause_function()
	}

	// Single-clause function: def func(...) do ... end
	if p.check(punctuation_token(.lparen)) {
		clause := p.parse_single_function_clause()?
		return [clause]
	}

	// If we get here, neither do nor ( was found
	p.add_error('Expected ( for single-clause function or do for multi-clause function',
		'Got ${p.current.str()}')
	return none
}

// ========================================
// SINGLE FUNCTION CLAUSE
// Grammar: function_clause ::= '(' param_list ')' [ 'when' guard ] 'do' block_expression 'end'
// ========================================

// parse_single_function_clause parses a single function clause
fn (mut p LXParser) parse_single_function_clause() ?ast.FunctionClause {
	position := p.get_current_position()

	// Parse parameters
	mut parameters := []ast.Pattern{}

	p.consume(punctuation_token(.lparen), 'Expected ( for function parameters')?

	if !p.check(punctuation_token(.rparen)) {
		for {
			param := p.parse_pattern()?
			parameters << param

			if !p.match(punctuation_token(.comma)) {
				break
			}
		}
	}

	p.consume(punctuation_token(.rparen), 'Expected ) after function parameters')?

	// Parse optional guard
	mut guard := ast.Expr(ast.LiteralExpr{
		value: ast.BooleanLiteral{
			value: true
		}
	})

	if p.match(keyword_token(.when)) {
		guard = p.with_context(.expression, fn (mut parser LXParser) ?ast.Expr {
			return parser.parse_expression()
		})?
	}

		// Parse function body as block expression (expression context)
	body_expr := p.with_context(.expression, fn (mut parser LXParser) ?ast.Expr {
		return parser.parse_block_expression()
	})?

	// Convert to BlockExpr if needed
	body := if body_expr is ast.BlockExpr {
		body_expr as ast.BlockExpr
	} else {
		ast.BlockExpr{
			body: [ast.ExprStmt{expr: body_expr}]
			position: position
		}
	}

	return ast.FunctionClause{
		parameters: parameters
		guard:      guard
		body:       body
		position:   position
	}
}

// ========================================
// MULTI-CLAUSE FUNCTION
// Grammar: multi_clause_function ::= 'do' { function_header } 'end'
// Grammar: function_header ::= '(' param_list ')' [ 'when' guard ] '->' expression_list
// ========================================

// parse_multi_clause_function parses multi-clause functions
fn (mut p LXParser) parse_multi_clause_function() ?[]ast.FunctionClause {
	p.consume(keyword_token(.do_), 'Expected do for multi-clause function')?
	p.skip_newlines()

	mut clauses := []ast.FunctionClause{}

	// Parse multiple function headers inside do...end block
	for !p.check(keyword_token(.end_)) && !p.is_at_end() {
		p.skip_newlines()

		if p.check(keyword_token(.end_)) {
			break
		}

		// Look for function clause start: (
		if !p.check(punctuation_token(.lparen)) {
			p.add_error('Expected ( to start function clause', 'Got ${p.current.str()}')
			break
		}

		clause := p.parse_function_header()?
		clauses << clause

		p.skip_newlines()
	}

	p.consume(keyword_token(.end_), 'Expected end after function clauses')?

	// Validate that we have at least one clause
	if clauses.len == 0 {
		p.add_error('Multi-clause function must have at least one clause', 'Expected function clauses')
		return none
	}

	return clauses
}

// ========================================
// FUNCTION HEADER
// Grammar: function_header ::= '(' param_list ')' [ 'when' guard ] '->' expression_list
// ========================================

// parse_function_header parses function headers in multi-clause syntax
fn (mut p LXParser) parse_function_header() ?ast.FunctionClause {
	position := p.get_current_position()

	// Parse parameters
	mut parameters := []ast.Pattern{}

	p.consume(punctuation_token(.lparen), 'Expected ( for function header')?

	if !p.check(punctuation_token(.rparen)) {
		for {
			param := p.parse_pattern()?
			parameters << param

			if !p.match(punctuation_token(.comma)) {
				break
			}
		}
	}

	p.consume(punctuation_token(.rparen), 'Expected ) after parameters')?

	// Parse optional guard
	mut guard := ast.Expr(ast.LiteralExpr{
		value: ast.BooleanLiteral{
			value: true
		}
	})

	if p.match(keyword_token(.when)) {
		guard = p.with_context(.expression, fn (mut parser LXParser) ?ast.Expr {
			return parser.parse_expression()
		})?
	}

	// Parse arrow
	p.consume(operator_token(.arrow), 'Expected -> after function header')?
	p.skip_newlines()

		// Parse clause body as expression list (expression context)
	body_expr := p.with_context(.expression, fn (mut parser LXParser) ?ast.Expr {
		return parser.parse_clause_body_expressions()
	})?

	// Convert to BlockExpr if needed
	body := if body_expr is ast.BlockExpr {
		body_expr as ast.BlockExpr
	} else {
		ast.BlockExpr{
			body: [ast.ExprStmt{expr: body_expr}]
			position: position
		}
	}

	return ast.FunctionClause{
		parameters: parameters
		guard:      guard
		body:       body
		position:   position
	}
}

// ========================================
// CLAUSE BODY EXPRESSIONS
// This parses the body of a function clause as a sequence of expressions
// ========================================

// parse_clause_body_expressions parses the body of a function clause
fn (mut p LXParser) parse_clause_body_expressions() ?ast.Expr {
	position := p.get_current_position()
	mut statements := []ast.Stmt{}

	// Parse expressions until we hit the next clause or end
	for !p.is_at_end() {
		p.skip_newlines()

		// Check for end of clause (next clause starts with ( or we hit end)
		if p.check(punctuation_token(.lparen)) {
			// Check if this is the start of a new clause
			if p.is_next_clause_start() {
				break
			}
		}

		if p.check(keyword_token(.end_)) {
			break
		}

		// Parse expression
		expr := p.parse_expression()?
		statements << ast.ExprStmt{
			expr: expr
		}

		p.skip_newlines()

		// If we're at the end or next clause, break
		if p.is_at_end() || p.is_next_clause_start() {
			break
		}
	}

	// If we only have one statement, return its expression directly
	// Otherwise, wrap in a block expression
	if statements.len == 1 {
		if statements[0] is ast.ExprStmt {
			expr_stmt := statements[0] as ast.ExprStmt
			return expr_stmt.expr
		}
	}

	return ast.BlockExpr{
		body:     statements
		position: position
	}
}

// ========================================
// HELPER FUNCTIONS
// ========================================

// is_next_clause_start checks if we're at the start of a new function clause
fn (p LXParser) is_next_clause_start() bool {
	if !p.check(punctuation_token(.lparen)) {
		return false
	}

	// Look ahead to see if this looks like a function clause
	// We need to find the matching ) followed by optional 'when' and then '->'
	mut pos := p.position + 1
	mut paren_count := 1

	// Find the matching closing parenthesis
	for pos < p.tokens.len && paren_count > 0 {
		token := p.tokens[pos]

		if token is lexer.PunctuationToken {
			punct := token as lexer.PunctuationToken
			if punct.value == .lparen {
				paren_count++
			} else if punct.value == .rparen {
				paren_count--
			}
		}

		pos++
	}

	// If we found the matching ), check what follows
	if paren_count == 0 && pos < p.tokens.len {
		// Skip optional 'when' clause
		if pos < p.tokens.len && p.tokens[pos] is lexer.KeywordToken {
			keyword := p.tokens[pos] as lexer.KeywordToken
			if keyword.value == .when {
				// Skip until we find the arrow
				pos++
				for pos < p.tokens.len {
					if p.tokens[pos] is lexer.OperatorToken {
						op := p.tokens[pos] as lexer.OperatorToken
						if op.value == .arrow {
							return true
						}
					}
					pos++
				}
				return false
			}
		}

		// Check for direct arrow
		if pos < p.tokens.len && p.tokens[pos] is lexer.OperatorToken {
			op := p.tokens[pos] as lexer.OperatorToken
			return op.value == .arrow
		}
	}

	return false
}

// ========================================
// RECORD FIELD PARSING
// Helper function for record definitions
// ========================================

// parse_record_field parses a single record field definition
pub fn (mut p LXParser) parse_record_field() ?ast.RecordFieldDef {
	// Parse field name
	field_name := p.current.get_value()
	if !p.current.is_identifier() {
		p.add_error('Expected field name', 'Got ${p.current.str()}')
		return ast.RecordFieldDef{}
	}

	position := p.get_current_position()
	p.advance()

	// Parse field type annotation
	p.consume(operator_token(.type_cons), 'Expected :: after field name')?

	field_type := p.parse_type_expression()?

	return ast.RecordFieldDef{
		name:       field_name
		field_type: field_type
		position:   position
	}
}