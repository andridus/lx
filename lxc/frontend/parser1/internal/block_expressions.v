module internal

import ast
import lexer

// ========================================
// BLOCK EXPRESSION PARSING
// Grammar: block_expression ::= 'do' expression_list 'end'
// Grammar: expression_list ::= expression { expression }
// ========================================

// parse_block_expression parses block expressions used in function bodies
// This is the main entry point for expression context parsing
pub fn (mut p LXParser) parse_block_expression() ?ast.Expr {
	// Must be in expression context
	if p.context != .expression {
		p.add_error('Block expressions only allowed in expression context', 'Invalid context')
		return none
	}

	position := p.get_current_position()

	// Consume 'do' keyword
	p.consume(keyword_token(.do_), 'Expected do to start block expression')?
	p.skip_newlines()

	// Parse expression list
	mut statements := []ast.Stmt{}

	for !p.check(keyword_token(.end_)) && !p.is_at_end() {
		p.skip_newlines()

		if p.check(keyword_token(.end_)) {
			break
		}

		// Parse expression and wrap in statement
		expr := p.parse_expression()?
		statements << ast.ExprStmt{
			expr: expr
		}

		p.skip_newlines()
	}

	// Consume 'end' keyword
	p.consume(keyword_token(.end_), 'Expected end to close block expression')?

	return ast.BlockExpr{
		body:     statements
		position: position
	}
}

// ========================================
// BLOCK TOP LEVEL PARSING
// Grammar: block_top_level ::= 'do' { module_statement } 'end'
// ========================================

// parse_block_top_level parses top-level blocks used in worker/supervisor/describe
// These blocks maintain mod context and only allow module statements
pub fn (mut p LXParser) parse_block_top_level() ?[]ast.Stmt {
	// Must be in mod context
	if p.context != .mod {
		p.add_error('Block top level only allowed in mod context', 'Invalid context')
		return none
	}

	// Consume 'do' keyword
	p.consume(keyword_token(.do_), 'Expected do to start block')?
	p.skip_newlines()

	mut statements := []ast.Stmt{}

	for !p.check(keyword_token(.end_)) && !p.is_at_end() {
		p.skip_newlines()

		if p.check(keyword_token(.end_)) {
			break
		}

		// Parse module statement (stays in mod context)
		stmt := p.parse_module_statement()?
		statements << stmt

		p.skip_newlines()
	}

	// Consume 'end' keyword
	p.consume(keyword_token(.end_), 'Expected end to close block')?

	return statements
}

// ========================================
// EXPRESSION PARSING
// This handles all expression types in expression context
// ========================================

// parse_expression parses expressions in expression context
pub fn (mut p LXParser) parse_expression() ?ast.Expr {
	// Must be in expression context
	if p.context != .expression {
		p.add_error('Expressions only allowed in expression context', 'Invalid context')
		return none
	}

	return p.parse_assignment_expression()
}

// ========================================
// ASSIGNMENT EXPRESSIONS
// Grammar: assignment_expression ::= identifier '=' expression | or_expression
// ========================================

// parse_assignment_expression parses assignment expressions
fn (mut p LXParser) parse_assignment_expression() ?ast.Expr {
	// Check for assignment pattern: identifier = expression
	if p.current.is_identifier() {
		next_token := p.peek()
		if next_token is lexer.OperatorToken {
			op := next_token as lexer.OperatorToken
			if op.value == .assign {
				// This is an assignment
				name := p.current.get_value()
				position := p.get_current_position()
				p.advance() // consume identifier
				p.advance() // consume '='

				value := p.parse_assignment_expression()?

				return ast.AssignExpr{
					name:     name
					value:    value
					position: position
				}
			}
		}
	}

	// Not an assignment, parse as or expression
	return p.parse_or_expression()
}

// ========================================
// LOGICAL EXPRESSIONS
// Grammar: or_expression ::= and_expression { 'or' and_expression }
// Grammar: and_expression ::= equality_expression { 'and' equality_expression }
// ========================================

// parse_or_expression parses logical OR expressions
fn (mut p LXParser) parse_or_expression() ?ast.Expr {
	mut left := p.parse_and_expression()?

	for p.check(operator_token(.or_)) {
		p.advance() // consume 'or'
		right := p.parse_and_expression()?
		left = ast.BinaryExpr{
			left:     left
			op:       .or
			right:    right
			position: p.get_current_position()
		}
	}

	return left
}

// parse_and_expression parses logical AND expressions
fn (mut p LXParser) parse_and_expression() ?ast.Expr {
	mut left := p.parse_equality_expression()?

	for p.check(operator_token(.and_)) {
		p.advance() // consume 'and'
		right := p.parse_equality_expression()?
		left = ast.BinaryExpr{
			left:     left
			op:       .and
			right:    right
			position: p.get_current_position()
		}
	}

	return left
}

// ========================================
// EQUALITY EXPRESSIONS
// Grammar: equality_expression ::= comparison_expression { ('==' | '!=') comparison_expression }
// ========================================

// parse_equality_expression parses equality expressions
fn (mut p LXParser) parse_equality_expression() ?ast.Expr {
	mut left := p.parse_comparison_expression()?

	for {
		if p.check(operator_token(.eq)) {
			p.advance() // consume '=='
			right := p.parse_comparison_expression()?
			left = ast.BinaryExpr{
				left:     left
				op:       .equal
				right:    right
				position: p.get_current_position()
			}
		} else if p.check(operator_token(.neq)) {
			p.advance() // consume '!='
			right := p.parse_comparison_expression()?
			left = ast.BinaryExpr{
				left:     left
				op:       .not_equal
				right:    right
				position: p.get_current_position()
			}
		} else {
			break
		}
	}

	return left
}

// ========================================
// COMPARISON EXPRESSIONS
// Grammar: comparison_expression ::= arithmetic_expression { ('<' | '>' | '<=' | '>=') arithmetic_expression }
// ========================================

// parse_comparison_expression parses comparison expressions
fn (mut p LXParser) parse_comparison_expression() ?ast.Expr {
	mut left := p.parse_arithmetic_expression()?

	for {
		mut op := ast.BinaryOp.add
		mut matched := false

		if p.check(operator_token(.lt)) {
			op = .less_than
			matched = true
		} else if p.check(operator_token(.gt)) {
			op = .greater_than
			matched = true
		} else if p.check(operator_token(.leq)) {
			op = .less_equal
			matched = true
		} else if p.check(operator_token(.geq)) {
			op = .greater_equal
			matched = true
		}

		if matched {
			p.advance() // consume operator
			right := p.parse_arithmetic_expression()?
			left = ast.BinaryExpr{
				left:     left
				op:       op
				right:    right
				position: p.get_current_position()
			}
		} else {
			break
		}
	}

	return left
}

// ========================================
// ARITHMETIC EXPRESSIONS
// Grammar: arithmetic_expression ::= term { ('+' | '-') term }
// Grammar: term ::= factor { ('*' | '/') factor }
// ========================================

// parse_arithmetic_expression parses addition and subtraction
fn (mut p LXParser) parse_arithmetic_expression() ?ast.Expr {
	mut left := p.parse_term()?

	for {
		mut op := ast.BinaryOp.add
		mut matched := false

		if p.check(operator_token(.plus)) {
			op = .add
			matched = true
		} else if p.check(operator_token(.minus)) {
			op = .subtract
			matched = true
		}

		if matched {
			p.advance() // consume operator
			right := p.parse_term()?
			left = ast.BinaryExpr{
				left:     left
				op:       op
				right:    right
				position: p.get_current_position()
			}
		} else {
			break
		}
	}

	return left
}

// parse_term parses multiplication and division
fn (mut p LXParser) parse_term() ?ast.Expr {
	mut left := p.parse_unary_expression()?

	for {
		mut op := ast.BinaryOp.add
		mut matched := false

		if p.check(operator_token(.mult)) {
			op = .multiply
			matched = true
		} else if p.check(operator_token(.div)) {
			op = .divide
			matched = true
		}

		if matched {
			p.advance() // consume operator
			right := p.parse_unary_expression()?
			left = ast.BinaryExpr{
				left:     left
				op:       op
				right:    right
				position: p.get_current_position()
			}
		} else {
			break
		}
	}

	return left
}

// ========================================
// UNARY EXPRESSIONS
// Grammar: unary_expression ::= ('-' | 'not') unary_expression | postfix_expression
// ========================================

// parse_unary_expression parses unary expressions
fn (mut p LXParser) parse_unary_expression() ?ast.Expr {
	if p.check(operator_token(.minus)) {
		p.advance() // consume '-'
		operand := p.parse_unary_expression()?
		return ast.UnaryExpr{
			op:       .minus
			operand:  operand
			position: p.get_current_position()
		}
	}

	if p.check(operator_token(.not_)) {
		p.advance() // consume 'not'
		operand := p.parse_unary_expression()?
		return ast.UnaryExpr{
			op:       .not
			operand:  operand
			position: p.get_current_position()
		}
	}

	return p.parse_postfix_expression()
}

// ========================================
// POSTFIX EXPRESSIONS
// Grammar: postfix_expression ::= primary_expression { postfix_operator }
// Grammar: postfix_operator ::= '(' argument_list ')' | '.' identifier | '[' expression ']'
// ========================================

// parse_postfix_expression parses postfix expressions (function calls, field access, etc.)
fn (mut p LXParser) parse_postfix_expression() ?ast.Expr {
	mut expr := p.parse_primary_expression()?

	// Handle postfix operations
	for {
		// Function call: expr(args)
		if p.check(punctuation_token(.lparen)) {
			p.advance() // consume '('
			mut arguments := []ast.Expr{}

			if !p.check(punctuation_token(.rparen)) {
				for {
					arguments << p.parse_expression()?
					if !p.match(punctuation_token(.comma)) {
						break
					}
				}
			}

			p.consume(punctuation_token(.rparen), 'Expected closing parenthesis')?

			// Check if this is an external function call (atom.function pattern)
			if expr is ast.RecordAccessExpr {
				record_access := expr as ast.RecordAccessExpr
				if record_access.record is ast.LiteralExpr {
					lit_expr := record_access.record as ast.LiteralExpr
					if lit_expr.value is ast.AtomLiteral {
						atom_lit := lit_expr.value as ast.AtomLiteral
						// This is an external call: :module.function()
						expr = ast.CallExpr{
							function:      ast.LiteralExpr{
								value: ast.NilLiteral{}
							} // placeholder
							external:      true
							module:        atom_lit.value
							function_name: record_access.field
							arguments:     arguments
							position:      p.get_current_position()
						}
					} else {
						// Regular function call
						expr = ast.CallExpr{
							function:  expr
							arguments: arguments
							position:  p.get_current_position()
						}
					}
				} else {
					// Regular function call
					expr = ast.CallExpr{
						function:  expr
						arguments: arguments
						position:  p.get_current_position()
					}
				}
			} else {
				// Regular function call
				expr = ast.CallExpr{
					function:  expr
					arguments: arguments
					position:  p.get_current_position()
				}
			}
		}
		// Field access: expr.field
		else if p.check(operator_token(.dot)) {
			p.advance() // consume '.'

			// Expect an identifier after the dot
			if !p.current.is_identifier() {
				p.add_error('Expected field name after .', 'Got ${p.current.str()}')
				return none
			}

			field_name := p.current.get_value()
			p.advance()

			expr = ast.RecordAccessExpr{
				record:   expr
				field:    field_name
				position: p.get_current_position()
			}
		}
		// Map access: expr[key]
		else if p.check(punctuation_token(.lbracket)) {
			p.advance() // consume '['
			key := p.parse_expression()?
			p.consume(punctuation_token(.rbracket), 'Expected ]')?

			expr = ast.MapAccessExpr{
				map_expr: expr
				key:      key
				position: p.get_current_position()
			}
		} else {
			break
		}
	}

	return expr
}

// ========================================
// PRIMARY EXPRESSIONS
// Grammar: primary_expression ::= literal | identifier | '(' expression ')' | ...
// ========================================

// parse_primary_expression parses primary expressions
fn (mut p LXParser) parse_primary_expression() ?ast.Expr {
	return match p.current {
		lexer.IdentToken {
			p.parse_identifier_expression()
		}
		lexer.IntToken {
			p.parse_integer_literal()
		}
		lexer.FloatToken {
			p.parse_float_literal()
		}
		lexer.StringToken {
			p.parse_string_literal()
		}
		lexer.BoolToken {
			p.parse_boolean_literal()
		}
		lexer.AtomToken {
			p.parse_atom_literal()
		}
		lexer.NilToken {
			p.parse_nil_literal()
		}
		lexer.PunctuationToken {
			punct := p.current as lexer.PunctuationToken
			match punct.value {
				.lparen {
					p.parse_parenthesized_expression()
				}
				.lbrace {
					p.parse_tuple_expression()
				}
				.lbracket {
					p.parse_list_expression()
				}
				.colon {
					p.parse_external_atom()
				}
				else {
					p.add_error('Unexpected punctuation in expression', 'Got ${p.current.str()}')
					none
				}
			}
		}
		lexer.KeywordToken {
			keyword := p.current as lexer.KeywordToken
			match keyword.value {
				.do_ {
					p.parse_block_expression()
				}
				.if_ {
					p.parse_if_expression()
				}
				.case_ {
					p.parse_case_expression()
				}
				.with {
					p.parse_with_expression()
				}
				.match_ {
					p.parse_match_expression()
				}
				else {
					p.add_error('Unexpected keyword in expression', 'Got ${p.current.str()}')
					none
				}
			}
		}
		else {
			p.add_error('Unexpected token in expression', 'Got ${p.current.str()}')
			none
		}
	}
}

// ========================================
// EXTERNAL ATOM PARSING
// Grammar: external_atom ::= ':' (atom | identifier)
// ========================================

// parse_external_atom parses external atoms like :io, :module_name
fn (mut p LXParser) parse_external_atom() ?ast.Expr {
	position := p.get_current_position()
	p.advance() // consume ':'

	return match p.current {
		lexer.AtomToken {
			value := p.current.get_value()
			p.advance()
			ast.LiteralExpr{
				value:    ast.AtomLiteral{
					value: value
				}
				position: position
			}
		}
		lexer.IdentToken {
			value := p.current.get_value()
			p.advance()
			ast.LiteralExpr{
				value:    ast.AtomLiteral{
					value: value
				}
				position: position
			}
		}
		else {
			p.add_error('Expected atom or identifier after :', 'Got ${p.current.str()}')
			none
		}
	}
}

// ========================================
// LITERAL EXPRESSIONS
// ========================================

// parse_identifier_expression parses identifier expressions
fn (mut p LXParser) parse_identifier_expression() ?ast.Expr {
	name := p.current.get_value()
	position := p.get_current_position()
	p.advance()

	return ast.VariableExpr{
		name:     name
		position: position
	}
}

// parse_integer_literal parses integer literals
fn (mut p LXParser) parse_integer_literal() ?ast.Expr {
	value := p.current.get_numeric_value() or { 0.0 }
	position := p.get_current_position()
	p.advance()

	return ast.LiteralExpr{
		value:    ast.IntegerLiteral{
			value: int(value)
		}
		position: position
	}
}

// parse_float_literal parses float literals
fn (mut p LXParser) parse_float_literal() ?ast.Expr {
	value := p.current.get_numeric_value() or { 0.0 }
	position := p.get_current_position()
	p.advance()

	return ast.LiteralExpr{
		value:    ast.FloatLiteral{
			value: value
		}
		position: position
	}
}

// parse_string_literal parses string literals
fn (mut p LXParser) parse_string_literal() ?ast.Expr {
	value := p.current.get_value()
	position := p.get_current_position()
	p.advance()

	return ast.LiteralExpr{
		value:    ast.StringLiteral{
			value: value
		}
		position: position
	}
}

// parse_boolean_literal parses boolean literals
fn (mut p LXParser) parse_boolean_literal() ?ast.Expr {
	value := p.current.get_boolean_value() or { false }
	position := p.get_current_position()
	p.advance()

	return ast.LiteralExpr{
		value:    ast.BooleanLiteral{
			value: value
		}
		position: position
	}
}

// parse_atom_literal parses atom literals
fn (mut p LXParser) parse_atom_literal() ?ast.Expr {
	value := p.current.get_value()
	position := p.get_current_position()
	p.advance()

	return ast.LiteralExpr{
		value:    ast.AtomLiteral{
			value: value
		}
		position: position
	}
}

// parse_nil_literal parses nil literals
fn (mut p LXParser) parse_nil_literal() ?ast.Expr {
	position := p.get_current_position()
	p.advance()

	return ast.LiteralExpr{
		value:    ast.NilLiteral{}
		position: position
	}
}

// ========================================
// COMPLEX EXPRESSIONS
// ========================================

// parse_parenthesized_expression parses parenthesized expressions
fn (mut p LXParser) parse_parenthesized_expression() ?ast.Expr {
	p.advance() // consume '('
	expr := p.parse_expression()?
	p.consume(punctuation_token(.rparen), 'Expected ) after expression')?
	return expr
}

// parse_tuple_expression parses tuple expressions
fn (mut p LXParser) parse_tuple_expression() ?ast.Expr {
	position := p.get_current_position()
	p.advance() // consume '{'

	mut elements := []ast.Expr{}

	if !p.check(punctuation_token(.rbrace)) {
		for {
			elements << p.parse_expression()?

			if !p.match(punctuation_token(.comma)) {
				break
			}
		}
	}

	p.consume(punctuation_token(.rbrace), 'Expected } after tuple elements')?

	return ast.TupleExpr{
		elements: elements
		position: position
	}
}

// parse_list_expression parses list expressions
fn (mut p LXParser) parse_list_expression() ?ast.Expr {
	position := p.get_current_position()
	p.advance() // consume '['

	mut elements := []ast.Expr{}

	if !p.check(punctuation_token(.rbracket)) {
		for {
			elements << p.parse_expression()?

			if !p.match(punctuation_token(.comma)) {
				break
			}
		}
	}

	p.consume(punctuation_token(.rbracket), 'Expected ] after list elements')?

	return ast.ListLiteralExpr{
		elements: elements
		position: position
	}
}

// ========================================
// CONTROL FLOW EXPRESSIONS
// These are placeholder implementations - full implementation would be more complex
// ========================================

// parse_if_expression parses if expressions
fn (mut p LXParser) parse_if_expression() ?ast.Expr {
	position := p.get_current_position()
	p.advance() // consume 'if'

	condition := p.parse_expression()?
	p.consume(keyword_token(.do_), 'Expected do after if condition')?

	// Parse then body in expression context
	then_body_expr := p.with_context(.expression, fn (mut parser LXParser) ?ast.Expr {
		return parser.parse_block_expression()
	})?

	// Convert to BlockExpr if needed
	then_body := if then_body_expr is ast.BlockExpr {
		then_body_expr as ast.BlockExpr
	} else {
		ast.BlockExpr{
			body: [ast.ExprStmt{expr: then_body_expr}]
			position: position
		}
	}

	mut else_body := ast.BlockExpr{
		body: []
		position: position
	}

	if p.match(keyword_token(.else_)) {
		else_body_expr := p.with_context(.expression, fn (mut parser LXParser) ?ast.Expr {
			return parser.parse_block_expression()
		})?

		// Convert to BlockExpr if needed
		else_body = if else_body_expr is ast.BlockExpr {
			else_body_expr as ast.BlockExpr
		} else {
			ast.BlockExpr{
				body: [ast.ExprStmt{expr: else_body_expr}]
				position: position
			}
		}
	}

	p.consume(keyword_token(.end_), 'Expected end after if expression')?

	return ast.IfExpr{
		condition: condition
		then_body: then_body
		else_body: else_body
		position:  position
	}
}

// parse_case_expression parses case expressions
fn (mut p LXParser) parse_case_expression() ?ast.Expr {
	position := p.get_current_position()
	p.advance() // consume 'case'

	value := p.parse_expression()?
	p.consume(keyword_token(.do_), 'Expected do after case value')?

	mut cases := []ast.MatchCase{}

	for !p.check(keyword_token(.end_)) && !p.is_at_end() {
		pattern := p.parse_pattern()?

		mut guard := ast.Expr(ast.LiteralExpr{
			value: ast.BooleanLiteral{
				value: true
			}
		})

		if p.match(keyword_token(.when)) {
			guard = p.parse_expression()?
		}

		p.consume(operator_token(.arrow), 'Expected -> after pattern')?

		body_expr := p.with_context(.expression, fn (mut parser LXParser) ?ast.Expr {
			return parser.parse_expression()
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

		cases << ast.MatchCase{
			pattern:  pattern
			guard:    guard
			body:     body
			position: p.get_current_position()
		}
	}

	p.consume(keyword_token(.end_), 'Expected end after case expression')?

	return ast.CaseExpr{
		value:    value
		cases:    cases
		position: position
	}
}

// parse_with_expression parses with expressions
fn (mut p LXParser) parse_with_expression() ?ast.Expr {
	position := p.get_current_position()
	p.advance() // consume 'with'

	mut bindings := []ast.WithBinding{}

	for !p.check(keyword_token(.do_)) {
		pattern := p.parse_pattern()?

		mut guard := ast.Expr(ast.LiteralExpr{
			value: ast.BooleanLiteral{
				value: true
			}
		})

		if p.match(keyword_token(.when)) {
			guard = p.parse_expression()?
		}

		p.consume(operator_token(.pattern_match), 'Expected <- in with binding')?
		value := p.parse_expression()?

		bindings << ast.WithBinding{
			pattern:  pattern
			value:    value
			guard:    guard
			position: p.get_current_position()
		}

		if !p.match(punctuation_token(.comma)) {
			break
		}
	}

	p.consume(keyword_token(.do_), 'Expected do after with bindings')?

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

	mut else_body := ast.BlockExpr{
		body: []
		position: position
	}

	if p.match(keyword_token(.else_)) {
		else_body_expr := p.with_context(.expression, fn (mut parser LXParser) ?ast.Expr {
			return parser.parse_block_expression()
		})?

		// Convert to BlockExpr if needed
		else_body = if else_body_expr is ast.BlockExpr {
			else_body_expr as ast.BlockExpr
		} else {
			ast.BlockExpr{
				body: [ast.ExprStmt{expr: else_body_expr}]
				position: position
			}
		}
	}

	p.consume(keyword_token(.end_), 'Expected end after with expression')?

	return ast.WithExpr{
		bindings:  bindings
		body:      body
		else_body: else_body
		position:  position
	}
}

// parse_match_expression parses match expressions
fn (mut p LXParser) parse_match_expression() ?ast.Expr {
	position := p.get_current_position()
	p.advance() // consume 'match'

	pattern := p.parse_pattern()?

	mut guard := ast.Expr(ast.LiteralExpr{
		value: ast.BooleanLiteral{
			value: true
		}
	})

	if p.match(keyword_token(.when)) {
		guard = p.parse_expression()?
	}

	p.consume(operator_token(.pattern_match), 'Expected <- in match expression')?
	value := p.parse_expression()?

	return ast.SimpleMatchExpr{
		pattern:  pattern
		value:    value
		guard:    guard
		position: position
	}
}
