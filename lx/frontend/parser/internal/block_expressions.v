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

		if p.is_pattern_matching_statement() {
			statements << ast.ExprStmt{
				expr: p.parse_pattern_matching_statement()?
			}
		} else if p.is_simple_assignment() {
			statements << ast.ExprStmt{
				expr: p.parse_simple_assignment()?
			}
		} else {
			expr := p.parse_expression()?
			statements << ast.ExprStmt{
				expr: expr
			}
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

pub fn (mut p LXParser) parse_block_expression_with_delimiter(delimiters []lexer.Token) ?ast.Expr {
	// Must be in expression context
	if p.context != .expression {
		p.add_error('Block expressions only allowed in expression context', 'Invalid context')
		return none
	}

	position := p.get_current_position()

	// Parse expression list
	mut statements := []ast.Stmt{}

	for !is_delimiter(p.current, delimiters) && !p.is_at_end() {
		p.skip_newlines()

		if is_delimiter(p.current, delimiters) {
			break
		}

		if p.is_pattern_matching_statement() {
			pattern_match := p.parse_pattern_matching_statement()?
			statements << ast.ExprStmt{
				expr: pattern_match
			}
		} else if p.is_simple_assignment() {
			assignment := p.parse_simple_assignment()?
			statements << ast.ExprStmt{
				expr: assignment
			}
		} else {
			expr := p.parse_expression()?
			statements << ast.ExprStmt{
				expr: expr
			}
		}
	}
	return ast.BlockExpr{
		body:     statements
		position: position
	}
}

fn is_delimiter(token lexer.Token, delimiters []lexer.Token) bool {
	return token.str() in delimiters.map(it.str())
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

// =====================
// OPERATOR PRECEDENCE TABLE
// =====================

// Precedência dos operadores
fn operator_precedence(op lexer.OperatorValue) int {
	return match op {
		.send { 1 }
		.type_cons { 2 }
		.concat { 3 }
		.plus, .minus { 4 }
		.mult, .div, .modulo, .bitwise_and, .bitwise_or, .bitwise_xor { 5 }
		.eq, .neq, .lt, .gt, .leq, .geq { 6 }
		.and_ { 7 }
		.or_ { 8 }
		.fat_arrow, .arrow, .pattern_match, .pipe { -1 }
		.lshift, .rshift { -1 } // Não são operadores binários, apenas delimitadores
		else { 11 }
	}
}

// Associatividade dos operadores
fn operator_associativity(op lexer.OperatorValue) string {
	return match op {
		.send { 'right' }
		else { 'left' }
	}
}

// =====================
// CENTRALIZED BINARY EXPRESSION PARSER
// =====================

fn (mut p LXParser) parse_expression() ?ast.Expr {
	return p.parse_binary_expression(0)
}

fn (mut p LXParser) parse_binary_expression(min_prec int) ?ast.Expr {
	mut left := p.parse_unary_expression()?
	for p.current is lexer.OperatorToken {
		op_token := p.current as lexer.OperatorToken
		prec := operator_precedence(op_token.value)
		assoc := operator_associativity(op_token.value)
		if prec < min_prec {
			break
		}
		p.advance() // consume operator
		right_prec := if assoc == 'left' { prec + 1 } else { prec }
		right := p.parse_binary_expression(right_prec)?
		left = ast.BinaryExpr{
			left:     left
			op:       operator_value_to_ast_binary_op(op_token.value)
			right:    right
			position: p.get_current_position()
			ast_id:   p.generate_ast_id()
		}
	}
	return left
}

fn operator_value_to_ast_binary_op(op lexer.OperatorValue) ast.BinaryOp {
	return match op {
		.concat { .append }
		.plus { .add }
		.minus { .subtract }
		.mult { .multiply }
		.div { .divide }
		.modulo { .modulo }
		.eq { .equal }
		.neq { .not_equal }
		.lt { .less_than }
		.gt { .greater_than }
		.leq { .less_equal }
		.geq { .greater_equal }
		.and_ { .and }
		.or_ { .or }
		.bitwise_and { .bitwise_and }
		.bitwise_or { .bitwise_or }
		.bitwise_xor { .bitwise_xor }
		.lshift { .lshift }
		.rshift { .rshift }
		else { .add }
	}
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
		// Check if the postfix operator is on the same line as the identifier
		if !p.is_postfix_operator_on_same_line() {
			break
		}

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
						// This is a method call: obj.field()
						expr = ast.CallExpr{
							function:  expr
							arguments: arguments
							position:  p.get_current_position()
						}
					}
				} else {
					// This is a method call: obj.field()
					expr = ast.CallExpr{
						function:  expr
						arguments: arguments
						position:  p.get_current_position()
					}
				}
			} else if expr is ast.VariableExpr {
				// This is a local function call: func_a()
				// Use atom literal instead of variable
				var_expr := expr as ast.VariableExpr
				func_name := var_expr.name
				position := var_expr.position
				expr = ast.CallExpr{
					function:  ast.LiteralExpr{
						value:    ast.AtomLiteral{
							value: func_name
						}
						position: position
					}
					arguments: arguments
					position:  p.get_current_position()
				}
			} else {
				// Regular function call (could be complex expression)
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

// is_postfix_operator_on_same_line checks if the current token is a postfix operator
// and is on the same line as the previous expression
fn (p &LXParser) is_postfix_operator_on_same_line() bool {
	// Check if current token is a postfix operator
	if !p.is_postfix_operator() {
		return false
	}

	// Get the position of the current token
	current_pos := p.current.get_position()

	// Check if there are any newlines between the previous expression and this operator
	// We need to look back to see if there's a newline between the last expression and current token
	if p.position > 0 {
		prev_token := p.tokens[p.position - 1]
		prev_pos := prev_token.get_position()

		// If the line numbers are different, they're not on the same line
		if current_pos.line != prev_pos.line {
			return false
		}
	}

	return true
}

// is_postfix_operator checks if the current token is a valid postfix operator
fn (p &LXParser) is_postfix_operator() bool {
	return p.check(punctuation_token(.lparen)) || // function call: expr(...)
	 p.check(operator_token(.dot)) || // field access: expr.field
	 p.check(punctuation_token(.lbracket)) // map access: expr[...]
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
		lexer.UpperIdentToken {
			if p.peek() is lexer.PunctuationToken {
				punc := p.peek() as lexer.PunctuationToken
				if punc.value == .lbrace {
					return p.parse_record_value_expression()
				}
			}
			return p.parse_identifier_expression()
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
				.nil_ {
					p.parse_nil_literal()
				}
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
				.for_ {
					p.parse_for_expression()
				}
				.receive {
					p.parse_receive_expression()
				}
				else {
					p.add_error('Unexpected keyword in expression', 'Got ${p.current.str()}')
					none
				}
			}
		}
		lexer.OperatorToken {
			op := p.current as lexer.OperatorToken
			match op.value {
				.lshift {
					mut result := ?ast.Expr(none)
					if p.peek() is lexer.StringToken || p.peek() is lexer.IntToken
						|| p.peek() is lexer.AtomToken || p.peek() is lexer.FloatToken
						|| p.peek() is lexer.IdentToken || p.peek() is lexer.KeyToken {
						result = p.parse_binary_value_expression()
					} else {
						p.add_error('Unexpected token after <<', 'Got ${p.peek().str()}')
						result = none
					}
					result
				}
				.modulo {
					p.parse_map_expression()
				}
				.concat {
					p.parse_binary_expression(0)
				}
				else {
					p.add_error('Unexpected operator in expression', 'Got ${p.current.str()}')
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

fn (mut p LXParser) parse_binary_pattern_expression() ?ast.Pattern {
	p.advance()
	mut segments := []ast.BinarySegment{}
	for !p.check(operator_token(.rshift)) && !p.is_at_end() {
		if p.check(punctuation_token(.comma)) {
			p.advance()
			continue
		}
		segments << p.parse_binary_segment()?
	}
	p.consume(operator_token(.rshift), 'Expected >> to close binary pattern')?
	return ast.BinaryPattern{
		segments: segments
	}
}

pub fn (mut p LXParser) parse_binary_segment() ?ast.BinarySegment {
	start_pos := p.get_current_position()
	mut value := ast.Expr{}
	if p.current is lexer.KeyToken {
		key_token := p.current as lexer.KeyToken
		key_value := key_token.value
		if key_value.ends_with(':') {
			var_name := key_value.trim_string_right(':')
			value = ast.VariableExpr{
				name:     var_name
				position: start_pos
				ast_id:   p.generate_ast_id()
			}
			p.advance()
		} else {
			value = ast.VariableExpr{
				name:     key_value
				position: start_pos
				ast_id:   p.generate_ast_id()
			}
			p.advance()
		}
	} else {
		value = match p.current {
			lexer.StringToken {
				p.parse_string_literal()?
			}
			lexer.IntToken {
				p.parse_integer_literal()?
			}
			lexer.FloatToken {
				p.parse_float_literal()?
			}
			lexer.IdentToken {
				// Handle variables like 'x'
				p.parse_identifier_expression()?
			}
			else {
				p.add_error('Invalid binary segment', 'Got ${p.current.str()}')
				return none
			}
		}
	}
	mut size := ?ast.Expr(none)
	mut options := []string{}

	if p.check(punctuation_token(.colon)) {
		p.advance()
		// Only parse simple literals for size, not full expressions
		if p.current is lexer.IntToken {
			size = p.parse_integer_literal()?
		}
	}

	if p.check(operator_token(.div)) {
		p.advance() // consume '/'

		// Parse qualifiers until we hit >>, comma, or end (same logic as binary expressions)
		for !p.check(operator_token(.rshift)) && !p.check(punctuation_token(.comma))
			&& !p.is_at_end() {
			mut qualifier := ''

			if p.current is lexer.KeyToken {
				// Handle KeyToken like 'unit:' and get the value after
				key_token := p.current as lexer.KeyToken
				key_name := key_token.value.trim_string_right(':')
				p.advance()

				if p.current is lexer.IntToken {
					int_token := p.current as lexer.IntToken
					int_value := int_token.value.str()
					qualifier = key_name + ':' + int_value
					p.advance()
				} else {
					qualifier = key_name
				}
			} else if p.current.is_identifier() {
				qualifier = p.current.get_value()
				p.advance()

				// Check for :value (like unit:8)
				if p.check(punctuation_token(.colon)) {
					p.advance() // consume ':'
					if p.current is lexer.IntToken {
						int_token := p.current as lexer.IntToken
						int_value := int_token.value.str()
						qualifier += ':' + int_value
						p.advance()
					}
				}
			} else {
				break
			}

			options << qualifier

			// Check for - separator for next qualifier
			if p.check(operator_token(.minus)) {
				p.advance() // consume '-'
			} else {
				break // no more qualifiers
			}
		}
	}
	return ast.BinarySegment{
		value:    value
		size:     size
		options:  options
		position: start_pos
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
	// 	mut name := ''
	// 	if p.current is lexer.KeyToken {
	// 		name = p.current.get_key_value()
	// 	} else {
	name := p.current.get_value()
	// }
	position := p.get_current_position()
	p.advance()

	return ast.VariableExpr{
		name:     name
		position: position
		ast_id:   p.generate_ast_id()
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
		ast_id:   p.generate_ast_id()
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
		ast_id:   p.generate_ast_id()
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
		ast_id:   p.generate_ast_id()
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
		ast_id:   p.generate_ast_id()
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
		ast_id:   p.generate_ast_id()
	}
}

// parse_nil_literal parses nil literals
fn (mut p LXParser) parse_nil_literal() ?ast.Expr {
	position := p.get_current_position()
	p.advance()

	return ast.LiteralExpr{
		value:    ast.NilLiteral{}
		position: position
		ast_id:   p.generate_ast_id()
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
		ast_id:   p.generate_ast_id()
	}
}

// parse_list_expression parses list expressions
fn (mut p LXParser) parse_list_expression() ?ast.Expr {
	position := p.get_current_position()
	p.advance() // consume '['

	// Handle empty list
	if p.check(punctuation_token(.rbracket)) {
		p.advance() // consume ']'
		return ast.ListLiteralExpr{
			elements: []
			position: position
			ast_id:   p.generate_ast_id()
		}
	}

	mut elements := []ast.Expr{}

	elements << p.parse_expression()?

	for p.match(punctuation_token(.comma)) {
		elements << p.parse_expression()?
	}
	if p.match(operator_token(.pipe)) {
		tail := p.parse_expression()?
		p.consume(punctuation_token(.rbracket), 'Expected ] after cons expression')?

		if elements.len > 1 {
			mut current_tail := tail
			for i := elements.len - 1; i > 0; i-- {
				current_tail = ast.ListConsExpr{
					head:     elements[i]
					tail:     current_tail
					position: position
				}
			}
			return ast.ListConsExpr{
				head:     elements[0]
				tail:     current_tail
				position: position
			}
		} else {
			return ast.ListConsExpr{
				head:     elements[0]
				tail:     tail
				position: position
			}
		}
	}
	p.consume(punctuation_token(.rbracket), 'Expected ] after list elements')?
	return ast.ListLiteralExpr{
		elements: elements
		position: position
		ast_id:   p.generate_ast_id()
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

	// Parse then body using block_expression_with_delimiter like with expressions
	then_body_expr := p.with_context(.expression, fn (mut parser LXParser) ?ast.Expr {
		return parser.parse_block_expression_with_delimiter([
			keyword_token(.else_),
			keyword_token(.end_),
		])
	})?

	// Convert to BlockExpr if needed
	then_body := if then_body_expr is ast.BlockExpr {
		then_body_expr as ast.BlockExpr
	} else {
		ast.BlockExpr{
			body:     [ast.ExprStmt{
				expr: then_body_expr
			}]
			position: position
		}
	}

	mut else_body := ast.BlockExpr{
		body:     []
		position: position
	}

	if p.match(keyword_token(.else_)) {
		else_body_expr := p.parse_block_expression_with_delimiter([
			keyword_token(.end_),
		])?

		// Convert to BlockExpr if needed
		else_body = if else_body_expr is ast.BlockExpr {
			else_body_expr as ast.BlockExpr
		} else {
			ast.BlockExpr{
				body:     [ast.ExprStmt{
					expr: else_body_expr
				}]
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

// ========================================
// CASE EXPRESSIONS
// Grammar: case_expression ::= 'case' expression 'do' case_cases 'end'
// Grammar: case_cases ::= case_case { case_case }
// Grammar: case_case ::= pattern ['when' expression] '->' expression_list
// ========================================

// parse_case_expression parses case expressions
fn (mut p LXParser) parse_case_expression() ?ast.Expr {
	position := p.get_current_position()
	p.advance() // consume 'case'

	value := p.parse_expression()?
	p.consume(keyword_token(.do_), 'Expected do after case value')?

	mut cases := []ast.MatchCase{}

	for !p.check(keyword_token(.end_)) && !p.is_at_end() {
		mut pattern := p.parse_pattern()?
		// Check for optional assignment to variable (pattern = variable)
		mut assign_variable := ''
		if p.match(operator_token(.assign)) {
			if p.current.is_identifier() {
				assign_variable = p.current.get_value()
				p.advance() // consume the identifier
			} else {
				p.add_error('Expected variable name after = in case pattern', 'Invalid syntax')
				return none
			}
		}

		mut guard := ast.Expr(ast.LiteralExpr{
			value: ast.BooleanLiteral{
				value: true
			}
		})

		if p.match(keyword_token(.when)) {
			guard = p.parse_expression()?
		}

		// If we have an assign_variable, update the pattern to include it
		if assign_variable.len > 0 {
			if pattern is ast.RecordPattern {
				record_pattern := pattern as ast.RecordPattern
				pattern = ast.RecordPattern{
					name:            record_pattern.name
					fields:          record_pattern.fields
					assign_variable: assign_variable
				}
			}
		}

		p.consume(operator_token(.arrow), 'Expected -> after pattern')?

		// Parse block of statements for the case body
		mut statements := []ast.Stmt{}

		for !p.check(keyword_token(.end_)) && !p.check(keyword_token(.after)) && !p.is_at_end() {
			p.skip_newlines()

			// Check for next case pattern
			if p.is_next_case_pattern() {
				break
			}

			if p.is_pattern_matching_statement() {
				pattern_match := p.parse_pattern_matching_statement()?
				statements << ast.ExprStmt{
					expr: pattern_match
				}
			} else if p.is_simple_assignment() {
				assignment := p.parse_simple_assignment()?
				statements << ast.ExprStmt{
					expr: assignment
				}
			} else {
				expr := p.parse_expression()?
				statements << ast.ExprStmt{
					expr: expr
				}
			}

			p.skip_newlines()
		}

		body := ast.BlockExpr{
			body:     statements
			position: p.get_current_position()
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
		mut pattern := p.parse_pattern()?

		// Check for optional assignment to variable (pattern = variable)
		mut assign_variable := ''
		if p.match(operator_token(.assign)) {
			if p.current.is_identifier() {
				assign_variable = p.current.get_value()
				p.advance() // consume the identifier
			} else {
				p.add_error('Expected variable name after = in with binding', 'Invalid syntax')
				return none
			}
		}

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

		// If we have an assign_variable, update the pattern to include it
		if assign_variable.len > 0 {
			// For record patterns, we need to set the assign_variable
			if pattern is ast.RecordPattern {
				record_pattern := pattern as ast.RecordPattern
				pattern = ast.RecordPattern{
					name:            record_pattern.name
					fields:          record_pattern.fields
					assign_variable: assign_variable
				}
			}
		}

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
		return parser.parse_block_expression_with_delimiter([
			keyword_token(.else_),
			keyword_token(.end_),
		])
	})?

	// Convert to BlockExpr if needed
	body := if body_expr is ast.BlockExpr {
		body_expr as ast.BlockExpr
	} else {
		ast.BlockExpr{
			body:     [ast.ExprStmt{
				expr: body_expr
			}]
			position: position
		}
	}

	mut else_body := ast.BlockExpr{
		body:     []
		position: position
	}

	if p.match(keyword_token(.else_)) {
		else_body_expr := p.parse_block_expression_with_delimiter([
			keyword_token(.end_),
		])?

		// Convert to BlockExpr if needed
		else_body = if else_body_expr is ast.BlockExpr {
			else_body_expr as ast.BlockExpr
		} else {
			ast.BlockExpr{
				body:     [ast.ExprStmt{
					expr: else_body_expr
				}]
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

	mut pattern := p.parse_pattern()?

	// Check for optional assignment to variable (pattern = variable)
	mut assign_variable := ''
	if p.match(operator_token(.assign)) {
		if p.current.is_identifier() {
			assign_variable = p.current.get_value()
			p.advance() // consume the identifier
		} else {
			p.add_error('Expected variable name after = in match pattern', 'Invalid syntax')
			return none
		}
	}

	mut guard := ast.Expr(ast.LiteralExpr{
		value: ast.BooleanLiteral{
			value: true
		}
	})

	if p.match(keyword_token(.when)) {
		guard = p.parse_expression()?
	}

	// If we have an assign_variable, update the pattern to include it
	if assign_variable.len > 0 {
		if pattern is ast.RecordPattern {
			record_pattern := pattern as ast.RecordPattern
			pattern = ast.RecordPattern{
				name:            record_pattern.name
				fields:          record_pattern.fields
				assign_variable: assign_variable
			}
		}
	}

	p.consume(operator_token(.pattern_match), 'Expected <- in match expression')?
	value := p.parse_expression()?
	mut rescue_expr := ?ast.Expr(none)

	// Check if this is a simple match or match rescue
	if p.check(keyword_token(.rescue)) {
		// Match rescue version
		p.consume(keyword_token(.rescue), 'Expected rescue after match expression')?

		// Parse rescue variable
		if p.current !is lexer.IdentToken {
			p.add_error('Expected variable name after rescue', 'Got ${p.current.str()}')
			return none
		}
		rescue_var_token := p.current as lexer.IdentToken
		rescue_var := rescue_var_token.value
		p.advance()

		p.consume(keyword_token(.do_), 'Expected do after rescue variable')?

		rescue_body_expr := p.with_context(.expression, fn (mut parser LXParser) ?ast.Expr {
			return parser.parse_block_expression_with_delimiter([
				keyword_token(.end_),
			])
		})?

		// Convert to BlockExpr if needed
		rescue_body := if rescue_body_expr is ast.BlockExpr {
			rescue_body_expr as ast.BlockExpr
		} else {
			ast.BlockExpr{
				body:     [ast.ExprStmt{
					expr: rescue_body_expr
				}]
				position: position
			}
		}

		p.consume(keyword_token(.end_), 'Expected end after rescue body')?

		// Criar rescue expression
		rescue_expr = ast.MatchRescueExpr{
			pattern:     pattern
			value:       value
			rescue_var:  rescue_var
			rescue_body: rescue_body
			position:    position
		}
	}

	// Criar o MatchExpr atual
	return ast.MatchExpr{
		value:    value
		cases:    [
			ast.MatchCase{
				pattern:  pattern
				guard:    guard
				position: position
			},
		]
		expr:     p.parse_expression()? // placeholder
		rescue:   rescue_expr
		position: position
		ast_id:   p.generate_ast_id()
	}
}

// is_next_case_pattern checks if the current token starts a new case pattern
fn (p &LXParser) is_next_case_pattern() bool {
	result := p.is_pattern_followed_by_arrow(p.position)
	return result
}

// is_pattern_followed_by_arrow checks if there's a pattern at the given position followed by ->
fn (p &LXParser) is_pattern_followed_by_arrow(pos int) bool {
	if pos >= p.tokens.len {
		return false
	}

	// Check for simple patterns (literals, identifiers, atoms)
	if p.tokens[pos] is lexer.IntToken || p.tokens[pos] is lexer.StringToken
		|| p.tokens[pos] is lexer.IdentToken || p.tokens[pos] is lexer.AtomToken {
		// Look ahead to see if this is a pattern (followed by ->)
		mut lookahead_pos := pos + 1
		if lookahead_pos < p.tokens.len {
			if p.tokens[lookahead_pos] is lexer.OperatorToken {
				op_token := p.tokens[lookahead_pos] as lexer.OperatorToken
				if op_token.value == .arrow {
					return true
				}
			}
		}
	}

	// Check for record patterns RecordName{field: pattern, ...}
	if p.tokens[pos] is lexer.UpperIdentToken {
		// Look ahead to see if this is followed by { (record pattern)
		mut lookahead_pos := pos + 1
		if lookahead_pos < p.tokens.len {
			if p.tokens[lookahead_pos] is lexer.PunctuationToken {
				punc_token := p.tokens[lookahead_pos] as lexer.PunctuationToken
				if punc_token.value == .lbrace {
					// Find the matching closing brace
					mut brace_count := 1
					mut scan_pos := lookahead_pos + 1
					for scan_pos < p.tokens.len && brace_count > 0 {
						if p.tokens[scan_pos] is lexer.PunctuationToken {
							punc := p.tokens[scan_pos] as lexer.PunctuationToken
							if punc.value == .lbrace {
								brace_count++
							} else if punc.value == .rbrace {
								brace_count--
							}
						}
						scan_pos++
					}

					// Check if there's an arrow after the closing brace
					if brace_count == 0 && scan_pos < p.tokens.len {
						// Check for direct arrow
						if p.tokens[scan_pos] is lexer.OperatorToken {
							op_token := p.tokens[scan_pos] as lexer.OperatorToken
							if op_token.value == .arrow {
								return true
							}
						}
						// Check for when followed by arrow
						if p.tokens[scan_pos] is lexer.KeywordToken {
							kw_token := p.tokens[scan_pos] as lexer.KeywordToken
							if kw_token.value == .when {
								// Look ahead for arrow after when
								mut when_scan_pos := scan_pos + 1
								// Skip the guard expression
								for when_scan_pos < p.tokens.len {
									if p.tokens[when_scan_pos] is lexer.OperatorToken {
										op_token := p.tokens[when_scan_pos] as lexer.OperatorToken
										if op_token.value == .arrow {
											return true
										}
									}
									when_scan_pos++
								}
							}
						}
					}
				}
			}
		}
	}

	// Check for tuple patterns {pattern, pattern, ...}
	if p.tokens[pos] is lexer.PunctuationToken {
		punc_token := p.tokens[pos] as lexer.PunctuationToken
		if punc_token.value == .lbrace {
			// Find the matching closing brace
			mut brace_count := 1
			mut scan_pos := pos + 1
			for scan_pos < p.tokens.len && brace_count > 0 {
				if p.tokens[scan_pos] is lexer.PunctuationToken {
					punc := p.tokens[scan_pos] as lexer.PunctuationToken
					if punc.value == .lbrace {
						brace_count++
					} else if punc.value == .rbrace {
						brace_count--
					}
				}
				scan_pos++
			}

			// Check if there's an arrow after the closing brace
			if brace_count == 0 && scan_pos < p.tokens.len {
				if p.tokens[scan_pos] is lexer.OperatorToken {
					op_token := p.tokens[scan_pos] as lexer.OperatorToken
					if op_token.value == .arrow {
						return true
					}
				}
			}
		}
	}

	// Check for list patterns [pattern, pattern, ...] or [head | tail]
	if p.tokens[pos] is lexer.PunctuationToken {
		punc_token := p.tokens[pos] as lexer.PunctuationToken
		if punc_token.value == .lbracket {
			// Find the matching closing bracket
			mut bracket_count := 1
			mut scan_pos := pos + 1
			for scan_pos < p.tokens.len && bracket_count > 0 {
				if p.tokens[scan_pos] is lexer.PunctuationToken {
					punc := p.tokens[scan_pos] as lexer.PunctuationToken
					if punc.value == .lbracket {
						bracket_count++
					} else if punc.value == .rbracket {
						bracket_count--
					}
				}
				scan_pos++
			}

			// Check if there's an arrow after the closing bracket
			if bracket_count == 0 && scan_pos < p.tokens.len {
				if p.tokens[scan_pos] is lexer.OperatorToken {
					op_token := p.tokens[scan_pos] as lexer.OperatorToken
					if op_token.value == .arrow {
						return true
					}
				}
			}
		}
	}

	return false
}

// ========================================
// FOR EXPRESSIONS (LIST COMPREHENSIONS)
// Grammar: for_expression ::= 'for' pattern 'in' expression ['when' expression] 'do' block_expression 'end'
// ========================================

// parse_for_expression parses for expressions (list comprehensions)
fn (mut p LXParser) parse_for_expression() ?ast.Expr {
	position := p.get_current_position()
	p.advance() // consume 'for'

	pattern := p.parse_pattern()?
	// Aceita tanto KeywordToken(.in) quanto IdentToken("in")
	if p.current is lexer.KeywordToken {
		kw := p.current as lexer.KeywordToken
		if kw.value == .in {
			p.advance()
		} else {
			p.add_error('Expected in after pattern', 'Expected in, got ${p.current.str()}')
			return none
		}
	} else if p.current is lexer.IdentToken {
		id := p.current as lexer.IdentToken
		if id.value == 'in' {
			p.advance()
		} else {
			p.add_error('Expected in after pattern', 'Expected in, got ${p.current.str()}')
			return none
		}
	} else {
		p.add_error('Expected in after pattern', 'Expected in, got ${p.current.str()}')
		return none
	}

	collection := p.parse_expression()?

	mut guard := ast.Expr(ast.LiteralExpr{
		value: ast.BooleanLiteral{
			value: true
		}
	})
	if p.match(keyword_token(.when)) {
		guard = p.parse_expression()?
	}

	p.consume(keyword_token(.do_), 'Expected do after for expression')?

	body_expr := p.with_context(.expression, fn (mut parser LXParser) ?ast.Expr {
		return parser.parse_block_expression_with_delimiter([
			keyword_token(.end_),
		])
	})?

	// Convert to BlockExpr if needed
	body := if body_expr is ast.BlockExpr {
		body_expr as ast.BlockExpr
	} else {
		ast.BlockExpr{
			body:     [ast.ExprStmt{
				expr: body_expr
			}]
			position: position
		}
	}

	p.consume(keyword_token(.end_), 'Expected end after for expression')?

	return ast.ForExpr{
		pattern:    pattern
		collection: collection
		guard:      guard
		body:       body
		position:   position
	}
}

// ========================================
// MAP EXPRESSIONS
// Grammar: map_expression ::= '%' '{' map_entries '}'
// Grammar: map_entries ::= map_entry { ',' map_entry }
// Grammar: map_entry ::= key_token expression | expression '=>' expression
// ========================================

// parse_map_expression parses map expressions
fn (mut p LXParser) parse_map_expression() ?ast.Expr {
	position := p.get_current_position()
	p.advance() // consume '%'

	// Check if this is a record update: %RecordName{...}
	if p.current is lexer.UpperIdentToken {
		return p.parse_record_update_expression()
	}

	p.consume(punctuation_token(.lbrace), 'Expected opening brace after %')?

	mut entries := []ast.MapEntry{}
	mut base_map := ast.Expr(ast.LiteralExpr{})

	if !p.check(punctuation_token(.rbrace)) {
		// Check for map update syntax first: %{base_map | key: value, ...}
		// Look ahead to see if this is a map update
		if p.is_map_update_syntax() {
			// Parse base map - use a specialized parser that stops at |
			base_map = p.parse_base_map_expression()?

			// The | should be the next token
			if !p.check(operator_token(.pipe)) {
				p.add_error('Expected | after base map in map update', 'Got ${p.current.str()}')
				return none
			}
			p.advance() // consume '|'

			// Parse entries after the pipe
			if !p.check(punctuation_token(.rbrace)) {
				for {
					// Check if we have a key token (identifier:)
					if p.current.is_key() {
						key_value := p.current.get_key_value()
						p.advance() // consume key token

						// Create an atom literal for the key
						key := ast.LiteralExpr{
							value:    ast.AtomLiteral{
								value: key_value
							}
							position: p.get_current_position()
						}

						value := p.parse_expression()?
						entries << ast.MapEntry{
							key:      key
							value:    value
							position: p.get_current_position()
						}
					} else {
						// Parse regular key expression
						key := p.parse_expression()?

						// Check for fat_arrow or arrow (general key)
						if p.match(operator_token(.fat_arrow)) {
							value := p.parse_expression()?
							entries << ast.MapEntry{
								key:      key
								value:    value
								position: p.get_current_position()
							}
						} else if p.match(operator_token(.arrow)) {
							value := p.parse_expression()?
							entries << ast.MapEntry{
								key:      key
								value:    value
								position: p.get_current_position()
							}
						} else {
							p.add_error('Expected => or -> in map entry', 'Got ${p.current.str()}')
							return none
						}
					}

					if !p.match(punctuation_token(.comma)) {
						break
					}
				}
			}
		} else {
			// Regular map literal: %{key: value, key2: value2}
			for {
				// Check if we have a key token (identifier:)
				if p.current.is_key() {
					key_value := p.current.get_key_value()
					p.advance() // consume key token

					// Create an atom literal for the key
					key := ast.LiteralExpr{
						value:    ast.AtomLiteral{
							value: key_value
						}
						position: p.get_current_position()
					}

					value := p.parse_expression()?
					entries << ast.MapEntry{
						key:      key
						value:    value
						position: p.get_current_position()
					}
				} else {
					// Parse regular key expression
					key := p.parse_expression()?

					// Check for fat_arrow or arrow (general key)
					if p.match(operator_token(.fat_arrow)) {
						value := p.parse_expression()?
						entries << ast.MapEntry{
							key:      key
							value:    value
							position: p.get_current_position()
						}
					} else if p.match(operator_token(.arrow)) {
						value := p.parse_expression()?
						entries << ast.MapEntry{
							key:      key
							value:    value
							position: p.get_current_position()
						}
					} else {
						p.add_error('Expected => or -> in map entry', 'Got ${p.current.str()}')
						return none
					}
				}

				if !p.match(punctuation_token(.comma)) {
					break
				}
			}
		}
	}

	p.consume(punctuation_token(.rbrace), 'Expected closing brace')?

	// Check if this is a map update (has base_map) or regular map literal
	if base_map != ast.Expr(ast.LiteralExpr{}) {
		return ast.MapUpdateExpr{
			base_map: base_map
			entries:  entries
			position: position
		}
	} else {
		return ast.MapLiteralExpr{
			entries:  entries
			position: position
		}
	}
}

// is_map_update_syntax checks if the current tokens indicate map update syntax
fn (p &LXParser) is_map_update_syntax() bool {
	// Look ahead to see if there's a | after the opening brace
	mut pos := p.position + 1
	for pos < p.tokens.len {
		if p.tokens[pos] is lexer.PunctuationToken {
			punc := p.tokens[pos] as lexer.PunctuationToken
			if punc.value == .rbrace {
				return false // End of map, no update syntax
			}
		} else if p.tokens[pos] is lexer.OperatorToken {
			op := p.tokens[pos] as lexer.OperatorToken
			if op.value == .pipe {
				return true // Found |, this is update syntax
			}
		}
		pos++
	}
	return false
}

// parse_base_map_expression parses the base map expression in map updates
fn (mut p LXParser) parse_base_map_expression() ?ast.Expr {
	// Parse until we hit the | operator
	mut pos := p.position
	for pos < p.tokens.len {
		if p.tokens[pos] is lexer.OperatorToken {
			op := p.tokens[pos] as lexer.OperatorToken
			if op.value == .pipe {
				break
			}
		}
		pos++
	}

	// For now, just parse a simple expression
	// In a full implementation, this would parse the entire base map
	return p.parse_expression()
}

// parse_record_update_expression parses record update expressions
fn (mut p LXParser) parse_record_update_expression() ?ast.Expr {
	// The current token is UpperIdentToken (record name)
	record_name := p.current.get_value()
	p.advance() // consume record name

	p.consume(punctuation_token(.lbrace), 'Expected opening brace after record name')?

	// Parse base record expression - should stop at pipe operator
	base_record := p.parse_expression()?

	// Expect pipe operator
	if !p.check(operator_token(.pipe)) {
		p.add_error('Expected | after base record in record update', 'Got ${p.current.str()}')
		return none
	}
	p.advance() // consume '|'

	// Parse fields after the pipe
	mut fields := []ast.RecordField{}
	if !p.check(punctuation_token(.rbrace)) {
		for {
			if p.current.is_key() {
				key_value := p.current.get_key_value()
				p.advance() // consume KeyToken
				value := p.parse_expression()?
				fields << ast.RecordField{
					name:     key_value
					value:    value
					position: p.get_current_position()
				}
			} else {
				field_name := p.current.get_value()
				if !p.current.is_identifier() {
					p.add_error('Expected field name', 'Got ${p.current.str()}')
					return none
				}
				p.advance()
				p.consume(punctuation_token(.colon), 'Expected colon after field name')?
				value := p.parse_expression()?
				fields << ast.RecordField{
					name:     field_name
					value:    value
					position: p.get_current_position()
				}
			}

			if !p.match(punctuation_token(.comma)) {
				break
			}
		}
	}

	p.consume(punctuation_token(.rbrace), 'Expected closing brace for record update')?

	return ast.RecordUpdateExpr{
		record_name: record_name
		base_record: base_record
		fields:      fields
		position:    p.get_current_position()
	}
}

// ========================================
// RECORD VALUE EXPRESSIONS
// Grammar: record_value_expression ::= UpperIdentToken '{' record_fields '}'
// Grammar: record_fields ::= record_field { ',' record_field }
// Grammar: record_field ::= key_token expression | identifier ':' expression
// ========================================

// parse_record_value_expression parses record value expressions
fn (mut p LXParser) parse_record_value_expression() ?ast.Expr {
	position := p.get_current_position()
	record_name := p.current.get_value()
	p.advance()
	p.consume(punctuation_token(.lbrace), 'Expected opening brace after record name')?

	// Check if this is a record update: RecordName{var | field: value, ...}
	// Look ahead to see if there's a pipe operator after the first expression
	if !p.check(punctuation_token(.rbrace)) {
		// Parse the first expression to see if it's followed by |
		mut pos := p.position
		mut found_pipe := false

		// Look ahead to find pipe operator
		for pos < p.tokens.len {
			if p.tokens[pos] is lexer.OperatorToken {
				op := p.tokens[pos] as lexer.OperatorToken
				if op.value == .pipe {
					found_pipe = true
					break
				}
			}
			if p.tokens[pos] is lexer.PunctuationToken {
				punc := p.tokens[pos] as lexer.PunctuationToken
				if punc.value == .rbrace {
					break
				}
			}
			pos++
		}

		if found_pipe {
			// This is a record update: RecordName{var | field: value, ...}
			// Parse the base record variable
			base_record := p.parse_expression()?

			// Expect pipe operator
			if !p.check(operator_token(.pipe)) {
				p.add_error('Expected | after base record in record update', 'Got ${p.current.str()}')
				return none
			}
			p.advance() // consume '|'

			// Parse fields after the pipe
			mut fields := []ast.RecordField{}
			if !p.check(punctuation_token(.rbrace)) {
				for {
					if p.current.is_key() {
						key_value := p.current.get_key_value()
						p.advance()
						value := p.parse_expression()?
						fields << ast.RecordField{
							name:     key_value
							value:    value
							position: p.get_current_position()
						}
					} else {
						field_name := p.current.get_value()
						if !p.current.is_identifier() {
							p.add_error('Expected field name', 'Got ${p.current.str()}')
							return none
						}
						p.advance()
						p.consume(punctuation_token(.colon), 'Expected colon after field name')?
						value := p.parse_expression()?
						fields << ast.RecordField{
							name:     field_name
							value:    value
							position: p.get_current_position()
						}
					}

					if !p.match(punctuation_token(.comma)) {
						break
					}
				}
			}

			p.consume(punctuation_token(.rbrace), 'Expected closing brace for record update')?

			return ast.RecordUpdateExpr{
				record_name: record_name
				base_record: base_record
				fields:      fields
				position:    position
			}
		}
	}

	// This is a regular record literal: RecordName{field: value, ...}
	mut fields := []ast.RecordField{}
	if !p.check(punctuation_token(.rbrace)) {
		for {
			if p.current.is_key() {
				key_value := p.current.get_key_value()
				p.advance()
				value := p.parse_expression()?
				fields << ast.RecordField{
					name:     key_value
					value:    value
					position: p.get_current_position()
				}
			} else {
				field_name := p.current.get_value()
				if !p.current.is_identifier() {
					p.add_error('Expected field name', 'Got ${p.current.str()}')
					return none
				}
				p.advance()
				p.consume(punctuation_token(.colon), 'Expected colon after field name')?
				value := p.parse_expression()?
				fields << ast.RecordField{
					name:     field_name
					value:    value
					position: p.get_current_position()
				}
			}

			if !p.match(punctuation_token(.comma)) {
				break
			}
		}
	}

	p.consume(punctuation_token(.rbrace), 'Expected closing brace for record value')?

	return ast.RecordLiteralExpr{
		name:     record_name
		fields:   fields
		position: position
	}
}

// ========================================
// RECEIVE EXPRESSIONS
// Grammar: receive_expression ::= 'receive' 'do' receive_cases ['after' expression '->' block_expression] 'end'
// Grammar: receive_cases ::= receive_case { receive_case }
// Grammar: receive_case ::= pattern ['when' expression] '->' block_expression
// ========================================

// parse_receive_expression parses receive expressions
fn (mut p LXParser) parse_receive_expression() ?ast.Expr {
	position := p.get_current_position()
	p.advance() // consume 'receive'
	p.consume(keyword_token(.do_), 'Expected do after receive')?

	mut cases := []ast.ReceiveCase{}
	for !p.check(keyword_token(.after)) && !p.check(keyword_token(.end_)) {
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

		// Parse block of statements for the case body
		mut statements := []ast.Stmt{}

		for !p.check(keyword_token(.end_)) && !p.check(keyword_token(.after)) && !p.is_at_end() {
			p.skip_newlines()

			// Check for next case pattern
			if p.is_next_receive_pattern() {
				break
			}

			if p.is_pattern_matching_statement() {
				pattern_match := p.parse_pattern_matching_statement()?
				statements << ast.ExprStmt{
					expr: pattern_match
				}
			} else if p.is_simple_assignment() {
				assignment := p.parse_simple_assignment()?
				statements << ast.ExprStmt{
					expr: assignment
				}
			} else {
				expr := p.parse_expression()?
				statements << ast.ExprStmt{
					expr: expr
				}
			}

			p.skip_newlines()
		}

		body := ast.BlockExpr{
			body:     statements
			position: p.get_current_position()
		}

		cases << ast.ReceiveCase{
			pattern:  pattern
			guard:    guard
			body:     body
			position: p.get_current_position()
		}
	}

	mut timeout := ?ast.TimeoutClause(none)
	if p.match(keyword_token(.after)) {
		timeout_expr := p.parse_expression()?
		p.consume(operator_token(.arrow), 'Expected -> after timeout expression')?

		// Parse block of statements for the timeout body
		mut timeout_statements := []ast.Stmt{}

		for !p.check(keyword_token(.end_)) && !p.is_at_end() {
			p.skip_newlines()

			if p.check(keyword_token(.end_)) {
				break
			}

			if p.is_pattern_matching_statement() {
				pattern_match := p.parse_pattern_matching_statement()?
				timeout_statements << ast.ExprStmt{
					expr: pattern_match
				}
			} else if p.is_simple_assignment() {
				assignment := p.parse_simple_assignment()?
				timeout_statements << ast.ExprStmt{
					expr: assignment
				}
			} else {
				expr := p.parse_expression()?
				timeout_statements << ast.ExprStmt{
					expr: expr
				}
			}

			p.skip_newlines()
		}

		timeout_body := ast.BlockExpr{
			body:     timeout_statements
			position: p.get_current_position()
		}

		timeout = ast.TimeoutClause{
			timeout:  timeout_expr
			body:     timeout_body
			position: p.get_current_position()
		}
	}

	p.consume(keyword_token(.end_), 'Expected end after receive expression')?

	return ast.ReceiveExpr{
		cases:    cases
		timeout:  timeout
		position: position
	}
}

// is_next_receive_pattern checks if the current token starts a new receive pattern
fn (p &LXParser) is_next_receive_pattern() bool {
	// Similar to case patterns, but for receive
	return p.is_pattern_followed_by_arrow(p.position)
}

// ========================================
// PATTERN MATCHING STATEMENT DETECTION
// ========================================

// is_pattern_matching_statement checks if the current tokens represent a pattern matching statement
// Pattern matching statements have the form: pattern = expression
fn (p &LXParser) is_pattern_matching_statement() bool {
	match p.current {
		lexer.BinaryPatternToken {
			return true
		}
		lexer.OperatorToken {
			op := p.current as lexer.OperatorToken
			if op.value == .lshift {
				mut pos := p.position + 1
				mut depth := 1
				for pos < p.tokens.len {
					match p.tokens[pos] {
						lexer.OperatorToken {
							op_token := p.tokens[pos] as lexer.OperatorToken
							if op_token.value == .lshift {
								depth++
							} else if op_token.value == .rshift {
								depth--
								if depth == 0 {
									// Now look for = after >>
									if pos + 1 < p.tokens.len {
										next_token := p.tokens[pos + 1]
										if next_token is lexer.OperatorToken {
											next_op := next_token as lexer.OperatorToken
											if next_op.value == .assign {
												return true
											}
										}
									}
									break
								}
							}
						}
						else {}
					}
					pos++
				}
				return false
			}
		}
		lexer.IdentToken {
			return false
		}
		lexer.UpperIdentToken {
			return false
		}
		lexer.PunctuationToken {
			punct := p.current as lexer.PunctuationToken
			match punct.value {
				.lbrace, .lbracket, .lparen {
					next_token := p.peek()
					return next_token is lexer.OperatorToken
						&& (next_token as lexer.OperatorToken).value == .assign
				}
				else {
					return false
				}
			}
		}
		else {
			return false
		}
	}
	return false
}

fn (mut p LXParser) parse_pattern_matching_statement() ?ast.Expr {
	position := p.get_current_position()
	pattern := p.parse_pattern()?
	p.consume(operator_token(.assign), 'Expected = in pattern matching statement')?
	value := p.parse_expression()?
	return ast.SimpleMatchExpr{
		pattern:  pattern
		value:    value
		guard:    ast.LiteralExpr{
			value: ast.BooleanLiteral{
				value: true
			}
		}
		position: position
	}
}

// is_simple_assignment checks if the current tokens represent a simple assignment statement
// Simple assignment statements have the form: variable = expression
fn (p &LXParser) is_simple_assignment() bool {
	if p.current is lexer.IdentToken {
		next_token := p.peek()
		return next_token is lexer.OperatorToken
			&& (next_token as lexer.OperatorToken).value == .assign
	}
	return false
}

// parse_simple_assignment parses a simple assignment statement: variable = expression
fn (mut p LXParser) parse_simple_assignment() ?ast.Expr {
	position := p.get_current_position()
	if !p.current.is_identifier() {
		return none
	}
	var_name := p.current.get_value()
	p.advance() // consume variable name
	p.consume(operator_token(.assign), 'Expected = in assignment')?
	value := p.parse_expression()?
	return ast.AssignExpr{
		name:     var_name
		value:    value
		position: position
	}
}

fn (mut p LXParser) parse_binary_value_expression() ?ast.Expr {
	start_pos := p.get_current_position()
	p.advance() // consume '<<'
	mut binary_segments := []ast.BinarySegment{}

	for !p.check(operator_token(.rshift)) && !p.is_at_end() {
		if p.check(punctuation_token(.comma)) {
			p.advance()
			continue
		}

		// Parse the value (can be literal, variable, etc.)
		mut value := ast.Expr{}
		match p.current {
			lexer.StringToken {
				value = p.parse_string_literal()?
			}
			lexer.IntToken {
				value = p.parse_integer_literal()?
			}
			lexer.FloatToken {
				value = p.parse_float_literal()?
			}
			lexer.AtomToken {
				value = p.parse_atom_literal()?
			}
			lexer.IdentToken {
				value = p.parse_identifier_expression()?
			}
			lexer.KeyToken {
				key_token := p.current as lexer.KeyToken
				var_name := key_token.value.trim_string_right(':')
				value = ast.VariableExpr{
					name:     var_name
					position: p.get_current_position()
					ast_id:   p.generate_ast_id()
				}
				p.advance()
			}
			else {
				p.add_error('Invalid value in binary expression', 'Got ${p.current.str()}')
				return none
			}
		}

		mut size := ?ast.Expr(none)
		if p.check(punctuation_token(.colon)) {
			p.advance() // consume ':'
			if p.current is lexer.IntToken {
				size = p.parse_integer_literal()?
			}
		} else if p.current is lexer.IntToken {
			size = p.parse_integer_literal()?
		}

		mut options := []string{}
		if p.check(operator_token(.div)) {
			p.advance() // consume '/'
			for !p.check(operator_token(.rshift)) && !p.check(punctuation_token(.comma))
				&& !p.is_at_end() {
				mut qualifier := ''

				if p.current is lexer.KeyToken {
					key_token := p.current as lexer.KeyToken
					key_name := key_token.value.trim_string_right(':')
					p.advance()

					if p.current is lexer.IntToken {
						int_token := p.current as lexer.IntToken
						int_value := int_token.value.str()
						qualifier = key_name + ':' + int_value
						p.advance()
					} else {
						qualifier = key_name
					}
				} else if p.current.is_identifier() {
					qualifier = p.current.get_value()
					p.advance()

					if p.check(punctuation_token(.colon)) {
						p.advance() // consume ':'
						if p.current is lexer.IntToken {
							qualifier += ':' + p.current.get_value()
							p.advance()
						}
					}
				} else {
					break
				}

				options << qualifier

				if p.check(operator_token(.minus)) {
					p.advance() // consume '-'
				} else {
					break // no more qualifiers
				}
			}
		}

		binary_segments << ast.BinarySegment{
			value:    value
			size:     size
			options:  options
			position: start_pos
		}
	}
	p.consume(operator_token(.rshift), 'Expected >> to close binary expression')?

	return ast.BinaryPatternExpr{
		segments: binary_segments
		position: start_pos
		ast_id:   p.generate_ast_id()
	}
}
