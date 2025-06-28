module parser

import ast
import lexer
import errors

// ExpressionParser handles parsing of all expression types in LX
@[heap]
pub struct ExpressionParser {
	Parser
}

pub fn new_expression_parser(tokens []lexer.Token) &ExpressionParser {
	return &ExpressionParser{
		Parser: new_parser(tokens)
	}
}

// parse_expression parses the top-level expression
pub fn (mut ep ExpressionParser) parse_expression() ?ast.Expr {
	return ep.parse_or_expression()
}

// parse_or_expression parses expressions with 'or' precedence
fn (mut ep ExpressionParser) parse_or_expression() ?ast.Expr {
	mut left := ep.parse_and_expression()?

	for ep.match(lexer.OperatorToken.or_) {
		right := ep.parse_and_expression()?
		left = ast.BinaryExpr{
			left:     left
			op:       .or
			right:    right
			position: ep.get_current_position()
		}
	}

	return left
}

// parse_and_expression parses expressions with 'and' precedence
fn (mut ep ExpressionParser) parse_and_expression() ?ast.Expr {
	mut left := ep.parse_orelse_expression()?

	for ep.match(lexer.OperatorToken.and_) {
		right := ep.parse_orelse_expression()?
		left = ast.BinaryExpr{
			left:     left
			op:       .and
			right:    right
			position: ep.get_current_position()
		}
	}

	return left
}

// parse_orelse_expression parses expressions with 'orelse' precedence
fn (mut ep ExpressionParser) parse_orelse_expression() ?ast.Expr {
	mut left := ep.parse_andalso_expression()?

	for ep.match(lexer.OperatorToken.orelse) {
		right := ep.parse_andalso_expression()?
		left = ast.BinaryExpr{
			left:     left
			op:       .or
			right:    right
			position: ep.get_current_position()
		}
	}

	return left
}

// parse_andalso_expression parses expressions with 'andalso' precedence
fn (mut ep ExpressionParser) parse_andalso_expression() ?ast.Expr {
	mut left := ep.parse_comparison_expression()?

	for ep.match(lexer.OperatorToken.andalso) {
		right := ep.parse_comparison_expression()?
		left = ast.BinaryExpr{
			left:     left
			op:       .and
			right:    right
			position: ep.get_current_position()
		}
	}

	return left
}

// parse_comparison_expression parses comparison expressions
fn (mut ep ExpressionParser) parse_comparison_expression() ?ast.Expr {
	mut left := ep.parse_concatenation_expression()?
	if ep.current !is lexer.OperatorToken {
		return left
	}
	op_token := ep.current as lexer.OperatorToken
	for {
		mut op := ast.BinaryOp.equal
		mut should_continue := false
		match op_token {
			.eq {
				op = .equal
				should_continue = true
			}
			.neq {
				op = .not_equal
				should_continue = true
			}
			.lt {
				op = .less_than
				should_continue = true
			}
			.gt {
				op = .greater_than
				should_continue = true
			}
			.leq {
				op = .less_equal
				should_continue = true
			}
			.geq {
				op = .greater_equal
				should_continue = true
			}
			else { break }
		}

		if !should_continue {
			break
		}

		ep.advance()
		right := ep.parse_concatenation_expression()?
		left = ast.BinaryExpr{
			left:     left
			op:       op
			right:    right
			position: ep.get_current_position()
		}
	}

	return left
}

// parse_concatenation_expression parses string concatenation expressions
fn (mut ep ExpressionParser) parse_concatenation_expression() ?ast.Expr {
	mut left := ep.parse_additive_expression()?

	for ep.match(lexer.OperatorToken.concat) {
		right := ep.parse_additive_expression()?
		left = ast.BinaryExpr{
			left:     left
			op:       .append
			right:    right
			position: ep.get_current_position()
		}
	}

	return left
}

// parse_additive_expression parses addition and subtraction expressions
fn (mut ep ExpressionParser) parse_additive_expression() ?ast.Expr {
	mut left := ep.parse_multiplicative_expression()?

	if ep.current !is lexer.OperatorToken {
		return left
	}
	op_token := ep.current as lexer.OperatorToken
	for {
		mut op := ast.BinaryOp.add
		mut should_continue := false

		match op_token {
			.plus {
				op = .add
				should_continue = true
			}
			.minus {
				op = .subtract
				should_continue = true
			}
			else { break }
		}

		if !should_continue {
			break
		}

		ep.advance()
		right := ep.parse_multiplicative_expression()?
		left = ast.BinaryExpr{
			left:     left
			op:       op
			right:    right
			position: ep.get_current_position()
		}
	}

	return left
}

// parse_multiplicative_expression parses multiplication and division expressions
fn (mut ep ExpressionParser) parse_multiplicative_expression() ?ast.Expr {
	mut left := ep.parse_unary_expression()?

	if ep.current !is lexer.OperatorToken {
		return left
	}
	op_token := ep.current as lexer.OperatorToken

	for {
		mut op := ast.BinaryOp.multiply
		mut should_continue := false

		match op_token {
			.mult {
				op = .multiply
				should_continue = true
			}
			.div {
				op = .divide
				should_continue = true
			}
			else { break }
		}

		if !should_continue {
			break
		}

		ep.advance()
		right := ep.parse_unary_expression()?
		left = ast.BinaryExpr{
			left:     left
			op:       op
			right:    right
			position: ep.get_current_position()
		}
	}

	return left
}

// parse_unary_expression parses unary expressions
fn (mut ep ExpressionParser) parse_unary_expression() ?ast.Expr {
	if ep.current is lexer.OperatorToken {
		op_token := ep.current as lexer.OperatorToken
		if op_token == .not_ {
			operand := ep.parse_unary_expression()?
			return ast.UnaryExpr{
				op:       .not
				operand:  operand
				position: ep.get_current_position()
			}
		}
	}

	return ep.parse_send_expression()
}

// parse_send_expression parses message sending expressions
fn (mut ep ExpressionParser) parse_send_expression() ?ast.Expr {
	mut left := ep.parse_cons_expression()?

	for ep.match(lexer.OperatorToken.send) {
		right := ep.parse_cons_expression()?
		left = ast.SendExpr{
			pid:      left
			message:  right
			position: ep.get_current_position()
		}
	}

	return left
}

// parse_cons_expression parses list cons expressions
fn (mut ep ExpressionParser) parse_cons_expression() ?ast.Expr {
	mut left := ep.parse_primary_expression()?

	for ep.match(lexer.OperatorToken.type_cons) {
		right := ep.parse_primary_expression()?
		left = ast.ListConsExpr{
			head:     left
			tail:     right
			position: ep.get_current_position()
		}
	}

	return left
}

// parse_primary_expression parses primary expressions
fn (mut ep ExpressionParser) parse_primary_expression() ?ast.Expr {
	mut expr := ep.parse_atom_expression()?

	// Handle postfix operations
	for {
		if ep.current is lexer.PunctuationToken {
			punc_token := ep.current as lexer.PunctuationToken
			match punc_token {
				.lparen {
					expr = ep.parse_call_expression(expr)?
				}
				.lbracket {
					expr = ep.parse_map_access_expression(expr)?
				}
				else { break }
			}
		} else if ep.current is lexer.OperatorToken {
			op_token := ep.current as lexer.OperatorToken
			match op_token {
				.dot {
					expr = ep.parse_record_access_expression(expr)?
				}
				else { break }
			}
		} else {
			break
		}
	}

	return expr
}

// parse_atom_expression parses atomic expressions
fn (mut ep ExpressionParser) parse_atom_expression() ?ast.Expr {
	return match ep.current {
		lexer.IdentToken { ep.parse_identifier_expression() }
		lexer.UpperIdentToken { ep.parse_identifier_expression() }
		lexer.StringToken { ep.parse_string_literal() }
		lexer.IntToken { ep.parse_integer_literal() }
		lexer.FloatToken { ep.parse_float_literal() }
		lexer.BoolToken { ep.parse_boolean_literal() }
		lexer.AtomToken { ep.parse_atom_literal() }
		lexer.NilToken { ep.parse_nil_literal() }
		lexer.PunctuationToken {
			punc_token := ep.current as lexer.PunctuationToken
			match punc_token {
				.lparen { ep.parse_parenthesized_expression() }
				.lbrace { ep.parse_tuple_expression() }
				.lbracket { ep.parse_list_expression() }
				else {
					ep.add_error('Unexpected token: ${ep.current.str()}', 'Expected expression')
					none
				}
			}
		}
		lexer.OperatorToken {
			op_token := ep.current as lexer.OperatorToken
			match op_token {
				.record_update { ep.parse_map_expression() }
				else {
					ep.add_error('Unexpected token: ${ep.current.str()}', 'Expected expression')
					none
				}
			}
		}
		lexer.KeywordToken {
			keyword_token := ep.current as lexer.KeywordToken
			match keyword_token {
				.if_ { ep.parse_if_expression() }
				.case_ { ep.parse_case_expression() }
				.with { ep.parse_with_expression() }
				.for_ { ep.parse_for_expression() }
				.receive { ep.parse_receive_expression() }
				.record { ep.parse_record_expression() }
				.unsafe { ep.parse_unsafe_expression() }
				else {
					ep.add_error('Unexpected token: ${ep.current.str()}', 'Expected expression')
					none
				}
			}
		}
		else {
			ep.add_error('Unexpected token: ${ep.current.str()}', 'Expected expression')
			none
		}
	}
}

// parse_identifier_expression parses identifier expressions
fn (mut ep ExpressionParser) parse_identifier_expression() ?ast.Expr {
	token := ep.current
	ep.advance()

	return ast.VariableExpr{
		name: token.get_value()
	}
}

// parse_string_literal parses string literals
fn (mut ep ExpressionParser) parse_string_literal() ?ast.Expr {
	token := ep.current
	ep.advance()

	return ast.LiteralExpr{
		value: ast.StringLiteral{
			value: token.get_value()
		}
	}
}

// parse_integer_literal parses integer literals
fn (mut ep ExpressionParser) parse_integer_literal() ?ast.Expr {
	token := ep.current
	ep.advance()

	value := token.get_numeric_value() or { 0.0 }
	return ast.LiteralExpr{
		value: ast.IntegerLiteral{
			value: int(value)
		}
	}
}

// parse_float_literal parses float literals
fn (mut ep ExpressionParser) parse_float_literal() ?ast.Expr {
	token := ep.current
	ep.advance()

	value := token.get_numeric_value() or { 0.0 }
	return ast.LiteralExpr{
		value: ast.FloatLiteral{
			value: value
		}
	}
}

// parse_boolean_literal parses boolean literals
fn (mut ep ExpressionParser) parse_boolean_literal() ?ast.Expr {
	token := ep.current
	ep.advance()

	value := token.get_boolean_value() or { false }
	return ast.LiteralExpr{
		value: ast.BooleanLiteral{
			value: value
		}
	}
}

// parse_atom_literal parses atom literals
fn (mut ep ExpressionParser) parse_atom_literal() ?ast.Expr {
	token := ep.current
	ep.advance()

	return ast.LiteralExpr{
		value: ast.AtomLiteral{
			value: token.get_value()
		}
	}
}

// parse_nil_literal parses nil literals
fn (mut ep ExpressionParser) parse_nil_literal() ?ast.Expr {
	ep.advance()

	return ast.LiteralExpr{
		value: ast.NilLiteral{}
	}
}

// parse_parenthesized_expression parses parenthesized expressions
fn (mut ep ExpressionParser) parse_parenthesized_expression() ?ast.Expr {
	ep.advance() // consume '('
	expr := ep.parse_expression()?
	ep.consume(lexer.PunctuationToken.rparen, 'Expected closing parenthesis')?

	return expr
}

// parse_call_expression parses function call expressions
fn (mut ep ExpressionParser) parse_call_expression(function ast.Expr) ?ast.Expr {
	ep.advance() // consume '('

	mut arguments := []ast.Expr{}
	if !ep.check(lexer.PunctuationToken.rparen) {
		for {
			arguments << ep.parse_expression()?

			if !ep.match(lexer.PunctuationToken.comma) {
				break
			}
		}
	}

	ep.consume(lexer.PunctuationToken.rparen, 'Expected closing parenthesis')?

	return ast.CallExpr{
		function:  function
		arguments: arguments
		position:  ep.get_current_position()
	}
}

// parse_record_access_expression parses record field access expressions
fn (mut ep ExpressionParser) parse_record_access_expression(record ast.Expr) ?ast.Expr {
	ep.advance() // consume '.'

	field_token := ep.current
	if !ep.current.is_identifier() {
		ep.add_error('Expected identifier after dot', 'Got ${ep.current.str()}')
		return none
	}
	ep.advance()

	return ast.RecordAccessExpr{
		record:   record
		field:    field_token.get_value()
		position: ep.get_current_position()
	}
}

// parse_map_access_expression parses map access expressions
fn (mut ep ExpressionParser) parse_map_access_expression(map_expr ast.Expr) ?ast.Expr {
	ep.advance() // consume '['
	key := ep.parse_expression()?
	ep.consume(lexer.PunctuationToken.rbracket, 'Expected closing bracket')?

	return ast.MapAccessExpr{
		map_expr: map_expr
		key:      key
		position: ep.get_current_position()
	}
}

// parse_tuple_expression parses tuple expressions
fn (mut ep ExpressionParser) parse_tuple_expression() ?ast.Expr {
	ep.advance() // consume '{'

	mut elements := []ast.Expr{}
	if !ep.check(lexer.PunctuationToken.rbrace) {
		for {
			elements << ep.parse_expression()?

			if !ep.match(lexer.PunctuationToken.comma) {
				break
			}
		}
	}

	ep.consume(lexer.PunctuationToken.rbrace, 'Expected closing brace')?

	return ast.TupleExpr{
		elements: elements
		position: ep.get_current_position()
	}
}

// parse_list_expression parses list expressions
fn (mut ep ExpressionParser) parse_list_expression() ?ast.Expr {
	ep.advance() // consume '['

	mut elements := []ast.Expr{}
	if !ep.check(lexer.PunctuationToken.rbracket) {
		for {
			elements << ep.parse_expression()?

			if !ep.match(lexer.PunctuationToken.comma) {
				break
			}
		}
	}

	ep.consume(lexer.PunctuationToken.rbracket, 'Expected closing bracket')?

	return ast.ListLiteralExpr{
		elements: elements
		position: ep.get_current_position()
	}
}

// parse_map_expression parses map expressions
fn (mut ep ExpressionParser) parse_map_expression() ?ast.Expr {
	ep.advance() // consume '%'
	ep.consume(lexer.PunctuationToken.lbrace, 'Expected opening brace after %')?

	mut entries := []ast.MapEntry{}
	if !ep.check(lexer.PunctuationToken.rbrace) {
		for {
			key := ep.parse_expression()?

			// Check for colon (atom key) or arrow (general key)
			if ep.match(lexer.PunctuationToken.colon) {
				value := ep.parse_expression()?
				entries << ast.MapEntry{
					key:      key
					value:    value
					position: ep.get_current_position()
				}
			} else if ep.match(lexer.OperatorToken.arrow) {
				value := ep.parse_expression()?
				entries << ast.MapEntry{
					key:      key
					value:    value
					position: ep.get_current_position()
				}
			} else {
				ep.add_error('Expected : or => in map entry', 'Got ${ep.current.str()}')
				return none
			}

			if !ep.match(lexer.PunctuationToken.comma) {
				break
			}
		}
	}

	ep.consume(lexer.PunctuationToken.rbrace, 'Expected closing brace')?

	return ast.MapLiteralExpr{
		entries:  entries
		position: ep.get_current_position()
	}
}

// parse_record_expression parses record expressions
fn (mut ep ExpressionParser) parse_record_expression() ?ast.Expr {
	ep.advance() // consume 'record'

	// Parse record name
	name_token := ep.current
	if !ep.current.is_identifier() {
		ep.add_error('Expected record name', 'Got ${ep.current.str()}')
		return none
	}
	ep.advance()

	ep.consume(lexer.PunctuationToken.lbrace, 'Expected opening brace')?

	mut fields := []ast.RecordField{}
	if !ep.check(lexer.PunctuationToken.rbrace) {
		for {
			field_name := ep.current.get_value()
			if !ep.current.is_identifier() {
				ep.add_error('Expected field name', 'Got ${ep.current.str()}')
				return none
			}
			ep.advance()

			ep.consume(lexer.PunctuationToken.colon, 'Expected colon after field name')?

			value := ep.parse_expression()?
			fields << ast.RecordField{
				name:     field_name
				value:    value
				position: ep.get_current_position()
			}

			if !ep.match(lexer.PunctuationToken.comma) {
				break
			}
		}
	}

	ep.consume(lexer.PunctuationToken.rbrace, 'Expected closing brace')?

	return ast.RecordLiteralExpr{
		name:     name_token.get_value()
		fields:   fields
		position: ep.get_current_position()
	}
}

// parse_if_expression parses if expressions
fn (mut ep ExpressionParser) parse_if_expression() ?ast.Expr {
	ep.advance() // consume 'if'

	condition := ep.parse_expression()?
	ep.consume(lexer.KeywordToken.do_, 'Expected do after if condition')?

	then_body := ep.parse_statement_block()?

	mut else_body := []ast.Stmt{}
	if ep.match(lexer.KeywordToken.else_) {
		else_body = ep.parse_statement_block()?
	}

	ep.consume(lexer.KeywordToken.end_, 'Expected end after if expression')?

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
	ep.consume(lexer.KeywordToken.do_, 'Expected do after case value')?

	mut cases := []ast.MatchCase{}
	for !ep.check(lexer.KeywordToken.end_) {
		pattern := ep.parse_pattern()?

		mut guard := ast.Expr(ast.LiteralExpr{ value: ast.BooleanLiteral{ value: true } })
		if ep.match(lexer.KeywordToken.when) {
			guard = ep.parse_expression()?
		}

		ep.consume(lexer.OperatorToken.arrow, 'Expected -> after pattern')?

		body := ep.parse_statement_block()?

		cases << ast.MatchCase{
			pattern:  pattern
			guard:    guard
			body:     body
			position: ep.get_current_position()
		}
	}

	ep.consume(lexer.KeywordToken.end_, 'Expected end after case expression')?

	return ast.CaseExpr{
		value:   value
		cases:   cases
		position: ep.get_current_position()
	}
}

// parse_with_expression parses with expressions
fn (mut ep ExpressionParser) parse_with_expression() ?ast.Expr {
	ep.advance() // consume 'with'

	mut bindings := []ast.WithBinding{}
	for !ep.check(lexer.KeywordToken.do_) {
		pattern := ep.parse_pattern()?
		ep.consume(lexer.OperatorToken.pattern_match, 'Expected <- in with binding')?
		value := ep.parse_expression()?

		bindings << ast.WithBinding{
			pattern:  pattern
			value:    value
			position: ep.get_current_position()
		}

		if !ep.match(lexer.PunctuationToken.comma) {
			break
		}
	}

	ep.consume(lexer.KeywordToken.do_, 'Expected do after with bindings')?

	body := ep.parse_statement_block()?

	mut else_body := []ast.Stmt{}
	if ep.match(lexer.KeywordToken.else_) {
		else_body = ep.parse_statement_block()?
	}

	ep.consume(lexer.KeywordToken.end_, 'Expected end after with expression')?

	return ast.WithExpr{
		bindings: bindings
		body:     body
		else_body: else_body
		position:  ep.get_current_position()
	}
}

// parse_for_expression parses for expressions (list comprehensions)
fn (mut ep ExpressionParser) parse_for_expression() ?ast.Expr {
	ep.advance() // consume 'for'

	pattern := ep.parse_pattern()?
	ep.consume(lexer.KeywordToken.in, 'Expected in after pattern')?

	collection := ep.parse_expression()?

	mut guard := ast.Expr(ast.LiteralExpr{ value: ast.BooleanLiteral{ value: true } })
	if ep.match(lexer.KeywordToken.when) {
		guard = ep.parse_expression()?
	}

	ep.consume(lexer.KeywordToken.do_, 'Expected do after for expression')?

	body := ep.parse_statement_block()?

	ep.consume(lexer.KeywordToken.end_, 'Expected end after for expression')?

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
	ep.consume(lexer.KeywordToken.do_, 'Expected do after receive')?

	mut cases := []ast.ReceiveCase{}
	for !ep.check(lexer.KeywordToken.after) && !ep.check(lexer.KeywordToken.end_) {
		pattern := ep.parse_pattern()?

		mut guard := ast.Expr(ast.LiteralExpr{ value: ast.BooleanLiteral{ value: true } })
		if ep.match(lexer.KeywordToken.when) {
			guard = ep.parse_expression()?
		}

		ep.consume(lexer.OperatorToken.arrow, 'Expected -> after pattern')?

		body := ep.parse_statement_block()?

		cases << ast.ReceiveCase{
			pattern:  pattern
			guard:    guard
			body:     body
			position: ep.get_current_position()
		}
	}

	mut timeout := ast.Expr(ast.LiteralExpr{ value: ast.IntegerLiteral{ value: 0 } })
	if ep.match(lexer.KeywordToken.after) {
		timeout = ep.parse_expression()?
		ep.consume(lexer.KeywordToken.do_, 'Expected do after timeout')?
		// timeout_body := ep.parse_statement_block()?
		// For now, we'll use the timeout expression directly
		// In a full implementation, we'd need to handle the timeout body
	}

	ep.consume(lexer.KeywordToken.end_, 'Expected end after receive expression')?

	return ast.ReceiveExpr{
		cases:   cases
		timeout: timeout
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

	for !ep.check(lexer.KeywordToken.end_) && !ep.is_at_end() {
		stmt := ep.parse_statement()?
		statements << stmt
	}

	return statements
}

// parse_statement parses a single statement
fn (mut ep ExpressionParser) parse_statement() ?ast.Stmt {
	expr := ep.parse_expression()?
	return ast.ExprStmt{ expr: expr }
}

// parse_pattern parses patterns for pattern matching
fn (mut ep ExpressionParser) parse_pattern() ?ast.Pattern {
	return match ep.current {
		lexer.IdentToken { ep.parse_variable_pattern() }
		lexer.UpperIdentToken { ep.parse_variable_pattern() }
		lexer.StringToken { ep.parse_literal_pattern() }
		lexer.IntToken { ep.parse_literal_pattern() }
		lexer.FloatToken { ep.parse_literal_pattern() }
		lexer.BoolToken { ep.parse_literal_pattern() }
		lexer.AtomToken { ep.parse_atom_pattern() }
		lexer.NilToken { ep.parse_nil_pattern() }
		lexer.PunctuationToken {
			punc_token := ep.current as lexer.PunctuationToken
			match punc_token {
				.lparen { ep.parse_parenthesized_pattern() }
				.lbrace { ep.parse_tuple_pattern() }
				.lbracket { ep.parse_list_pattern() }
				else {
					ep.add_error('Unexpected token in pattern: ${ep.current.str()}', 'Expected pattern')
					none
				}
			}
		}
		lexer.OperatorToken { ep.parse_map_pattern() }
		else {
			ep.add_error('Unexpected token in pattern: ${ep.current.str()}', 'Expected pattern')
			none
		}
	}
}

// parse_variable_pattern parses variable patterns
fn (mut ep ExpressionParser) parse_variable_pattern() ?ast.Pattern {
	token := ep.current
	ep.advance()

	return ast.VarPattern{
		name: token.get_value()
	}
}

// parse_literal_pattern parses literal patterns
fn (mut ep ExpressionParser) parse_literal_pattern() ?ast.Pattern {
	token := ep.current
	ep.advance()

	value := match token {
		lexer.StringToken { ast.Literal(ast.StringLiteral{ value: token.value }) }
		lexer.IntToken { ast.Literal(ast.IntegerLiteral{ value: token.value }) }
		lexer.FloatToken { ast.Literal(ast.FloatLiteral{ value: token.value }) }
		lexer.BoolToken { ast.Literal(ast.BooleanLiteral{ value: token.value }) }
		else { ast.Literal(ast.NilLiteral{}) }
	}

	return ast.LiteralPattern{ value: value }
}

// parse_atom_pattern parses atom patterns
fn (mut ep ExpressionParser) parse_atom_pattern() ?ast.Pattern {
	token := ep.current
	ep.advance()

	return ast.AtomPattern{
		value: token.get_value()
	}
}

// parse_nil_pattern parses nil patterns
fn (mut ep ExpressionParser) parse_nil_pattern() ?ast.Pattern {
	ep.advance()
	return ast.WildcardPattern{}
}

// parse_parenthesized_pattern parses parenthesized patterns
fn (mut ep ExpressionParser) parse_parenthesized_pattern() ?ast.Pattern {
	ep.advance() // consume '('
	pattern := ep.parse_pattern()?
	ep.consume(lexer.PunctuationToken.rparen, 'Expected closing parenthesis')?

	return pattern
}

// parse_tuple_pattern parses tuple patterns
fn (mut ep ExpressionParser) parse_tuple_pattern() ?ast.Pattern {
	ep.advance() // consume '{'

	mut elements := []ast.Pattern{}
	if !ep.check(lexer.PunctuationToken.rbrace) {
		for {
			elements << ep.parse_pattern()?

			if !ep.match(lexer.PunctuationToken.comma) {
				break
			}
		}
	}

	ep.consume(lexer.PunctuationToken.rbrace, 'Expected closing brace')?

	return ast.TuplePattern{
		elements: elements
	}
}

// parse_list_pattern parses list patterns
fn (mut ep ExpressionParser) parse_list_pattern() ?ast.Pattern {
	ep.advance() // consume '['

	if ep.check(lexer.PunctuationToken.rbracket) {
		ep.advance() // consume ']'
		return ast.ListEmptyPattern{}
	}

	mut elements := []ast.Pattern{}
	for {
		elements << ep.parse_pattern()?

		if !ep.match(lexer.PunctuationToken.comma) {
			break
		}
	}

	ep.consume(lexer.PunctuationToken.rbracket, 'Expected closing bracket')?

	return ast.ListLiteralPattern{
		elements: elements
	}
}

// parse_map_pattern parses map patterns
fn (mut ep ExpressionParser) parse_map_pattern() ?ast.Pattern {
	ep.advance() // consume '%'
	ep.consume(lexer.PunctuationToken.lbrace, 'Expected opening brace after %')?

	mut entries := []ast.MapPatternEntry{}
	if !ep.check(lexer.PunctuationToken.rbrace) {
		for {
			key := ep.parse_pattern()?

			// Check for colon (atom key) or arrow (general key)
			if ep.match(lexer.PunctuationToken.colon) {
				value := ep.parse_pattern()?
				entries << ast.MapPatternEntry{
					key:      key
					value:    value
					position: ep.get_current_position()
				}
			} else if ep.match(lexer.OperatorToken.arrow) {
				value := ep.parse_pattern()?
				entries << ast.MapPatternEntry{
					key:      key
					value:    value
					position: ep.get_current_position()
				}
			} else {
				ep.add_error('Expected : or => in map pattern', 'Got ${ep.current.str()}')
				return none
			}

			if !ep.match(lexer.PunctuationToken.comma) {
				break
			}
		}
	}

	ep.consume(lexer.PunctuationToken.rbrace, 'Expected closing brace')?

	return ast.MapPattern{
		entries: entries
	}
}

// Helper methods for error handling and position tracking
fn (mut ep ExpressionParser) add_error(message string, context string) {
	pos := ep.get_current_position()
	comp_error := errors.new_compilation_error(
		errors.ErrorKind(errors.SyntaxError{
			message:  message
			expected: context
			found:    ep.current.str()
		}),
		pos,
		'${message}: ${context}'
	)
	ep.errors << comp_error
}

fn (ep ExpressionParser) get_current_position() ast.Position {
	return ast.Position{
		line:   1
		column: ep.position + 1
	}
}
