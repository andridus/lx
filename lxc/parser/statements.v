module parser

import ast
import lexer
import errors

// StatementParser handles parsing of all statement types in LX
@[heap]
pub struct StatementParser {
	Parser
}

pub fn new_statement_parser(tokens []lexer.Token) &StatementParser {
	return &StatementParser{
		Parser: new_parser(tokens)
	}
}

// parse_statement parses a single statement
fn (mut sp StatementParser) parse_statement() ?ast.Stmt {
	return match sp.current {
		lexer.KeywordToken {
			keyword_token := sp.current as lexer.KeywordToken
			match keyword_token {
				.def { sp.parse_function_statement() }
				.defp { sp.parse_private_function_statement() }
				.record { sp.parse_record_definition() }
				.worker { sp.parse_worker_statement() }
				.supervisor { sp.parse_supervisor_statement() }
				.spec { sp.parse_specification_statement() }
				.describe { sp.parse_test_describe_statement() }
				.test_ { sp.parse_test_statement() }
				else { sp.parse_expression_statement() }
			}
		}
		else { sp.parse_expression_statement() }
	}
}

// parse_expression_statement parses an expression as a statement
fn (mut sp StatementParser) parse_expression_statement() ?ast.Stmt {
	mut expr_parser := new_expression_parser(sp.tokens[sp.position..])
	expr := expr_parser.parse_expression()?

	// Advance the main parser to the position where expression parser ended
	sp.position += expr_parser.position
	if sp.position < sp.tokens.len {
		sp.current = sp.tokens[sp.position]
	} else {
		sp.current = lexer.EOFToken{}
	}

	return ast.ExprStmt{ expr: expr }
}

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

	mut clauses := []ast.FunctionClause{}

	// Parse function clauses
	for sp.check(lexer.PunctuationToken.lparen) || sp.check(lexer.KeywordToken.do_) {
		clause := sp.parse_function_clause()?
		clauses << clause
	}

	return ast.FunctionStmt{
		name:     name
		clauses:  clauses
		position: sp.get_current_position()
	}
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

	// Parse function clauses
	for sp.check(lexer.PunctuationToken.lparen) || sp.check(lexer.KeywordToken.do_) {
		clause := sp.parse_function_clause()?
		clauses << clause
	}

	// For now, we'll use the same structure as public functions
	// In a full implementation, we'd mark it as private
	return ast.FunctionStmt{
		name:     name
		clauses:  clauses
		position: sp.get_current_position()
	}
}

// parse_function_clause parses a single function clause
fn (mut sp StatementParser) parse_function_clause() ?ast.FunctionClause {
	// Parse parameters
	mut parameters := []ast.Pattern{}

	if sp.check(lexer.PunctuationToken.lparen) {
		sp.advance() // consume '('
		for !sp.check(lexer.PunctuationToken.rparen) {
			param := sp.parse_pattern()?
			parameters << param

			if !sp.match(lexer.PunctuationToken.comma) {
				break
			}
		}
		sp.consume(lexer.PunctuationToken.rparen, 'Expected closing parenthesis')?
	}

	// Parse guard (optional)
	mut guard := ast.Expr(ast.LiteralExpr{ value: ast.BooleanLiteral{ value: true } })
	if sp.match(lexer.KeywordToken.when) {
		guard = sp.parse_expression()?
	}

	// Parse body
	sp.consume(lexer.KeywordToken.do_, 'Expected do after function clause')?
	body := sp.parse_statement_block()?
	sp.consume(lexer.KeywordToken.end_, 'Expected end after function body')?

	return ast.FunctionClause{
		parameters: parameters
		guard:      guard
		body:       body
		position:   sp.get_current_position()
	}
}

// parse_record_definition parses record definitions
fn (mut sp StatementParser) parse_record_definition() ?ast.Stmt {
	sp.advance() // consume 'record'

	name := sp.current.get_value()
	if !sp.current.is_identifier() {
		sp.add_error('Expected record name', 'Got ${sp.current.str()}')
		return none
	}
	sp.advance()

	sp.consume(lexer.PunctuationToken.lbrace, 'Expected opening brace')?

	mut fields := []ast.RecordFieldDef{}
	if !sp.check(lexer.PunctuationToken.rbrace) {
		for {
			field_name := sp.current.get_value()
			if !sp.current.is_identifier() {
				sp.add_error('Expected field name', 'Got ${sp.current.str()}')
				return none
			}
			sp.advance()

			sp.consume(lexer.OperatorToken.type_cons, 'Expected :: after field name')?

			field_type := sp.parse_type()?

			fields << ast.RecordFieldDef{
				name:       field_name
				field_type: field_type
				position:   sp.get_current_position()
			}

			if !sp.match(lexer.PunctuationToken.comma) {
				break
			}
		}
	}

	sp.consume(lexer.PunctuationToken.rbrace, 'Expected closing brace')?

	return ast.RecordDefStmt{
		name:     name
		fields:   fields
		position: sp.get_current_position()
	}
}

// parse_worker_statement parses worker definitions
fn (mut sp StatementParser) parse_worker_statement() ?ast.Stmt {
	sp.advance() // consume 'worker'

	name := sp.current.get_value()
	if !sp.current.is_identifier() {
		sp.add_error('Expected worker name', 'Got ${sp.current.str()}')
		return none
	}
	sp.advance()

	sp.consume(lexer.KeywordToken.do_, 'Expected do after worker name')?

	mut statements := []ast.Stmt{}
	for !sp.check(lexer.KeywordToken.end_) && !sp.is_at_end() {
		stmt := sp.parse_statement()?
		statements << stmt
	}

	sp.consume(lexer.KeywordToken.end_, 'Expected end after worker body')?

	// For now, we'll return a module statement
	// In a full implementation, we'd have a specific WorkerStmt type
	return ast.ModuleStmt{
		name:       name
		exports:    []
		imports:    []
		statements: statements
		position:   sp.get_current_position()
	}
}

// parse_supervisor_statement parses supervisor definitions
fn (mut sp StatementParser) parse_supervisor_statement() ?ast.Stmt {
	sp.advance() // consume 'supervisor'

	name := sp.current.get_value()
	if !sp.current.is_identifier() {
		sp.add_error('Expected supervisor name', 'Got ${sp.current.str()}')
		return none
	}
	sp.advance()

	sp.consume(lexer.PunctuationToken.lbrace, 'Expected opening brace')?

	// Parse strategy
	sp.consume(lexer.KeywordToken.strategy, 'Expected strategy keyword')?
	// strategy := sp.current.get_value()
	if !sp.current.is_identifier() {
		sp.add_error('Expected strategy name', 'Got ${sp.current.str()}')
		return none
	}
	sp.advance()

	// Parse children
	sp.consume(lexer.KeywordToken.children, 'Expected children keyword')?
	sp.consume(lexer.PunctuationToken.lbracket, 'Expected opening bracket')?

	mut children := []string{}
	if !sp.check(lexer.PunctuationToken.rbracket) {
		for {
			child := sp.current.get_value()
			if !sp.current.is_identifier() {
				sp.add_error('Expected child name', 'Got ${sp.current.str()}')
				return none
			}
			sp.advance()
			children << child

			if !sp.match(lexer.PunctuationToken.comma) {
				break
			}
		}
	}

	sp.consume(lexer.PunctuationToken.rbracket, 'Expected closing bracket')?
	sp.consume(lexer.PunctuationToken.rbrace, 'Expected closing brace')?

	// For now, we'll return a module statement
	// In a full implementation, we'd have a specific SupervisorStmt type
	return ast.ModuleStmt{
		name:       name
		exports:    []
		imports:    []
		statements: []
		position:   sp.get_current_position()
	}
}

// parse_specification_statement parses specification statements
fn (mut sp StatementParser) parse_specification_statement() ?ast.Stmt {
	sp.advance() // consume 'spec'

	name := sp.current.get_value()
	if !sp.current.is_identifier() {
		sp.add_error('Expected specification name', 'Got ${sp.current.str()}')
		return none
	}
	sp.advance()

	sp.consume(lexer.PunctuationToken.lbrace, 'Expected opening brace')?

	mut statements := []ast.Stmt{}
	for !sp.check(lexer.PunctuationToken.rbrace) && !sp.is_at_end() {
		stmt := sp.parse_statement()?
		statements << stmt
	}

	sp.consume(lexer.PunctuationToken.rbrace, 'Expected closing brace')?

	// For now, we'll return a module statement
	// In a full implementation, we'd have a specific SpecStmt type
	return ast.ModuleStmt{
		name:       name
		exports:    []
		imports:    []
		statements: statements
		position:   sp.get_current_position()
	}
}

// parse_test_describe_statement parses test describe blocks
fn (mut sp StatementParser) parse_test_describe_statement() ?ast.Stmt {
	sp.advance() // consume 'describe'

	description := sp.current.get_value()
	if sp.current is lexer.StringToken {
		sp.advance()
	} else {
		sp.add_error('Expected string description', 'Got ${sp.current.str()}')
		return none
	}

	sp.consume(lexer.PunctuationToken.lbrace, 'Expected opening brace')?

	mut statements := []ast.Stmt{}
	for !sp.check(lexer.PunctuationToken.rbrace) && !sp.is_at_end() {
		stmt := sp.parse_statement()?
		statements << stmt
	}

	sp.consume(lexer.PunctuationToken.rbrace, 'Expected closing brace')?

	// For now, we'll return a module statement
	// In a full implementation, we'd have a specific TestDescribeStmt type
	return ast.ModuleStmt{
		name:       description
		exports:    []
		imports:    []
		statements: statements
		position:   sp.get_current_position()
	}
}

// parse_test_statement parses test statements
fn (mut sp StatementParser) parse_test_statement() ?ast.Stmt {
	sp.advance() // consume 'test'

	description := sp.current.get_value()
	if sp.current is lexer.StringToken {
		sp.advance()
	} else {
		sp.add_error('Expected string description', 'Got ${sp.current.str()}')
		return none
	}

	sp.consume(lexer.PunctuationToken.lbrace, 'Expected opening brace')?

	mut statements := []ast.Stmt{}
	for !sp.check(lexer.PunctuationToken.rbrace) && !sp.is_at_end() {
		stmt := sp.parse_statement()?
		statements << stmt
	}

	sp.consume(lexer.PunctuationToken.rbrace, 'Expected closing brace')?

	// For now, we'll return a module statement
	// In a full implementation, we'd have a specific TestStmt type
	return ast.ModuleStmt{
		name:       description
		exports:    []
		imports:    []
		statements: statements
		position:   sp.get_current_position()
	}
}

// parse_pattern parses patterns for pattern matching
fn (mut sp StatementParser) parse_pattern() ?ast.Pattern {
	mut expr_parser := new_expression_parser(sp.tokens[sp.position..])
	expr_parser.position = 0
	expr_parser.current = sp.current

	pattern := expr_parser.parse_pattern()?

	// Advance the main parser to the position where expression parser ended
	sp.position += expr_parser.position
	if sp.position < sp.tokens.len {
		sp.current = sp.tokens[sp.position]
	} else {
		sp.current = lexer.EOFToken{}
	}

	return pattern
}

// parse_expression parses expressions
fn (mut sp StatementParser) parse_expression() ?ast.Expr {
	mut expr_parser := new_expression_parser(sp.tokens[sp.position..])
	expr_parser.position = 0
	expr_parser.current = sp.current

	expr := expr_parser.parse_expression()?

	// Advance the main parser to the position where expression parser ended
	sp.position += expr_parser.position
	if sp.position < sp.tokens.len {
		sp.current = sp.tokens[sp.position]
	} else {
		sp.current = lexer.EOFToken{}
	}

	return expr
}

// parse_type parses type expressions
fn (mut sp StatementParser) parse_type() ?ast.LXType {
	return match sp.current {
		lexer.IdentToken {
			type_name := sp.current.value
			sp.advance()
			match type_name {
				'integer' { ast.LXType.integer }
				'float' { ast.LXType.float }
				'string' { ast.LXType.string }
				'boolean' { ast.LXType.boolean }
				'atom' { ast.LXType.atom }
				'nil' { ast.LXType.nil }
				'list' { ast.LXType.list }
				'tuple' { ast.LXType.tuple }
				'map' { ast.LXType.map }
				'record' { ast.LXType.record }
				'function' { ast.LXType.function }
				'pid' { ast.LXType.pid }
				'reference' { ast.LXType.reference }
				'port' { ast.LXType.port }
				'binary' { ast.LXType.binary }
				'bitstring' { ast.LXType.bitstring }
				'any' { ast.LXType.any }
				else { ast.LXType.unknown }
			}
		}
		else {
			sp.add_error('Expected type name', 'Got ${sp.current.str()}')
			ast.LXType.unknown
		}
	}
}

// parse_statement_block parses a block of statements
fn (mut sp StatementParser) parse_statement_block() ?[]ast.Stmt {
	mut statements := []ast.Stmt{}

	for {
		if sp.current is lexer.KeywordToken {
			keyword_token := sp.current as lexer.KeywordToken
			if keyword_token == .end_ {
				break
			}
		}

		stmt := sp.parse_statement()?
		statements << stmt
	}

	return statements
}

// Helper methods for error handling and position tracking
fn (mut sp StatementParser) add_error(message string, context string) {
	pos := sp.get_current_position()
	comp_error := errors.new_compilation_error(
		errors.ErrorKind(errors.SyntaxError{
			message:  message
			expected: context
			found:    sp.current.str()
		}),
		pos,
		'${message}: ${context}'
	)
	sp.errors << comp_error
}

fn (sp StatementParser) get_current_position() ast.Position {
	return ast.Position{
		line:   1
		column: sp.position + 1
	}
}
