module parser

import ast
import lexer
import errors

// StatementParser handles parsing of all statement types in LX
@[heap]
pub struct StatementParser {
	Parser
}

fn (sp StatementParser) current() lexer.Token {
	return sp.tokens[sp.position]
}

pub fn new_statement_parser(tokens []lexer.Token) &StatementParser {
	return &StatementParser{
		Parser: new_parser(tokens)
	}
}

// sync_current_token ensures the current token is always synchronized with the position
fn (mut sp StatementParser) sync_current_token() {
	if sp.position < sp.tokens.len {
		sp.current = sp.tokens[sp.position]
	} else {
		sp.current = lexer.EOFToken{}
	}
}

// safe_advance advances the parser and ensures current token is synchronized
fn (mut sp StatementParser) safe_advance() {
	sp.position++
	sp.sync_current_token()
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
		else {
			sp.parse_expression_statement()
		}
	}
}

// parse_expression_statement parses an expression as a statement
fn (mut sp StatementParser) parse_expression_statement() ?ast.Stmt {
	// Parse the expression using the current parser state
	expr := sp.parse_expression()?

	return ast.ExprStmt{
		expr: expr
	}
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

	// PRIMEIRO: se for 'do', é multi-head
	if sp.check(lexer.KeywordToken.do_) {
		sp.advance() // consume 'do'
		mut clauses := []ast.FunctionClause{}
		for !sp.check(lexer.KeywordToken.end_) && !sp.is_at_end() {
			sp.skip_irrelevant_tokens()
			// Pular até o próximo (
			for sp.position < sp.tokens.len && !(sp.tokens[sp.position] is lexer.PunctuationToken && (sp.tokens[sp.position] as lexer.PunctuationToken) == .lparen) && !sp.check(lexer.KeywordToken.end_) {
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
		if !sp.check(lexer.KeywordToken.end_) {
			sp.add_error('Expected end after function headers', 'Got ${sp.current.str()}')
			return none
		}
		sp.consume(lexer.KeywordToken.end_, 'Expected end after function headers')?
		return ast.FunctionStmt{
			name:     name
			clauses:  clauses
			position: sp.get_current_position()
		}
	}

	// SÓ SE NÃO FOR 'do', verifica se é '('
	if sp.check(lexer.PunctuationToken.lparen) {
		clause := sp.parse_function_clause()?
		return ast.FunctionStmt{
			name:     name
			clauses:  [clause]
			position: sp.get_current_position()
		}
	}

	sp.add_error('Expected do or ( after function name (multi-head or single-head)', 'Got ${sp.current.str()}')
	return none
}

// skip_irrelevant_tokens pula tokens que não são significativos para o parsing
fn (mut sp StatementParser) skip_irrelevant_tokens() {
	// Por enquanto, não há tokens específicos de comentário ou whitespace
	// Se no futuro adicionar tokens de comentário, whitespace, etc, adicione aqui
	// Por enquanto, é um no-op
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
	if sp.check(lexer.KeywordToken.do_) {
		sp.advance() // consume 'do'

		// Parse multiple function headers inside do...end block
		for !sp.check(lexer.KeywordToken.end_) && !sp.is_at_end() {
			clause := sp.parse_function_header()?
			clauses << clause
		}

		if !sp.check(lexer.KeywordToken.end_) {
			sp.add_error('Expected end after function headers', 'Got ${sp.current.str()}')
			return none
		}
		sp.consume(lexer.KeywordToken.end_, 'Expected end after function headers')?
	} else {
		// Parse traditional single clause function
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
	mut guard := ast.Expr(ast.LiteralExpr{
		value: ast.BooleanLiteral{
			value: true
		}
	})
	if sp.match(lexer.KeywordToken.when) {
		guard = sp.parse_simple_expression()?
	}

	// Parse body
	sp.consume(lexer.KeywordToken.do_, 'Expected do after function clause')?
	body := sp.parse_clause_body()?
	sp.consume(lexer.KeywordToken.end_, 'Expected end after function body')?

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

	sp.consume(lexer.PunctuationToken.lparen, 'Expected opening parenthesis for function header')?

	// Parse parameter list
	if !sp.check(lexer.PunctuationToken.rparen) {
		for {
			param := sp.parse_pattern()?
			parameters << param

			if !sp.match(lexer.PunctuationToken.comma) {
				break
			}
		}
	}
	sp.consume(lexer.PunctuationToken.rparen, 'Expected closing parenthesis')?

	// Parse guard (optional)
	mut guard := ast.Expr(ast.LiteralExpr{
		value: ast.BooleanLiteral{
			value: true
		}
	})
	if sp.match(lexer.KeywordToken.when) {
		guard = sp.parse_simple_expression()?
	}

	// Parse arrow
	sp.consume(lexer.OperatorToken.arrow, 'Expected -> after function header')?

	// Parse body - block of statements until next clause or end
	body := sp.parse_clause_body()?

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
	mut expr_parser := new_expression_parser(sp.tokens)
	expr_parser.position = sp.position
	expr_parser.current = sp.current

	pattern := expr_parser.parse_pattern()?

	// Advance the main parser to the position where expression parser ended
	sp.position = expr_parser.position
	sp.sync_current_token()

	return pattern
}

// parse_expression parses expressions
fn (mut sp StatementParser) parse_expression() ?ast.Expr {
	mut expr_parser := new_expression_parser(sp.tokens)
	expr_parser.position = sp.position
	expr_parser.current = sp.current

	expr := expr_parser.parse_expression()?

	// Advance the main parser to the position where expression parser ended
	sp.position = expr_parser.position
	sp.sync_current_token()

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
	comp_error := errors.new_compilation_error(errors.ErrorKind(errors.SyntaxError{
		message:  message
		expected: context
		found:    sp.current.str()
	}), pos, '${message}: ${context}')
	sp.errors << comp_error
}

fn (sp StatementParser) get_current_position() ast.Position {
	return ast.Position{
		line:   1
		column: sp.position + 1
	}
}

// parse_simple_expression parses simple expressions without function calls
// This prevents the parser from consuming tokens from the next clause
fn (mut sp StatementParser) parse_simple_expression() ?ast.Expr {
	mut left := sp.parse_simple_atom()?

	// Check for binary operators
	for sp.current is lexer.OperatorToken {
		op_token := sp.current as lexer.OperatorToken
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
			.mult {
				op = .multiply
				should_continue = true
			}
			.div {
				op = .divide
				should_continue = true
			}
			.gt {
				op = .greater_than
				should_continue = true
			}
			.lt {
				op = .less_than
				should_continue = true
			}
			.geq {
				op = .greater_equal
				should_continue = true
			}
			.leq {
				op = .less_equal
				should_continue = true
			}
			.eq {
				op = .equal
				should_continue = true
			}
			.neq {
				op = .not_equal
				should_continue = true
			}
			else {
				break
			}
		}

		if !should_continue {
			break
		}

		sp.advance() // consume operator
		right := sp.parse_simple_atom()?

		left = ast.BinaryExpr{
			left:     left
			op:       op
			right:    right
			position: sp.get_current_position()
		}
	}

	return left
}

// parse_simple_atom parses atomic expressions (identifiers, literals)
fn (mut sp StatementParser) parse_simple_atom() ?ast.Expr {
	return match sp.current {
		lexer.IdentToken {
			token := sp.current as lexer.IdentToken
			sp.safe_advance()
			mut expr := ast.Expr(ast.VariableExpr{
				name: token.value
			})
			// Allow both record access and function calls
			for {
				match sp.current() {
					lexer.OperatorToken {
						op_token := sp.current() as lexer.OperatorToken
						// Stop if we encounter -> (marks end of clause body)
						if op_token == .arrow {
							break
						}
						if op_token == .dot {
							sp.safe_advance()
							match sp.current() {
								lexer.IdentToken {
									field_token := sp.current() as lexer.IdentToken
									sp.safe_advance()
									expr = ast.Expr(ast.RecordAccessExpr{
										record: expr
										field: field_token.value
										position: sp.get_current_position()
									})
									continue
								}
								else {
									sp.add_error('Expected field name after dot', 'Got ${sp.current().str()}')
									return none
								}
							}
						} else {
							break
						}
					}
					lexer.PunctuationToken {
						punc_token := sp.current() as lexer.PunctuationToken
						if punc_token == .lparen {
							// Verificar se este ( é o início de uma nova cláusula multi-head
							if sp.is_potential_new_clause_start() {
								break
							}
							sp.safe_advance()
							mut arguments := []ast.Expr{}
							if !sp.check(lexer.PunctuationToken.rparen) {
								for {
									arguments << sp.parse_simple_expression()?
									if !sp.match(lexer.PunctuationToken.comma) {
										break
									}
								}
							}
							sp.consume(lexer.PunctuationToken.rparen, 'Expected closing parenthesis')?
							expr = ast.Expr(ast.CallExpr{
								function: expr
								arguments: arguments
								position: sp.get_current_position()
							})
							continue
						} else {
							break
						}
					}
					else {
						break
					}
				}
			}
			expr
		}
		lexer.StringToken {
			token := sp.current as lexer.StringToken
			sp.advance()
			ast.LiteralExpr{
				value: ast.StringLiteral{
					value: token.value
				}
			}
		}
		lexer.IntToken {
			token := sp.current as lexer.IntToken
			sp.advance()
			ast.LiteralExpr{
				value: ast.IntegerLiteral{
					value: token.value
				}
			}
		}
		lexer.BoolToken {
			token := sp.current as lexer.BoolToken
			sp.advance()
			ast.LiteralExpr{
				value: ast.BooleanLiteral{
					value: token.value
				}
			}
		}
		lexer.AtomToken {
			token := sp.current as lexer.AtomToken
			sp.advance()
			ast.LiteralExpr{
				value: ast.AtomLiteral{
					value: token.value
				}
			}
		}
		else {
			sp.add_error('Expected simple expression', 'Got ${sp.current.str()}')
			none
		}
	}
}

// parse_clause_body parses a block of statements until next clause or end
fn (mut sp StatementParser) parse_clause_body() ?[]ast.Stmt {
	mut statements := []ast.Stmt{}

	for !sp.is_at_end() {
		// Detecta início de nova cláusula multi-head: ( ... ) ->
		if sp.is_potential_new_clause_start() {
			break
		}
		if sp.check(lexer.KeywordToken.end_) {
			break
		}
		// Se o próximo token é (, verificar se é início de nova cláusula antes de consumir
		if sp.check(lexer.PunctuationToken.lparen) {
			if sp.is_potential_new_clause_start() {
				break
			}
		}
		// Parse assignment statements
		if sp.current is lexer.IdentToken && sp.peek().str() == 'Operator(=)' {
			stmt := sp.parse_assignment_statement()?
			statements << stmt
		} else {
			expr := sp.parse_clause_expression()?
			statements << ast.ExprStmt{
				expr: expr
			}
		}
	}

	if statements.len == 0 {
		expr := sp.parse_simple_expression()?
		statements << ast.ExprStmt{
			expr: expr
		}
	}

	return statements
}

// Detecta início de nova cláusula multi-head pelo padrão ( ... ) ->
fn (sp StatementParser) is_potential_new_clause_start() bool {
	mut pos := sp.position
	if pos >= sp.tokens.len { return false }
	if !(sp.tokens[pos] is lexer.PunctuationToken) { return false }
	punc := sp.tokens[pos] as lexer.PunctuationToken
	if punc != .lparen { return false }
	// Procurar pelo fechamento do parêntese
	mut paren_count := 1
	pos++
	for pos < sp.tokens.len && paren_count > 0 {
		tok := sp.tokens[pos]
		if tok is lexer.PunctuationToken {
			p := tok as lexer.PunctuationToken
			if p == .lparen { paren_count++ }
			if p == .rparen { paren_count-- }
		}
		pos++
	}
	if paren_count != 0 { return false }
	// Após fechar parêntese, deve vir ->
	if pos < sp.tokens.len && sp.tokens[pos] is lexer.OperatorToken {
		op := sp.tokens[pos] as lexer.OperatorToken
		if op == .arrow {
			return true
		}
	}
	return false
}

// parse_assignment_statement parses assignment statements
fn (mut sp StatementParser) parse_assignment_statement() ?ast.Stmt {
	// Parse variable name
	if !sp.current.is_identifier() {
		sp.add_error('Expected variable name', 'Got ${sp.current.str()}')
		return none
	}
	name := sp.current.get_value()
	sp.advance()

	// Parse assignment operator
	sp.consume(lexer.OperatorToken.assign, 'Expected = in assignment')?

	// Parse value
	value := sp.parse_expression()?

	return ast.ExprStmt{
		expr: ast.AssignExpr{
			name:     name
			value:    value
			position: sp.get_current_position()
		}
	}
}

// parse_clause_expression parses expressions for clause bodies
// This is similar to parse_simple_expression but supports tuples
fn (mut sp StatementParser) parse_clause_expression() ?ast.Expr {
	return match sp.current {
		lexer.PunctuationToken {
			punc_token := sp.current as lexer.PunctuationToken
			match punc_token {
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
	if !sp.check(lexer.PunctuationToken.rbrace) {
		for {
			elements << sp.parse_simple_expression()?

			if !sp.match(lexer.PunctuationToken.comma) {
				break
			}
		}
	}

	sp.consume(lexer.PunctuationToken.rbrace, 'Expected closing brace')?

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
		if keyword_token == .end_ {
			// Rollback to start position
			sp.position = start_pos
			sp.sync_current_token()
			// Return just the simple expression
			return sp.parse_simple_expression()
		}
	}

	return expr
}
