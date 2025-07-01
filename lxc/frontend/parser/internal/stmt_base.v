module internal

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
		sp.current = lexer.EOFToken{
			position: lexer.new_token_position(1, 1, '')
		}
	}
}

// safe_advance advances the parser and ensures current token is synchronized
fn (mut sp StatementParser) safe_advance() {
	sp.position++
	sp.sync_current_token()
}

// parse_statement parses a single statement
fn (mut sp StatementParser) parse_statement() ?ast.Stmt {
	// Skip directive tokens and go to the next token
	if sp.current is lexer.DirectiveToken {
		sp.advance()
		return sp.parse_statement()
	}

	return match sp.current {
		lexer.KeywordToken {
			keyword_token := sp.current as lexer.KeywordToken
			match keyword_token.value {
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
	// Use the current token's position if available
	pos := sp.current.get_position()
	return ast.new_position(pos.line, pos.column, pos.filename)
}
