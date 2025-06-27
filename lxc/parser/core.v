module parser

import ast
import lexer
import errors

pub struct Parser {
pub mut:
	tokens   []lexer.Token
	position int
	current  lexer.Token
	errors   []errors.CompilationError
}

pub fn new_parser(tokens []lexer.Token) Parser {
	return Parser{
		tokens: tokens
		position: 0
		current: if tokens.len > 0 { tokens[0] } else { lexer.EOFToken{} }
		errors: []
	}
}

pub fn (mut p Parser) advance() {
	p.position++
	if p.position < p.tokens.len {
		p.current = p.tokens[p.position]
	} else {
		p.current = lexer.EOFToken{}
	}
}

pub fn (p Parser) peek() lexer.Token {
	if p.position + 1 < p.tokens.len {
		return p.tokens[p.position + 1]
	}
	return lexer.EOFToken{}
}

pub fn (p Parser) peek_n(n int) lexer.Token {
	if p.position + n < p.tokens.len {
		return p.tokens[p.position + n]
	}
	return lexer.EOFToken{}
}

pub fn (p Parser) is_at_end() bool {
	return p.position >= p.tokens.len
}

pub fn (p Parser) check(token_type lexer.Token) bool {
	if p.is_at_end() {
		return false
	}
	return p.current == token_type
}

pub fn (mut p Parser) match(token_type lexer.Token) bool {
	if p.check(token_type) {
		p.advance()
		return true
	}
	return false
}

pub fn (mut p Parser) consume(token_type lexer.Token, message string) ?lexer.Token {
	if p.check(token_type) {
		p.advance()
		return p.tokens[p.position - 1]
	}

	pos := ast.Position{
		line: 1
		column: p.position + 1
	}

	comp_error := errors.new_compilation_error(
		errors.ErrorKind(errors.SyntaxError{
			message: message
			expected: token_type.str()
			found: p.current.str()
		}),
		pos,
		'Expected ${token_type.str()}, got ${p.current.str()}'
	)

	p.errors << comp_error
	return none
}

pub fn (p Parser) has_errors() bool {
	return p.errors.len > 0
}

pub fn (p Parser) get_errors() []errors.CompilationError {
	return p.errors
}