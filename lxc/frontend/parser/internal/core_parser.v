module internal

import ast
import lexer
import errors

@[heap]
pub struct Parser {
pub mut:
	tokens   []lexer.Token
	position int
	current  lexer.Token
	errors   []errors.CompilationError
}

pub fn new_parser(tokens []lexer.Token) &Parser {
	return &Parser{
		tokens:   tokens
		position: 0
		current:  if tokens.len > 0 {
			tokens[0]
		} else {
			lexer.EOFToken{
				position: lexer.new_token_position(1, 1, '')
			}
		}
		errors:   []
	}
}

pub fn (mut p Parser) advance() {
	p.position++
	if p.position < p.tokens.len {
		p.current = p.tokens[p.position]
	} else {
		p.current = lexer.EOFToken{
			position: lexer.new_token_position(1, 1, '')
		}
	}
}

pub fn (p Parser) peek() lexer.Token {
	if p.position + 1 < p.tokens.len {
		return p.tokens[p.position + 1]
	}
	return lexer.EOFToken{
		position: lexer.new_token_position(1, 1, '')
	}
}

pub fn (p Parser) peek_n(n int) lexer.Token {
	if p.position + n < p.tokens.len {
		return p.tokens[p.position + n]
	}
	return lexer.EOFToken{
		position: lexer.new_token_position(1, 1, '')
	}
}

pub fn (p Parser) is_at_end() bool {
	return p.position >= p.tokens.len
}

pub fn (p Parser) check(token_type lexer.Token) bool {
	if p.is_at_end() {
		return false
	}
	return p.current.str() == token_type.str()
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

	// Use the current token's position for error reporting
	pos := p.current.get_position()
	ast_pos := ast.new_position(pos.line, pos.column, pos.filename)

	comp_error := errors.new_compilation_error(errors.ErrorKind(errors.SyntaxError{
		message:  message
		expected: token_type.str()
		found:    p.current.str()
	}), ast_pos, 'Expected ${token_type.str()}, got ${p.current.str()}')

	p.errors << comp_error
	return none
}

pub fn (p Parser) has_errors() bool {
	return p.errors.len > 0
}

pub fn (p Parser) get_errors() []errors.CompilationError {
	return p.errors
}

pub fn (mut p Parser) sync_current_token() {
	if p.position < p.tokens.len {
		p.current = p.tokens[p.position]
	} else {
		p.current = lexer.EOFToken{
			position: lexer.new_token_position(1, 1, '')
		}
	}
}
