module internal

import ast
import lexer
import errors

// ========================================
// PARSING CONTEXT MANAGEMENT
// ========================================

// ParsingContext defines the strict parsing contexts as per grammar specification
pub enum ParsingContext {
	mod        // for program and block_top_level - only module_statements allowed
	expression // for block_expression, function bodies - expressions allowed
}

// ========================================
// CORE PARSER INFRASTRUCTURE
// ========================================

// LXParser is the internal parser that implements the grammar specification
// It maintains strict context separation and follows the grammar rules
@[heap]
pub struct LXParser {
mut:
	tokens      []lexer.Token
	position    int
	current     lexer.Token
	context     ParsingContext
	errors      []errors.CompilationError
	next_ast_id int = 1 // Next available AST ID
}

// new_lx_parser creates a new internal parser instance
pub fn new_lx_parser(tokens []lexer.Token) &LXParser {
	mut parser := &LXParser{
		tokens:   tokens
		position: 0
		context:  .mod // Start in mod context
		errors:   []
	}

	// Initialize current token
	if tokens.len > 0 {
		parser.current = tokens[0]
	} else {
		parser.current = lexer.EOFToken{
			position: lexer.new_token_position(1, 1, '')
		}
	}

	return parser
}

// ========================================
// TOKEN NAVIGATION
// ========================================

// advance moves to the next token
fn (mut p LXParser) advance() {
	if p.position < p.tokens.len - 1 {
		p.position++
		p.current = p.tokens[p.position]
	} else {
		p.current = lexer.EOFToken{
			position: lexer.new_token_position(1, 1, '')
		}
	}
}

// peek returns the next token without advancing
fn (p LXParser) peek() lexer.Token {
	if p.position + 1 < p.tokens.len {
		return p.tokens[p.position + 1]
	}
	return lexer.EOFToken{
		position: lexer.new_token_position(1, 1, '')
	}
}

// is_at_end checks if we've reached the end of tokens
fn (p LXParser) is_at_end() bool {
	return p.current is lexer.EOFToken
}

// ========================================
// TOKEN CHECKING AND MATCHING
// ========================================

// check verifies if current token matches expected type
fn (p LXParser) check(expected lexer.Token) bool {
	match expected {
		lexer.KeywordToken {
			keyword := expected as lexer.KeywordToken
			if p.current is lexer.KeywordToken {
				current_keyword := p.current as lexer.KeywordToken
				return current_keyword.value == keyword.value
			}
			return false
		}
		lexer.PunctuationToken {
			punct := expected as lexer.PunctuationToken
			if p.current is lexer.PunctuationToken {
				current_punct := p.current as lexer.PunctuationToken
				return current_punct.value == punct.value
			}
			return false
		}
		lexer.OperatorToken {
			op := expected as lexer.OperatorToken
			if p.current is lexer.OperatorToken {
				current_op := p.current as lexer.OperatorToken
				return current_op.value == op.value
			}
			return false
		}
		else {
			return false
		}
	}
}

// match checks and consumes token if it matches
fn (mut p LXParser) match(expected lexer.Token) bool {
	if p.check(expected) {
		p.advance()
		return true
	}
	return false
}

// consume expects and consumes a specific token, adding error if not found
fn (mut p LXParser) consume(expected lexer.Token, error_message string) ?lexer.Token {
	if p.check(expected) {
		token := p.current
		p.advance()
		return token
	}

	p.add_error(error_message, 'Expected ${expected.str()}, got ${p.current.str()}')
	return none
}

// ========================================
// CONTEXT MANAGEMENT
// ========================================

// with_context temporarily changes parsing context
fn (mut p LXParser) with_context[T](new_context ParsingContext, parse_fn fn (mut LXParser) ?T) ?T {
	old_context := p.context
	p.context = new_context
	defer {
		p.context = old_context
	}

	return parse_fn(mut p)
}

// ========================================
// ERROR HANDLING
// ========================================

// add_error adds a parsing error
fn (mut p LXParser) add_error(message string, context string) {
	pos := p.get_current_position()
	comp_error := errors.new_compilation_error(errors.ErrorKind(errors.SyntaxError{
		message:  message
		expected: context
		found:    p.current.str()
	}), pos, '${message}: ${context}')
	p.errors << comp_error
}

// get_errors returns all parsing errors
pub fn (p LXParser) get_errors() []errors.CompilationError {
	return p.errors
}

// get_current_position returns the current position for error reporting
fn (p LXParser) get_current_position() ast.Position {
	pos := p.current.get_position()
	return ast.new_position(pos.line, pos.column, pos.filename)
}

// ========================================
// UTILITY FUNCTIONS
// ========================================

// skip_newlines skips over newline tokens
fn (mut p LXParser) skip_newlines() {
	for p.current is lexer.NewlineToken {
		p.advance()
	}
}

// generate_ast_id generates a new unique AST ID
fn (mut p LXParser) generate_ast_id() int {
	id := p.next_ast_id
	p.next_ast_id++
	return id
}

// Helper functions for creating tokens
fn keyword_token(kw lexer.KeywordValue) lexer.Token {
	return lexer.KeywordToken{
		value:    kw
		position: lexer.new_token_position(0, 0, '')
	}
}

fn punctuation_token(punct lexer.PunctuationValue) lexer.Token {
	return lexer.PunctuationToken{
		value:    punct
		position: lexer.new_token_position(0, 0, '')
	}
}

fn operator_token(op lexer.OperatorValue) lexer.Token {
	return lexer.OperatorToken{
		value:    op
		position: lexer.new_token_position(0, 0, '')
	}
}
