module lexer

import ast

pub enum TokenType {
	// Keywords
	def
	do
	end
	true_
	false_
	nil_

	// Literals
	integer
	float
	string
	atom
	identifier

	// Delimiters
	lparen    // (
	rparen    // )
	equals    // =
	semicolon // ;

	// Special
	newline
	eof
	error
}

pub struct Token {
pub:
	type_    TokenType
	value    string
	position ast.Position
}

pub fn new_token(type_ TokenType, value string, pos ast.Position) Token {
	return Token{
		type_:    type_
		value:    value
		position: pos
	}
}

pub fn (t Token) str() string {
	return '${t.type_}("${t.value}") at ${t.position}'
}

pub fn (tt TokenType) str() string {
	return match tt {
		.def { 'def' }
		.do { 'do' }
		.end { 'end' }
		.true_ { 'true' }
		.false_ { 'false' }
		.nil_ { 'nil' }
		.integer { 'integer' }
		.float { 'float' }
		.string { 'string' }
		.atom { 'atom' }
		.identifier { 'identifier' }
		.lparen { '(' }
		.rparen { ')' }
		.equals { '=' }
		.semicolon { ';' }
		.newline { 'newline' }
		.eof { 'eof' }
		.error { 'error' }
	}
}
