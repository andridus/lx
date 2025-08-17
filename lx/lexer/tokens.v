module lexer

import ast

pub enum TokenType {
	// Keywords
	def
	defp
	do
	end
	true_
	false_
	nil_
	record
	case
	when
	fn
	type

	// Task 11: Control Flow Keywords
	if_
	else_
	with
	match
	rescue

	// Task 11: Concurrency Keywords
	spawn
	receive
	supervisor
	worker

	// Task 11: Module System Keywords
	deps
	application
	import

	// Task 11: Advanced Keywords
	describe
	test
	assert
	for_
	in

	// Function tokens
	arrow // ->

	// Literals
	integer
	float
	string
	charlist
	atom
	identifier

	// Special tokens
	module_token // __MODULE__

	// Delimiters
	lparen       // (
	rparen       // )
	semicolon    // ;
	comma        // ,
	bind         // =
	lbracket     // [
	rbracket     // ]
	pipe         // |
	pipe_forward // |>
	lbrace       // {
	rbrace       // }
	percent      // %
	colon        // :
	dot          // .
	double_colon // ::

	// Task 11: New operators and delimiters
	left_arrow  // <-
	exclamation // !
	double_pipe // ||
	hash        // #
	at_sign     // @
	langle      // <
	rangle      // >
	double_lt   // <<
	double_gt   // >>
	triple_lt   // <<<
	triple_gt   // >>>
	slash       // /

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

// pub fn (t Token) str() string {
// 	return '${t.type_}("${t.value}") at ${t.position}'
// }

pub fn (tt TokenType) str() string {
	return match tt {
		.def { 'def' }
		.defp { 'defp' }
		.do { 'do' }
		.end { 'end' }
		.true_ { 'true' }
		.false_ { 'false' }
		.nil_ { 'nil' }
		.record { 'record' }
		.case { 'case' }
		.when { 'when' }
		.fn { 'fn' }
		.type { 'type' }
		.if_ { 'if' }
		.else_ { 'else' }
		.with { 'with' }
		.match { 'match' }
		.rescue { 'rescue' }
		.spawn { 'spawn' }
		.receive { 'receive' }
		.supervisor { 'supervisor' }
		.worker { 'worker' }
		.deps { 'deps' }
		.application { 'application' }
		.import { 'import' }
		.describe { 'describe' }
		.test { 'test' }
		.assert { 'assert' }
		.for_ { 'for' }
		.in { 'in' }
		.integer { 'integer' }
		.float { 'float' }
		.string { 'string' }
		.charlist { 'charlist' }
		.atom { 'atom' }
		.identifier { 'identifier' }
		.module_token { '__MODULE__' }
		.lparen { '(' }
		.rparen { ')' }
		.bind { 'bind' }
		.semicolon { ';' }
		.comma { ',' }
		.lbracket { '[' }
		.rbracket { ']' }
		.pipe { '|' }
		.pipe_forward { '|>' }
		.lbrace { '{' }
		.rbrace { '}' }
		.percent { '%' }
		.colon { ':' }
		.dot { '.' }
		.double_colon { '::' }
		.arrow { '->' }
		.left_arrow { '<-' }
		.exclamation { '!' }
		.double_pipe { '||' }
		.hash { '#' }
		.at_sign { '@' }
		.langle { '<' }
		.rangle { '>' }
		.double_lt { '<<' }
		.double_gt { '>>' }
		.triple_lt { '<<<' }
		.triple_gt { '>>>' }
		.slash { '/' }
		.newline { 'newline' }
		.eof { 'eof' }
		.error { 'error' }
	}
}
