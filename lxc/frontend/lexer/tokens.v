module lexer

// TokenPosition represents the position of a token in source code
pub struct TokenPosition {
pub:
	line     int    // 1-indexed line number
	column   int    // 1-indexed column number
	filename string // Source file name
}

// new_token_position creates a new TokenPosition
pub fn new_token_position(line int, column int, filename string) TokenPosition {
	return TokenPosition{
		line:     line
		column:   column
		filename: filename
	}
}

// str returns a string representation of TokenPosition
pub fn (p TokenPosition) str() string {
	return '${p.filename}:${p.line}:${p.column}'
}

// Token represents a lexical token in LX language using sum types
pub type Token = IdentToken
	| UpperIdentToken
	| StringToken
	| IntToken
	| FloatToken
	| BoolToken
	| AtomToken
	| NilToken
	| KeywordToken
	| OperatorToken
	| PunctuationToken
	| NewlineToken
	| DirectiveToken
	| EOFToken
	| ErrorToken

// IdentToken represents an identifier token
pub struct IdentToken {
pub:
	value    string
	position TokenPosition
}

// UpperIdentToken represents an uppercase identifier token (for records)
pub struct UpperIdentToken {
pub:
	value    string
	position TokenPosition
}

// StringToken represents a string literal token
pub struct StringToken {
pub:
	value    string
	position TokenPosition
}

// IntToken represents an integer literal token
pub struct IntToken {
pub:
	value    int
	position TokenPosition
}

// FloatToken represents a float literal token
pub struct FloatToken {
pub:
	value    f64
	position TokenPosition
}

// BoolToken represents a boolean literal token
pub struct BoolToken {
pub:
	value    bool
	position TokenPosition
}

// AtomToken represents an atom literal token
pub struct AtomToken {
pub:
	value    string
	position TokenPosition
}

// NilToken represents a nil literal token
pub struct NilToken {
pub:
	position TokenPosition
}

// KeywordValue represents the value of a keyword token
pub enum KeywordValue {
	def
	defp
	case_
	if_
	else_
	do_
	end_
	with
	for_
	when
	receive
	after
	true_
	false_
	nil_
	unsafe
	record
	worker
	supervisor
	strategy
	children
	one_for_one
	one_for_all
	rest_for_one
	spec
	requires
	ensures
	matches
	describe
	test_
	assert
	module
	import
	in
}

// KeywordToken represents a keyword token
pub struct KeywordToken {
pub:
	value    KeywordValue
	position TokenPosition
}

// OperatorValue represents the value of an operator token
pub enum OperatorValue {
	assign        // =
	pattern_match // <-
	arrow         // ->
	send          // !
	type_cons     // ::
	dot           // .
	concat        // ++
	record_update // |
	plus          // +
	minus         // -
	mult          // *
	div           // /
	eq            // ==
	neq           // !=
	lt            // <
	gt            // >
	leq           // <=
	geq           // >=
	and_
	or_
	not_
	andalso
	orelse
}

// OperatorToken represents an operator token
pub struct OperatorToken {
pub:
	value    OperatorValue
	position TokenPosition
}

// PunctuationValue represents the value of a punctuation token
pub enum PunctuationValue {
	lparen    // (
	rparen    // )
	lbrace    // {
	rbrace    // }
	lbracket  // [
	rbracket  // ]
	comma     // ,
	semicolon // ;
	colon     // :
}

// PunctuationToken represents a punctuation token
pub struct PunctuationToken {
pub:
	value    PunctuationValue
	position TokenPosition
}

// NewlineToken represents a newline token
pub struct NewlineToken {
pub:
	position TokenPosition
}

// DirectiveToken represents a directive token (e.g., @reflection, @inline)
pub struct DirectiveToken {
pub:
	directive string
	position  TokenPosition
}

// EOFToken represents end of file token
pub struct EOFToken {
pub:
	position TokenPosition
}

// ErrorToken represents an error token
pub struct ErrorToken {
pub:
	message  string
	position TokenPosition
}

// str returns a string representation of Token
pub fn (t Token) str() string {
	return match t {
		IdentToken { '${t.value}' }
		UpperIdentToken { '${t.value}' }
		StringToken { '"${t.value}"' }
		IntToken { '${t.value}' }
		FloatToken { '${t.value}' }
		BoolToken { '${t.value}' }
		AtomToken { '${t.value}' }
		NilToken { 'Nil' }
		KeywordToken { '${t.value.str()}' }
		OperatorToken { '${t.value.str()}' }
		PunctuationToken { '${t.value.str()}' }
		NewlineToken { 'Newline' }
		DirectiveToken { 'Directive(${t.directive})' }
		EOFToken { 'EOF' }
		ErrorToken { '${t.message}' }
	}
}

// str returns a string representation of KeywordValue
pub fn (k KeywordValue) str() string {
	return match k {
		.def { 'def' }
		.defp { 'defp' }
		.case_ { 'case' }
		.if_ { 'if' }
		.else_ { 'else' }
		.do_ { 'do' }
		.end_ { 'end' }
		.with { 'with' }
		.for_ { 'for' }
		.when { 'when' }
		.receive { 'receive' }
		.after { 'after' }
		.true_ { 'true' }
		.false_ { 'false' }
		.nil_ { 'nil' }
		.unsafe { 'unsafe' }
		.record { 'record' }
		.worker { 'worker' }
		.supervisor { 'supervisor' }
		.strategy { 'strategy' }
		.children { 'children' }
		.one_for_one { 'one_for_one' }
		.one_for_all { 'one_for_all' }
		.rest_for_one { 'rest_for_one' }
		.spec { 'spec' }
		.requires { 'requires' }
		.ensures { 'ensures' }
		.matches { 'matches' }
		.describe { 'describe' }
		.test_ { 'test' }
		.assert { 'assert' }
		.module { 'module' }
		.import { 'import' }
		.in { 'in' }
	}
}

// str returns a string representation of OperatorValue
pub fn (o OperatorValue) str() string {
	return match o {
		.assign { '=' }
		.pattern_match { '<-' }
		.arrow { '->' }
		.send { '!' }
		.type_cons { '::' }
		.dot { '.' }
		.concat { '++' }
		.record_update { '|' }
		.plus { '+' }
		.minus { '-' }
		.mult { '*' }
		.div { '/' }
		.eq { '==' }
		.neq { '!=' }
		.lt { '<' }
		.gt { '>' }
		.leq { '<=' }
		.geq { '>=' }
		.and_ { 'and' }
		.or_ { 'or' }
		.not_ { 'not' }
		.andalso { 'andalso' }
		.orelse { 'orelse' }
	}
}

// str returns a string representation of PunctuationValue
pub fn (p PunctuationValue) str() string {
	return match p {
		.lparen { '(' }
		.rparen { ')' }
		.lbrace { '{' }
		.rbrace { '}' }
		.lbracket { '[' }
		.rbracket { ']' }
		.comma { ',' }
		.semicolon { ';' }
		.colon { ':' }
	}
}

// is_keyword checks if a token is a keyword
pub fn (t Token) is_keyword() bool {
	return match t {
		KeywordToken { true }
		else { false }
	}
}

// is_operator checks if a token is an operator
pub fn (t Token) is_operator() bool {
	return match t {
		OperatorToken { true }
		else { false }
	}
}

// is_literal checks if a token is a literal
pub fn (t Token) is_literal() bool {
	return match t {
		StringToken, IntToken, FloatToken, BoolToken, AtomToken, NilToken { true }
		else { false }
	}
}

// is_identifier checks if a token is an identifier
pub fn (t Token) is_identifier() bool {
	return match t {
		IdentToken, UpperIdentToken { true }
		else { false }
	}
}

// get_value returns the string value of a token (for identifiers, strings, atoms)
pub fn (t Token) get_value() string {
	return match t {
		IdentToken { t.value }
		UpperIdentToken { t.value }
		StringToken { t.value }
		AtomToken { t.value }
		ErrorToken { t.message }
		else { '' }
	}
}

// get_numeric_value returns the numeric value of a token
pub fn (t Token) get_numeric_value() ?f64 {
	return match t {
		IntToken { f64(t.value) }
		FloatToken { t.value }
		else { none }
	}
}

// get_boolean_value returns the boolean value of a token
pub fn (t Token) get_boolean_value() ?bool {
	return match t {
		BoolToken { t.value }
		else { none }
	}
}

// Helper functions for token construction
pub fn keyword(val KeywordValue) KeywordToken {
	return KeywordToken{
		value: val
	}
}

pub fn operator(val OperatorValue) OperatorToken {
	return OperatorToken{
		value: val
	}
}

pub fn punctuation(val PunctuationValue) PunctuationToken {
	return PunctuationToken{
		value: val
	}
}

// get_position returns the position of a token
pub fn (t Token) get_position() TokenPosition {
	return match t {
		IdentToken { t.position }
		UpperIdentToken { t.position }
		StringToken { t.position }
		IntToken { t.position }
		FloatToken { t.position }
		BoolToken { t.position }
		AtomToken { t.position }
		NilToken { t.position }
		KeywordToken { t.position }
		OperatorToken { t.position }
		PunctuationToken { t.position }
		NewlineToken { t.position }
		DirectiveToken { t.position }
		EOFToken { t.position }
		ErrorToken { t.position }
	}
}

// Helper functions for creating tokens with default positions (for testing)
pub fn new_ident_token(value string) IdentToken {
	return IdentToken{
		value:    value
		position: new_token_position(1, 1, 'test')
	}
}

pub fn new_upper_ident_token(value string) UpperIdentToken {
	return UpperIdentToken{
		value:    value
		position: new_token_position(1, 1, 'test')
	}
}

pub fn new_string_token(value string) StringToken {
	return StringToken{
		value:    value
		position: new_token_position(1, 1, 'test')
	}
}

pub fn new_int_token(value int) IntToken {
	return IntToken{
		value:    value
		position: new_token_position(1, 1, 'test')
	}
}

pub fn new_float_token(value f64) FloatToken {
	return FloatToken{
		value:    value
		position: new_token_position(1, 1, 'test')
	}
}

pub fn new_bool_token(value bool) BoolToken {
	return BoolToken{
		value:    value
		position: new_token_position(1, 1, 'test')
	}
}

pub fn new_atom_token(value string) AtomToken {
	return AtomToken{
		value:    value
		position: new_token_position(1, 1, 'test')
	}
}

pub fn new_nil_token() NilToken {
	return NilToken{
		position: new_token_position(1, 1, 'test')
	}
}

pub fn new_keyword_token(value KeywordValue) KeywordToken {
	return KeywordToken{
		value:    value
		position: new_token_position(1, 1, 'test')
	}
}

pub fn new_operator_token(value OperatorValue) OperatorToken {
	return OperatorToken{
		value:    value
		position: new_token_position(1, 1, 'test')
	}
}

pub fn new_punctuation_token(value PunctuationValue) PunctuationToken {
	return PunctuationToken{
		value:    value
		position: new_token_position(1, 1, 'test')
	}
}

pub fn new_newline_token() NewlineToken {
	return NewlineToken{
		position: new_token_position(1, 1, 'test')
	}
}

pub fn new_eof_token() EOFToken {
	return EOFToken{
		position: new_token_position(1, 1, 'test')
	}
}

pub fn new_error_token(message string) ErrorToken {
	return ErrorToken{
		message:  message
		position: new_token_position(1, 1, 'test')
	}
}
