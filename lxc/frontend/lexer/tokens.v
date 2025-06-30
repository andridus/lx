module lexer

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
	| EOFToken
	| ErrorToken

// IdentToken represents an identifier token
pub struct IdentToken {
pub:
	value string
}

// UpperIdentToken represents an uppercase identifier token (for records)
pub struct UpperIdentToken {
pub:
	value string
}

// StringToken represents a string literal token
pub struct StringToken {
pub:
	value string
}

// IntToken represents an integer literal token
pub struct IntToken {
pub:
	value int
}

// FloatToken represents a float literal token
pub struct FloatToken {
pub:
	value f64
}

// BoolToken represents a boolean literal token
pub struct BoolToken {
pub:
	value bool
}

// AtomToken represents an atom literal token
pub struct AtomToken {
pub:
	value string
}

// NilToken represents a nil literal token
pub struct NilToken {
}

// KeywordToken represents a keyword token
pub enum KeywordToken {
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

// OperatorToken represents an operator token
pub enum OperatorToken {
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

// PunctuationToken represents a punctuation token
pub enum PunctuationToken {
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

// NewlineToken represents a newline token
pub struct NewlineToken {
}

// EOFToken represents end of file token
pub struct EOFToken {
}

// ErrorToken represents an error token
pub struct ErrorToken {
pub:
	message string
}

// str returns a string representation of Token
pub fn (t Token) str() string {
	return match t {
		IdentToken { 'Ident(${t.value})' }
		UpperIdentToken { 'UpperIdent(${t.value})' }
		StringToken { 'String("${t.value}")' }
		IntToken { 'Int(${t.value})' }
		FloatToken { 'Float(${t.value})' }
		BoolToken { 'Bool(${t.value})' }
		AtomToken { 'Atom(${t.value})' }
		NilToken { 'Nil' }
		KeywordToken { 'Keyword(${t.str()})' }
		OperatorToken { 'Operator(${t.str()})' }
		PunctuationToken { 'Punctuation(${t.str()})' }
		NewlineToken { 'Newline' }
		EOFToken { 'EOF' }
		ErrorToken { 'Error(${t.message})' }
	}
}

// str returns a string representation of KeywordToken
pub fn (k KeywordToken) str() string {
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

// str returns a string representation of OperatorToken
pub fn (o OperatorToken) str() string {
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

// str returns a string representation of PunctuationToken
pub fn (p PunctuationToken) str() string {
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
