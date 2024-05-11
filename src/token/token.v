module token

pub struct Token {
pub:
	kind  Kind = .eof
	value string
}

pub fn (t Token) kind() Kind {
	return t.kind
}

pub fn (t Token) value() string {
	return t.value
}

pub enum Kind {
	eof
	newline
	_add_op // +, -
	_mult_op // *, /
	_int // 1
	_flt // 1.5
	_char // 'a'
}

pub enum Associative {
	left
	right
	nonassoc
}

pub struct TokenPrecedence {
	assoc      Associative
	precedence int
}

pub fn (tp TokenPrecedence) get_assoc() Associative {
	return tp.assoc
}

pub fn (tp TokenPrecedence) is_infix() bool {
	return tp.assoc in [.right, .left]
}

pub fn (tp TokenPrecedence) get_precedence() int {
	return tp.precedence
}

const precedences = {
	'_add_op':  TokenPrecedence{.left, 210}
	'_mult_op': TokenPrecedence{.left, 220}
}

pub fn (token0 &Token) is_valid() bool {
	return token0.kind == .eof
}

pub fn (token0 &Token) is_infix() bool {
	prec := token0.precedence() or { return false }
	return prec.is_infix()
}

pub fn (token0 &Token) precedence() !TokenPrecedence {
	return token.precedences[token0.kind.str()] or { error('has no precedence') }
}

pub fn generate_eof() Token {
	return Token{
		kind: .eof
		value: 'EOF'
	}
}
