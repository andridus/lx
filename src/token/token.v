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
	_dual_op // +, -
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
	assoc Associative
	precedence int
	kind Kind
}
pub fn (tp TokenPrecedence) get_assoc() Associative {
	return tp.assoc
}
pub fn (tp TokenPrecedence) get_precedence() int {
	return tp.precedence
}

const precedences = [
	TokenPrecedence{.left, 210, ._dual_op},
	TokenPrecedence{.left, 220, ._mult_op}
]
pub fn (token0 &Token) is_valid() bool {
	return token0.kind == .eof
}

pub fn (token0 &Token) is_infix() bool {
	return token0.kind in [._dual_op, ._mult_op]
}

pub fn (token0 &Token) precedence() TokenPrecedence {
	for p in precedences {
		if p.kind == token0.kind { return p}
	}
	panic('FATAL: token precedence not found!')
}

pub fn generate_eof() Token {
	return Token{
		kind: .eof
		value: 'EOF'
	}
}
