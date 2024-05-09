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
	op_plus
	op_minus
	op_mult
	op_div
	lit_float
	lit_int
	lit_string
}

pub fn (token0 &Token) is_valid() bool {
	return token0.kind == .eof
}

pub fn (token0 &Token) is_infix() bool {
	return token0.kind in [.op_plus, .op_minus, .op_mult, .op_div]
}

pub fn generate_eof() Token {
	return Token{
		kind: .eof
		value: 'EOF'
	}
}
