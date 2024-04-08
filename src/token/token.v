module token

pub struct Token {
	kind  Kind = .eof
	value string
}

pub enum Kind {
	eof
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

pub fn generate_eof() Token {
	return Token{
		kind: .eof
		value: 'EOF'
	}
}
