module ast

// LXType represents the type system of LX language
pub enum LXType {
	integer
	float
	string
	boolean
	atom
	nil
	list
	tuple
	map
	record
	function
	pid
	reference
	port
	binary
	bitstring
	any
	unknown
}

// str returns a string representation of LXType
pub fn (t LXType) str() string {
	return match t {
		.integer { 'integer' }
		.float { 'float' }
		.string { 'string' }
		.boolean { 'boolean' }
		.atom { 'atom' }
		.nil { 'nil' }
		.list { 'list' }
		.tuple { 'tuple' }
		.map { 'map' }
		.record { 'record' }
		.function { 'function' }
		.pid { 'pid' }
		.reference { 'reference' }
		.port { 'port' }
		.binary { 'binary' }
		.bitstring { 'bitstring' }
		.any { 'any' }
		.unknown { 'unknown' }
	}
}

// Literal represents literal values in LX using sum types
pub type Literal = StringLiteral
	| IntegerLiteral
	| FloatLiteral
	| BooleanLiteral
	| AtomLiteral
	| NilLiteral

// str returns a string representation of Literal
pub fn (l Literal) str() string {
	return match l {
		StringLiteral { 'LString("${l.value}")' }
		IntegerLiteral { 'LInt(${l.value})' }
		FloatLiteral { 'LFloat(${l.value})' }
		BooleanLiteral { 'LBool(${l.value})' }
		AtomLiteral { 'LAtom(${l.value})' }
		NilLiteral { 'LNil' }
	}
}

// get_type returns the LXType for a given Literal
pub fn (l Literal) get_type() LXType {
	return match l {
		StringLiteral { .string }
		IntegerLiteral { .integer }
		FloatLiteral { .float }
		BooleanLiteral { .boolean }
		AtomLiteral { .atom }
		NilLiteral { .nil }
	}
}

// is_numeric checks if the literal is numeric
pub fn (l Literal) is_numeric() bool {
	return match l {
		IntegerLiteral, FloatLiteral { true }
		else { false }
	}
}

// is_atomic checks if the literal is atomic (atom, boolean, nil)
pub fn (l Literal) is_atomic() bool {
	return match l {
		AtomLiteral, BooleanLiteral, NilLiteral { true }
		else { false }
	}
}
