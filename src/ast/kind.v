module ast

pub type NodeKind = Atom | Float | Function | Integer | String | Nil

pub struct Atom {
pub:
	value string
}

pub struct Nil {}

pub struct String {}

pub struct Integer {}

pub struct Float {}

pub struct Function {
pub:
	precedence int
	position   FunctionPosition = .prefix
}

pub enum FunctionPosition {
	prefix
	infix
	postfix
}
