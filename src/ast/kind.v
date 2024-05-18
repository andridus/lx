module ast

pub type NodeKind = Atom | Float | Function | Integer | Nil

pub struct Atom {
pub:
	value string
}

pub struct Nil {}

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
