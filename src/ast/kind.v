module ast

pub type NodeKind = Atom | Float | Function | Integer | Nil

pub struct Atom {
	value string
}

pub struct Nil {}

pub struct Integer {}

pub struct Float {}

pub struct Function {}
