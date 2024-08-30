module ast

pub type NodeKind = Atom | Float | Function | Integer | String | Charlist | List | Boolean | Nil

pub struct Atom {
pub:
	value string
}

pub struct Nil {}

pub struct String {}

pub struct Charlist {}

pub struct Integer {}

pub struct Float {}

pub struct Boolean {
	value bool
}

pub fn Boolean.new(value bool) Boolean {
	return Boolean{
		value: value
	}
}

pub struct List {
	kind NodeKind
}

pub fn List.new(kind NodeKind) List {
	return List{
		kind: kind
	}
}

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
