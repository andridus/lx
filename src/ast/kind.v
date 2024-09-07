module ast

pub type NodeKind = Atom
	| Float
	| Function
	| Integer
	| String
	| Charlist
	| List
	| Tuple
	| Boolean
	| Module
	| Aliases
	| Mixed
	| Comment
	| Nil

pub struct Atom {
	value string
	idx   int
}

pub fn Atom.new(value string) Atom {
	return Atom{
		value: value
		idx:   1 // it will use atom table to ensure value
	}
}

pub struct Module {
	name string
}

pub fn Module.new(name string) Module {
	return Module{
		name: name
	}
}

pub struct Aliases {
	values []string
	idx    int
}

pub fn Aliases.new(values []string) Aliases {
	return Aliases{
		values: values
		idx:    1
	}
}

pub struct Mixed {
mut:
	kinds []NodeKind
}

pub fn Mixed.new(kinds []NodeKind) Mixed {
	return Mixed{kinds}
}

pub fn (m Mixed) kinds() []NodeKind {
	return m.kinds
}

pub fn (mut m Mixed) put_if_required(kind NodeKind) {
	if kind !in m.kinds {
		m.kinds << kind
	}
}

pub struct Nil {}

pub struct String {}

pub struct Comment {}

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

pub struct Tuple {
	kinds []NodeKind
}

pub fn Tuple.new(kinds []NodeKind) Tuple {
	return Tuple{
		kinds: kinds
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
