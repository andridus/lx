module ast

pub struct Node {
pub:
	id       int
	kind     NodeKind
	value    string
	children []Node
	position Position
}

pub enum NodeKind {
	// Literals
	integer
	float
	string
	boolean
	atom
	nil

	// Function structure
	function
	function_body

	// Module structure
	module // Raiz do módulo
}

pub struct Position {
pub:
	line   int
	column int
	file   string
}

pub struct Type {
pub:
	name   string
	params []Type
}

// Helper methods for Node
pub fn (n Node) str() string {
	return match n.kind {
		.integer { 'Int(${n.value})' }
		.float { 'Float(${n.value})' }
		.string { 'String("${n.value}")' }
		.boolean { 'Bool(${n.value})' }
		.atom { 'Atom(${n.value})' }
		.nil { 'Nil' }
		.function { 'Function(${n.value})' }
		.function_body { 'FunctionBody' }
		.module { 'Module' }
	}
}

pub fn (p Position) str() string {
	return '${p.file}:${p.line}:${p.column}'
}

pub fn (t Type) str() string {
	if t.params.len == 0 {
		return t.name
	}
	params_str := t.params.map(it.str()).join(', ')
	return '${t.name}(${params_str})'
}
