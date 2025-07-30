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

	// Variables
	variable_binding // x = value
	variable_ref     // x (usage)

	// Blocks
	block // do ... end or -> ... end (multiple expressions)

	// Function structure
	function
	function_body

	// Module structure
	module

	// Binary operators
	function_caller // +(a, b), *(a, b), >(a, b), etc.
	parentheses     // (expression)
	directive_call  // $print(a), $type(a)
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
	return '{${n.kind}, [id: ${n.id}, pos: ${n.position}], ${n.value}, ${n.children}}'
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

pub fn (n Node) tree_str(indent int) string {
	pad := '  '.repeat(indent)
	mut result := '${pad}{${n.kind}, [], ${n.value}, ['

	if n.children.len > 0 {
		result += '\n'
		for i, child in n.children {
			result += child.tree_str(indent + 1)
			if i < n.children.len - 1 {
				result += ','
			}
			result += '\n'
		}
		result += '${pad}]'
	} else {
		result += ']'
	}

	result += '}'
	return result
}
