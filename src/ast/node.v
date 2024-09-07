module ast

pub struct Node {
pub:
	left  NodeLeft
	kind  NodeKind
	nodes []Node
pub mut:
	idx  int
	meta Meta
}

pub fn (n Node) is_comment() bool {
	if n.kind is Comment {
		return true
	} else {
		return false
	}
}

pub fn new_node(kind NodeKind) Node {
	return Node{
		left: ''
		kind: kind
	}
}

pub fn new_atom_node(value string) Node {
	atom := Atom.new(value)
	return Node{
		left: NodeLeft(atom)
		kind: atom
	}
}

pub fn new_module_node(values []string) Node {
	mut nodes := []Node{}
	for value in values {
		nodes << new_node_2(value, String{})
	}
	return Node{
		left:  NodeLeft('__aliases__')
		kind:  Aliases.new(values)
		nodes: nodes
	}
}

pub fn new_node_2(value string, kind NodeKind) Node {
	value0 := str_to_node_left(value, kind)
	return Node{
		left: value0
		kind: kind
	}
}

pub fn new_node_3(value string, kind NodeKind, nodes []Node) Node {
	value0 := str_to_node_left(value, kind)
	return Node{
		left:  value0
		kind:  kind
		nodes: nodes
	}
}

pub fn new_node_4(value string, kind NodeKind, nodes []Node, meta Meta) Node {
	value0 := str_to_node_left(value, kind)
	return Node{
		left:  value0
		kind:  kind
		nodes: nodes
		meta:  meta
	}
}
