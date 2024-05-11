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
		left: value0
		kind: kind
		nodes: nodes
	}
}

pub fn new_node_4(value string, kind NodeKind, nodes []Node, meta Meta) Node {
	value0 := str_to_node_left(value, kind)
	return Node{
		left: value0
		kind: kind
		nodes: nodes
		meta: meta
	}
}
