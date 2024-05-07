module ast

pub struct Node {
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

pub fn new_node_4(value string, kind NodeKind, nodes []Node, meta Meta) Node {
	value0 := str_to_node_left(value, kind)
	return Node{
		left: value0
		kind: kind
		nodes: nodes
		meta: meta
	}
}

fn str_to_node_left(value string, kind NodeKind) NodeLeft {
	return match kind {
		Integer { NodeLeft(value.int()) }
		Float { NodeLeft(value.f64()) }
		else { NodeLeft(value) }
	}
}
