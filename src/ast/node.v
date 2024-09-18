module ast

pub struct Node {
pub:
	left  NodeLeft
	nodes ?[]Node
pub mut:
	idx  int
	meta Meta
}

pub fn (n Node) is_comment() bool {
	if n.meta.kind == .k_comment {
		return true
	} else {
		return false
	}
}

pub fn (n Node) get_meta_literal() Literal {
	return n.meta.literal
}

pub fn (n Node) get_kind() MetaKind {
	return n.meta.kind
}

pub fn (n Node) get_meta_literal_accepts() []Literal {
	return n.meta.literal_accepts
}

pub fn new_node(value NodeLeft, meta Meta, nodes ?[]Node) Node {
	return Node{
		left:  value
		meta:  meta
		nodes: nodes
	}
}
