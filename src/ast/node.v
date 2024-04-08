module ast

// node artifact
pub type NodeLeft = Node | f64 | int | string
pub type NodeKind = Nil
	| Integer
	| Float


pub struct Node{
	left NodeLeft
	kind NodeKind
	nodes []Node
pub mut:
	idx int
	meta Meta
}

pub struct Meta {
	line int
	start_pos int
	end_pos int
}

pub struct Nil {}
pub struct Integer {}
pub struct Float {}