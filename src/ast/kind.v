module ast

pub type NodeKind = Nil
	| Integer
	| Float

pub struct Nil {}
pub struct Integer {}
pub struct Float {}