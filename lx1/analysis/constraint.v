module analysis

import ast

pub struct Constraint {
	left  ast.Type
	right ast.Type
}
