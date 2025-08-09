module analysis

import ast

pub struct Constraint {
pub:
	left      ast.Type
	right     ast.Type
	position  ast.Position
}
