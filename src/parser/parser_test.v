module parser

import ast
fn test_parse_integer_stmt() {
	source := '1'
	expected := ast.Node{
		left: ast.NodeLeft(1)
		kind: ast.NodeKind(ast.Integer{})
		nodes: []
	}
	assert expected == parser.parse_stmt(source)!
}

fn test_parse_float_stmt() {
	source := '1.0'
	expected := ast.Node{
		left: ast.NodeLeft(1.0)
		kind: ast.NodeKind(ast.Float{})
		nodes: []
	}
	assert expected == parser.parse_stmt(source)!
}