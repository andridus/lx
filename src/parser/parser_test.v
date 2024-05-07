module parser

import ast

fn test_parse_integer_stmt() {
	source := '1'
	expected := ast.Node{
		left: ast.NodeLeft(1)
		kind: ast.NodeKind(ast.Integer{})
		nodes: []
	}
	assert expected == parse_stmt(source)!
}

fn test_parse_float_stmt() {
	source := '1.0'
	expected := ast.Node{
		left: ast.NodeLeft(1.0)
		kind: ast.NodeKind(ast.Float{})
		nodes: []
	}
	assert expected == parse_stmt(source)!
}

// fn test_parse_float_stmt_1() {
// 	source := '1.0 1'
//   expected := '** (SyntaxError) [source:1:4] syntax error before "1.0"
// 	  |
// 	1 | 1 1.0
// 	  |   ^
// 	'
// 	mut received := ''
// 	parse_stmt(source) or { received = err.msg() }
// 	assert expected == received
// }