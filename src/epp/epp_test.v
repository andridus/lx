module epp

import parser

fn test_preprocessor_ast_to_erlast() {
	source0 := '1'
	expected1 := new_list([
		new_tuple([
			new_atom('integer'),
			new_number(1),
			new_number(1),
		]),
	])
	nodes := parser.parse_stmt(source0)!
	p := process_ast(nodes)
	assert expected1 == p.erlast
}

fn test_preprocessor_precendece0_ast_to_erlast() {
	source0 := '1 + 2'
	expected1 := new_list([
		new_tuple([
			new_atom('op'),
			new_number(1),
			new_atom('+'),
			new_tuple([
				new_atom('integer'),
				new_number(1),
				new_number(1),
			]),
			new_tuple([
				new_atom('integer'),
				new_number(1),
				new_number(2),
			]),
		]),
	])
	nodes := parser.parse_stmt(source0)!
	p := process_ast(nodes)
	assert expected1 == p.erlast
}

fn test_preprocessor_precendece_ast_to_erlast() {
	source0 := '1 + 2 - 3'
	expected1 := new_list([
		new_tuple([
			new_atom('op'),
			new_number(1),
			new_atom('-'),
			new_tuple([
				new_atom('op'),
				new_number(1),
				new_atom('+'),
				new_tuple([
					new_atom('integer'),
					new_number(1),
					new_number(1),
				]),
				new_tuple([
					new_atom('integer'),
					new_number(1),
					new_number(2),
				]),
			]),
			new_tuple([
				new_atom('integer'),
				new_number(1),
				new_number(3),
			]),
		]),
	])
	nodes := parser.parse_stmt(source0)!
	p := process_ast(nodes)
	assert expected1 == p.erlast
}
