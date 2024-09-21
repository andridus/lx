module parser

import ast

fn (mut p Parser) parse_module() !ast.Node {
	mut meta := p.meta()
	mut code := []ast.Node{}

	// mut attributes := map[string]ast.Node{}
	p.expect(._defmodule)!
	aliases := p.expect(._aliases)!
	module_name := aliases.value

	ex := p.expect_one_of([._do, ._comma])!
	p.current_module << module_name
	if ex.kind == ._do {
		p.ignore_next_newline()
		for p.current_token.kind != ._end {
			node := p.stmt()!
			code << node

			// match node.meta.kind {
			// 	.k_attribute {
			// 		if nodes := node.nodes {
			// 			attributes[nodes[0].left.to_str()] = nodes[1]
			// 		}
			// 	}
			// 	else {}
			// }
			p.ignore_next_newline()
		}
		p.expect(._end)!
	} else if ex.kind == ._comma {
		p.expect_keyword('do')!
		code << p.stmt()!
	}
	p.current_module.delete_last()

	p.update_meta(mut meta)
	meta.set_kind(.k_module_def)

	return ast.new_node(module_name, meta, code)
}
