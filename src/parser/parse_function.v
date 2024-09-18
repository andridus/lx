module parser

import ast

fn (mut p Parser) parse_function() !ast.Node {
	mut code := []ast.Node{}
	p.expect(._def)!
	aliases := p.expect(._ident)!
	// module_name := aliases.value
	// ex := p.expect_one_of([._do, ._comma])!
	// if ex.kind == ._do {
	// 	p.ignore_next_newline()
	// 	for p.current_token.kind != ._end {
	// 		node := p.expr()!
	// 		code << node

	// 		match node.kind {
	// 			ast.ModuleAttribute {
	// 				attributes[node.nodes[0].left.to_str()] = node.nodes[1]
	// 			}
	// 			else {}
	// 		}
	// 		p.ignore_next_newline()
	// 	}
	// 	p.expect(._end)!
	// } else if ex.kind == ._comma {
	// 	p.expect_keyword('do')!
	// 	code << p.stmt()!
	// }
	// mod := ast.Function.new(module_name, attributes, code)
	return ast.new_node('nil', p.meta(), none)
}
