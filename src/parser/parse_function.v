module parser

import ast

fn (mut p Parser) parse_function() !ast.Node {
	mut meta := p.meta()
	mut code := []ast.Node{}
	p.expect(._def)!
	function_name := p.expect(._ident)!
	ex := p.expect_one_of([._do, ._comma])!
	if ex.kind == ._do {
		p.ignore_next_newline()
		for p.current_token.kind != ._end {
			node := p.expr()!
			code << node
			p.ignore_next_newline()
		}
		p.expect(._end)!
	} else if ex.kind == ._comma {
		p.expect_keyword('do')!
		code << p.stmt()!
	}
	if code.len > 0 {
		last := code.last()
		meta.copy_literal_from_node(last)
	}
	meta.set_kind(.k_function_def)
	return ast.new_node(function_name.value(), meta, code)
}
